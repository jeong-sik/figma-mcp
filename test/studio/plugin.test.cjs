const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScLttAAAAABJRU5ErkJggg==', 'base64');
function studio({ loadFont = async () => {}, mode = 'default' } = {}) {
  const nodes = new Map(), messages = [], awaiting = new Map();
  let sequence = 0, request = 0, exportHook = () => {};
  function documentNode(node) {
    const keys = ['id', 'type', 'name', 'x', 'y', 'width', 'height', 'characters', 'fontName', 'fontSize',
      'fills', 'layoutMode', 'paddingTop', 'itemSpacing'];
    const result = Object.fromEntries(keys.filter(key => node[key] !== undefined).map(key => [key, node[key]]));
    if (node.children) result.children = node.children.map(documentNode);
    return result;
  }
  function node(type, name) {
    const n = { id: `node:${++sequence}`, type, name, x: 0, y: 0, width: 100, height: 100,
      locked: false, removed: false, fills: [],
      resize(w, h) { this.width = w; this.height = h; },
      async exportAsync({ format }) {
        await exportHook(n, format);
        return format === 'PNG' ? new Uint8Array(png) : { document: documentNode(n), schemaVersion: 0 };
      } };
    if (['PAGE', 'FRAME'].includes(type)) Object.assign(n, { children: [],
      appendChild(child) {
        if (child.parent) child.parent.children.splice(child.parent.children.indexOf(child), 1);
        this.children.push(child); child.parent = this;
      } });
    if (type === 'TEXT') Object.assign(n, { characters: '', fontName: { family: 'Inter', style: 'Regular' },
      hasMissingFont: false, getRangeAllFontNames() { return [this.fontName]; } });
    nodes.set(n.id, n);
    return n;
  }
  const page = node('PAGE', 'Page');
  page.selection = [];
  page.loadAsync = async () => {};
  const frame = node('FRAME', 'Target'); page.appendChild(frame); page.selection = [frame];
  function create(type) { const n = node(type, type); page.appendChild(n); return n; }
  const figma = { root: { name: 'Fixture document' }, currentPage: page, editorType: 'figma', mode,
    showUI() {}, on() {}, notify() {}, commitUndo() {},
    createFrame: () => create('FRAME'), createText: () => create('TEXT'),
    getNodeByIdAsync: async id => nodes.get(id), loadFontAsync: loadFont,
    ui: { postMessage(message) { messages.push(message); if (awaiting.has(message.id)) awaiting.get(message.id)(message); } } };
  vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../../plugin/studio/code.js'), 'utf8'), { figma, __html__: '' });
  const session = messages[0].session_id;
  function send(kind, args = {}, id = `op:${++request}`) {
    return new Promise(resolve => {
      awaiting.set(id, resolve);
      figma.ui.onmessage({ type: 'command', session_id: session,
        command: { id, session_id: session, kind, root_id: frame.id, ...args } });
    });
  }
  return { nodes, page, frame, figma, session, send, hook(fn) { exportHook = fn; } };
}

test('native capture returns REST structure and the exported PNG from the actual entrypoint', async () => {
  const s = studio();
  const capture = await s.send('capture');
  assert.equal(capture.ok, true);
  assert.equal(JSON.parse(capture.payload.tree_json).document.id, s.frame.id);
  assert.deepEqual(Buffer.from(capture.payload.preview.data_base64, 'base64'), png);
  assert.equal(capture.payload.coverage.stable, true);
  assert.ok(capture.payload.coverage.missing.includes('font_files'));
});

test('create native layout/text and repeat the same operation without duplicate nodes', async () => {
  const s = studio();
  const before = (await s.send('capture')).payload;
  const args = { expected_tree_json: before.tree_json, ops: [
    { op: 'create_frame', params: { ref: '@card', parent_id: s.frame.id, name: 'Card', width: 320, height: 240 } },
    { op: 'set_auto_layout', params: { node_id: '@card', direction: 'VERTICAL', spacing: 12, padding: 24 } },
    { op: 'create_text', params: { ref: '@title', parent_id: '@card', text: '안녕하세요',
      font: { family: 'Inter', style: 'Regular' }, font_size: 20 } }
  ] };
  const first = await s.send('apply', args, 'edit:1');
  assert.equal(first.ok, true);
  assert.equal(s.frame.children[0].children[0].characters, '안녕하세요');
  assert.equal(s.frame.children[0].paddingTop, 24);
  const again = await s.send('apply', args, 'edit:1');
  assert.equal(again.payload.tree_json, first.payload.tree_json);
  assert.equal(s.frame.children.length, 1);
  const conflict = await s.send('apply', { ...args, ops: [] }, 'edit:1');
  assert.equal(conflict.ok, false);
  assert.match(conflict.payload.error, /different input/);
});

test('a bad operation late in the plan or a missing font leaves the document untouched', async () => {
  for (const badFont of [false, true]) {
    const s = studio({ loadFont: async () => { if (badFont) throw new Error('Missing fixture font'); } });
    const before = (await s.send('capture')).payload.tree_json;
    const result = await s.send('apply', { expected_tree_json: before, ops: [
      { op: 'create_frame', params: { ref: '@card', parent_id: s.frame.id, name: 'Card', width: 30, height: 40 } },
      badFont ? { op: 'create_text', params: { ref: '@title', parent_id: '@card', text: 'Title', font: { family: 'Missing', style: 'Regular' }, font_size: 20 } } :
        { op: 'unrecognized', params: {} }
    ] });
    assert.equal(result.ok, false);
    assert.equal(result.payload.status, 'rejected');
    assert.equal((await s.send('capture')).payload.tree_json, before);
  }
});

test('stale baselines and nodes from another root are refused', async () => {
  const s = studio();
  const before = (await s.send('capture')).payload.tree_json;
  s.frame.name = 'Changed by collaborator';
  const stale = await s.send('apply', { expected_tree_json: before, ops: [
    { op: 'rename', params: { node_id: s.frame.id, name: 'Overwrite' } }
  ] });
  assert.equal(stale.ok, false);
  assert.equal(s.frame.name, 'Changed by collaborator');
  const outside = s.figma.createFrame();
  const result = await s.send('apply', { expected_tree_json: (await s.send('capture')).payload.tree_json, ops: [
    { op: 'rename', params: { node_id: outside.id, name: 'Wrong root' } }
  ] });
  assert.equal(result.ok, false);
});

test('capture records concurrent changes and missing preview without inventing completeness', async () => {
  const s = studio();
  s.hook((node, format) => { if (format === 'PNG') { node.name = 'Concurrent'; throw new Error('No render'); } });
  const result = await s.send('capture');
  assert.equal(result.ok, true);
  assert.equal(result.payload.preview, null);
  assert.equal(result.payload.coverage.stable, false);
  assert.ok(result.payload.coverage.missing.some(x => x.startsWith('preview:')));
});

test('readback failure after creation keeps changed IDs and never reports rejection', async () => {
  const s = studio();
  const before = (await s.send('capture')).payload.tree_json;
  s.hook((node, format) => { if (format === 'JSON_REST_V1' && s.frame.children.length) throw new Error('Lost readback'); });
  const result = await s.send('apply', { expected_tree_json: before, ops: [
    { op: 'create_frame', params: { ref: '@card', parent_id: s.frame.id, name: 'Card', width: 30, height: 40 } }
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.status, 'outcome_unknown');
  assert.equal(result.payload.completed[0].node_id, s.frame.children[0].id);
});

test('Dev Mode cannot execute canvas edits', async () => {
  const s = studio({ mode: 'inspect' });
  const before = (await s.send('capture')).payload.tree_json;
  const result = await s.send('apply', { expected_tree_json: before, ops: [
    { op: 'rename', params: { node_id: s.frame.id, name: 'No edit' } }
  ] });
  assert.equal(result.ok, false);
  assert.equal(s.frame.name, 'Target');
});
