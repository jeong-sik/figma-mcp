const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

// Load the shipped Plugin entrypoint, not a mock bridge responder. The fake
// document implements the native operations exercised here; it is not Figma.
function plugin({ loadFont = async () => {} } = {}) {
  const nodes = new Map();
  let sequence = 0;
  let commandSequence = 0;
  const messages = [];
  const page = { id: 'page:1', name: 'Page', type: 'PAGE', children: [], selection: [],
    appendChild(node) {
      if (node.parent) node.parent.children.splice(node.parent.children.indexOf(node), 1);
      this.children.push(node);
      node.parent = this;
    } };
  nodes.set(page.id, page);
  function create(type) {
    const node = { id: `node:${++sequence}`, name: type, type,
      x: 0, y: 0, width: 100, height: 100, visible: true, locked: false,
      fills: [], strokes: [], effects: [], opacity: 1, cornerRadius: 0,
      resize(width, height) { this.width = width; this.height = height; } };
    if (type === 'FRAME') Object.assign(node, { children: [], appendChild: page.appendChild });
    if (type === 'TEXT') Object.assign(node, { characters: '', fontName: { family: 'Inter', style: 'Regular' } });
    nodes.set(node.id, node);
    page.appendChild(node);
    return node;
  }
  const figma = { showUI() {}, mixed: Symbol('mixed'),
    ui: { postMessage(message) { messages.push(message); } },
    currentPage: page, root: { children: [page] },
    on() {}, off() {}, loadAllPagesAsync: async () => {},
    getNodeByIdAsync: async id => nodes.get(id),
    createFrame: () => create('FRAME'), createText: () => create('TEXT'),
    loadFontAsync: loadFont };
  const context = vm.createContext({ figma, __html__: '', console });
  vm.runInContext(fs.readFileSync(path.join(__dirname, '../../plugin/code.js'), 'utf8'), context);
  const nonce = messages.find(message => message.type === 'init').nonce;
  async function send(name, payload = {}) {
    const id = `command:${++commandSequence}`;
    await figma.ui.onmessage({ type: 'command', nonce, command: { id, name, payload } });
    const response = messages.find(message => message.type === 'command_result' && message.command_id === id);
    assert.ok(response, 'Plugin sends a result through its real UI boundary');
    return { ok: response.ok, payload: JSON.parse(response.payload_json) };
  }
  return { send, nodes, page };
}

test('server apply_ops envelope creates a frame, text and auto layout; get_node reads them back', async () => {
  const { send, page } = plugin();
  const result = await send('apply_ops', { ops: [
    { op: 'create_frame', params: { name: 'Sign in', width: 360, height: 640 } },
    { op: 'create_text', params: { text: 'Welcome', parent_id: 'node:1' } },
    { op: 'set_auto_layout', params: { node_id: 'node:1', layout_mode: 'VERTICAL', padding: 24 } },
  ] });
  assert.equal(result.ok, true);
  assert.equal(result.payload.status, 'applied');
  assert.equal(result.payload.completed.length, 3);
  assert.equal(page.children.length, 1);
  assert.equal(page.children[0].layoutMode, 'VERTICAL');
  assert.equal(page.children[0].paddingLeft, 24);
  const readback = await send('get_node', { node_id: 'node:1' });
  assert.equal(readback.ok, true);
  assert.equal(readback.payload.name, 'Sign in');
  assert.equal(readback.payload.width, 360);
  assert.equal(readback.payload.children[0].characters, 'Welcome');
});

test('unknown operation anywhere rejects the complete envelope before the first edit', async () => {
  const { send, nodes } = plugin();
  const result = await send('apply_ops', { ops: [
    { op: 'create_frame', params: {} }, { op: 'invented_action', params: {} },
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.status, 'rejected');
  assert.equal(result.payload.failed_index, 1);
  assert.equal(nodes.size, 1);
});

test('malformed envelopes, inherited handlers and recursive apply_ops are rejected without edits', async () => {
  const { send, nodes } = plugin();
  for (const ops of [null, [], {}, [{ op: 'create_frame' }],
    [{ op: 'create_frame', params: {}, unexpected: true }],
    [{ op: 'create_frame', params: [] }],
    [{ op: 'apply_ops', params: { ops: [] } }],
    [{ op: 'execute_dsl', params: { operations: [{ action: 'invented' }] } }],
    [{ op: 'constructor', params: {} }], [{ op: '__proto__', params: {} }]]) {
    const result = await send('apply_ops', { ops });
    assert.equal(result.ok, false);
    assert.equal(result.payload.status, 'rejected');
  }
  assert.equal(nodes.size, 1);
  assert.equal((await send('constructor')).ok, false);
});

test('failure after native text creation preserves completed results and does not run later edits', async () => {
  const { send, nodes } = plugin({ loadFont: async () => { throw new Error('font unavailable'); } });
  const result = await send('apply_ops', { ops: [
    { op: 'create_frame', params: { name: 'Kept' } },
    { op: 'create_text', params: { text: 'Unwritten' } },
    { op: 'create_frame', params: { name: 'Never' } },
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.status, 'outcome_unknown');
  assert.equal(result.payload.failed_index, 1);
  assert.equal(result.payload.completed[0].payload.node_id, 'node:1');
  assert.equal(nodes.get('node:1').name, 'Kept');
  assert.equal(nodes.has('node:2'), true, 'failed handler already created a native text node');
  assert.equal(nodes.has('node:3'), false);
  assert.match(result.payload.failure.error, /font unavailable/);
  assert.equal((await send('create_frame', { name: 'After failure' })).ok, true);
});

test('native handler rejection does not become batch success', async () => {
  const { send } = plugin();
  const result = await send('apply_ops', { ops: [
    { op: 'set_text', params: { node_id: 'missing', text: 'No node' } },
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.status, 'outcome_unknown');
  assert.equal(result.payload.failure.error, 'Node not found');
});

test('native payload.error remains a failure and stops later edits', async () => {
  const { send, nodes } = plugin();
  await send('create_frame', {});
  const result = await send('apply_ops', { ops: [
    { op: 'set_parent', params: { node_id: 'node:1', parent_id: 'missing' } },
    { op: 'create_frame', params: { name: 'Must not exist' } },
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.status, 'outcome_unknown');
  assert.equal(result.payload.failure.error, 'Invalid parent');
  assert.equal(nodes.size, 2);
  assert.equal((await send('set_parent', { node_id: 'node:1', parent_id: 'missing' })).ok, false);
});

test('node-wrapper payload.error remains a failure and stops later edits', async () => {
  const { send, nodes } = plugin();
  const result = await send('apply_ops', { ops: [
    { op: 'create_frame', params: {} },
    { op: 'set_plugin_data', params: { node_id: 'node:1' } },
    { op: 'create_frame', params: {} },
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.failure.error, 'key required');
  assert.equal(result.payload.completed.length, 1);
  assert.equal(nodes.size, 2);
});

test('an oversized receipt is not delivered with ok:true after edits ran', async () => {
  const { send, nodes } = plugin();
  const result = await send('apply_ops', { ops: [
    { op: 'create_frame', params: { name: 'x'.repeat(5000) } },
    ...Array.from({ length: 500 }, () => ({ op: 'get_node', params: { node_id: 'node:1' } })),
  ] });
  assert.equal(nodes.has('node:1'), true, 'document effect exists despite receipt failure');
  assert.equal(result.ok, false);
  assert.equal(result.payload.error, 'Payload too large');
  assert.equal(result.payload.status, 'outcome_unknown');
  assert.equal(result.payload.completed[0].node_id, 'node:1');
  assert.equal(result.payload.completed_count, 501);
});

test('multibyte receipts respect UTF-8 bytes and retain compact operation identities', async () => {
  const { send } = plugin();
  const result = await send('apply_ops', { ops: [
    { op: 'create_frame', params: { name: '한'.repeat(1700) } },
    ...Array.from({ length: 450 }, () => ({ op: 'get_node', params: { node_id: 'node:1' } })),
  ] });
  assert.equal(result.ok, false);
  assert.equal(result.payload.error, 'Payload too large');
  assert.ok(result.payload.size > 2000000);
  assert.equal(result.payload.completed[0].node_id, 'node:1');
  assert.equal(result.payload.completed_omitted, 0);
});

test('bridge commands do not interleave with a batch waiting for a font', async () => {
  let releaseFont;
  let fontRequested;
  const started = new Promise(resolve => { fontRequested = resolve; });
  const font = new Promise(resolve => { releaseFont = resolve; });
  const { send, nodes } = plugin({ loadFont: async () => { fontRequested(); await font; } });
  const batch = send('apply_ops', { ops: [
    { op: 'create_text', params: { text: 'First' } },
    { op: 'create_frame', params: { name: 'Second' } },
  ] });
  await started;
  const following = send('create_frame', { name: 'Third' });
  // Network proxy requests are independent of document editing.
  const proxy = await send('llm_proxy');
  assert.equal(proxy.payload.error, 'url required');
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(nodes.size, 2, 'following command must not create a frame yet');
  releaseFont();
  assert.equal((await batch).ok, true);
  assert.equal((await following).ok, true);
  assert.equal(nodes.get('node:2').name, 'Second');
  assert.equal(nodes.get('node:3').name, 'Third');
});

test('existing execute_dsl UI command still creates a design', async () => {
  const { send } = plugin();
  const result = await send('execute_dsl', { operations: [
    { action: 'create_frame', name: 'Existing UI', width: 320, height: 480 },
  ] });
  assert.equal(result.ok, true);
  assert.equal(result.payload.created, 1);
});
