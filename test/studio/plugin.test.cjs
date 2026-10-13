const assert = require('node:assert/strict');
const test = require('node:test');
const { studio, png } = require('./fixture.cjs');

test('native capture returns REST structure and the exported PNG from the actual entrypoint', async () => {
  const s = studio();
  const capture = await s.send('capture');
  assert.equal(capture.ok, true);
  assert.equal(JSON.parse(capture.payload.tree_json).document.id, s.frame.id);
  assert.deepEqual(Buffer.from(capture.payload.preview.data_base64, 'base64'), png);
  assert.equal(capture.payload.coverage.stable, true);
  assert.ok(capture.payload.coverage.missing.includes('font_files'));
});

test('capture keeps the root owning page when the active page changes during export', async () => {
  const s = studio();
  s.hook((node, format) => {
    if (format === 'PNG') s.figma.currentPage = { id: 'other-page', selection: [] };
  });
  const capture = await s.send('capture');
  assert.equal(capture.ok, true);
  assert.equal(capture.payload.page_id, s.page.id);
  assert.equal(capture.payload.root_id, s.frame.id);
  assert.equal(capture.payload.coverage.stable, true, 'changing UI pages does not change the captured tree');
});

test('capture reports a root moving to another page as unstable', async () => {
  const s = studio();
  const otherPage = { id: 'other-page', type: 'PAGE', children: [], appendChild: s.page.appendChild };
  s.hook((node, format) => { if (format === 'PNG') otherPage.appendChild(node); });
  const capture = await s.send('capture');
  assert.equal(capture.ok, true);
  assert.equal(capture.payload.page_id, otherPage.id);
  assert.equal(capture.payload.coverage.stable, false);
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
