const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const tick = () => new Promise(resolve => setImmediate(resolve));
const response = value => ({ ok: true, json: async () => value });

function studioUI(onFetch) {
  const elements = Object.fromEntries(
    ['status', 'document', 'connect', 'disconnect', 'url', 'token']
      .map(id => [id, { value: '', disabled: false }])
  );
  elements.url.value = 'http://127.0.0.1:8941';
  elements.token.value = 'fixture-token';
  const sent = [];
  const parent = { postMessage(message) { sent.push(message.pluginMessage.command); } };
  const fixture = { elements, sent };
  const scope = vm.createContext({
    document: { getElementById: id => elements[id] },
    parent, URL, TextEncoder, setTimeout, onmessage: null,
    fetch: (url, init) => onFetch(url, init, fixture),
  });
  const html = fs.readFileSync(path.join(__dirname, '../../plugin/studio/ui.html'), 'utf8');
  const script = html.match(/<script>([\s\S]*)<\/script>/)[1];
  vm.runInContext(script, scope);
  fixture.deliver = message => scope.onmessage({ source: parent, data: { pluginMessage: message } });
  fixture.deliver({ type: 'context', session_id: 'session', name: 'Fixture', selection: [] });
  return fixture;
}

test('overlapping Connect clicks start one poller and retain every command result', async () => {
  const handshakes = [], retained = [];
  let pollCount = 0;
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
      if (url.endsWith('/session')) {
        return new Promise(resolve => handshakes.push(resolve));
      }
      if (url.endsWith('/result')) {
        retained.push(JSON.parse(init.body).operation_id);
        return response({});
      }
      assert.ok(url.endsWith('/poll'));
      pollCount++;
      if (pollCount > 2) {
        fixture.elements.disconnect.onclick();
        return response({});
      }
      return response({ command: {
        id: `op:${pollCount}`, session_id: 'session', kind: 'capture', root_id: 'root',
      } });
  });

  const first = elements.connect.onclick();
  assert.equal(elements.connect.disabled, true, 'Connect is disabled before the handshake awaits');
  const second = elements.connect.onclick();
  await tick();
  assert.equal(handshakes.length, 1, 'a second click cannot start another session handshake');
  handshakes[0](response({ max_request_bytes: 1000000, poll_interval_ms: 0 }));
  await tick();
  assert.deepEqual(sent.map(command => command.id), ['op:1']);

  deliver({ type: 'result', id: 'op:1', session_id: 'session', ok: true, payload: { tree_json: '{}' } });
  await tick();
  assert.deepEqual(retained, ['op:1']);
  assert.deepEqual(sent.map(command => command.id), ['op:1', 'op:2']);
  deliver({ type: 'result', id: 'op:2', session_id: 'session', ok: true, payload: { tree_json: '{}' } });
  await Promise.all([first, second]);
  assert.deepEqual(retained, ['op:1', 'op:2']);
  assert.equal(elements.connect.disabled, false, 'the completed connection can be started again');
});

test('oversized native replies retain a bounded UTF-8 changed-node summary with an unknown outcome', async () => {
  const maxBytes = 1024, retained = [];
  let pollCount = 0;
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/session')) return response({ max_request_bytes: maxBytes, poll_interval_ms: 0 });
    if (url.endsWith('/result')) {
      retained.push({ bytes: Buffer.byteLength(init.body, 'utf8'), body: JSON.parse(init.body) });
      return response({});
    }
    assert.ok(url.endsWith('/poll'));
    if (++pollCount > 1) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: { id: 'edit:1', session_id: 'session', kind: 'apply', root_id: 'root' } });
  });
  const connection = elements.connect.onclick();
  await tick();
  assert.deepEqual(sent.map(command => command.id), ['edit:1']);
  const completed = Array.from({ length: 100 }, (_, index) => ({
    op: 'create_frame', node_id: `생성된-노드:${index}`, extra: 'discarded detail'.repeat(100),
  }));
  deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: {
    status: 'applied', tree_json: '큰 readback'.repeat(1000), completed,
  } });
  await connection;

  assert.equal(retained.length, 1);
  const { bytes, body } = retained[0];
  assert.ok(bytes <= maxBytes, `the full UTF-8 envelope is ${bytes} bytes`);
  assert.equal(body.ok, false);
  assert.equal(body.payload.status, 'outcome_unknown');
  assert.equal(body.payload.completed_count, completed.length);
  assert.ok(body.payload.completed.length > 0, 'changed node identities remain available');
  assert.ok(body.payload.completed_omitted > 0, 'omitted identities are explicit');
  assert.equal(body.payload.completed.length + body.payload.completed_omitted, completed.length);
  assert.deepEqual(body.payload.completed, completed.slice(0, body.payload.completed.length)
    .map(({ op, node_id }, index) => ({ index, op, node_id })));
  assert.equal(body.payload.details_omitted, true);
  assert.equal(body.session_id, 'session');
  assert.equal(body.operation_id, 'edit:1');
});

test('a receipt that cannot fit is retained for reconnect without posting or replaying the edit', async () => {
  let maxBytes = 32, pollCount = 0;
  const retained = [];
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/session')) return response({ max_request_bytes: maxBytes, poll_interval_ms: 0 });
    if (url.endsWith('/result')) {
      retained.push({ bytes: Buffer.byteLength(init.body, 'utf8'), body: JSON.parse(init.body) });
      return response({});
    }
    assert.ok(url.endsWith('/poll'));
    if (++pollCount > 1) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: { id: 'edit:1', session_id: 'session', kind: 'apply', root_id: 'root' } });
  });
  const first = elements.connect.onclick();
  await tick();
  deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: {
    status: 'applied', tree_json: 'large'.repeat(1000), completed: [{ op: 'create_frame', node_id: 'created:1' }],
  } });
  await first;
  assert.equal(retained.length, 0, 'an oversized minimal receipt is never sent');
  assert.match(elements.status.textContent, /Relay envelope cannot hold the operation receipt/);
  maxBytes = 1024;
  await elements.connect.onclick();
  assert.deepEqual(sent.map(command => command.id), ['edit:1'], 'reconnect does not re-execute the edit');
  assert.equal(retained.length, 1);
  assert.ok(retained[0].bytes <= maxBytes);
  assert.equal(retained[0].body.ok, false);
  assert.equal(retained[0].body.payload.status, 'outcome_unknown');
  assert.equal(retained[0].body.payload.completed[0].node_id, 'created:1');
});
