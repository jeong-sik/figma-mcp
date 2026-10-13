const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const tick = () => new Promise(resolve => setImmediate(resolve));
const response = value => ({ ok: true, json: async () => value });
const refusal = (status, value) => ({ ok: false, status, json: async () => value });
const sessionResponse = (maxBytes = 1000000, boot = 'boot:1') => response({
  protocol: 'figma-studio/1', service_id: boot, max_request_bytes: maxBytes, poll_interval_ms: 0,
});

function studioUI(onFetch) {
  const elements = Object.fromEntries(
    ['status', 'uncertain', 'document', 'connect', 'disconnect', 'url', 'token']
      .map(id => [id, { value: '', disabled: false }])
  );
  elements.url.value = 'http://127.0.0.1:8941';
  elements.token.value = 'fixture-token';
  const sent = [];
  const parent = { postMessage(message) { sent.push(message.pluginMessage.command); } };
  const fixture = { elements, sent };
  const scope = vm.createContext({
    document: { getElementById: id => elements[id] },
    parent, URL, TextEncoder, AbortController, setTimeout, onmessage: null,
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
      if (url.endsWith('/v1/plugin/session')) {
        return new Promise(resolve => handshakes.push(resolve));
      }
      if (url.endsWith('/v1/plugin/result')) {
        retained.push(JSON.parse(init.body).operation_id);
        return response({});
      }
      assert.ok(url.endsWith('/v1/plugin/poll'));
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
  handshakes[0](sessionResponse());
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
    if (url.endsWith('/v1/plugin/session')) return sessionResponse(maxBytes);
    if (url.endsWith('/v1/plugin/result')) {
      retained.push({ bytes: Buffer.byteLength(init.body, 'utf8'), body: JSON.parse(init.body) });
      return response({});
    }
    assert.ok(url.endsWith('/v1/plugin/poll'));
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
  assert.equal(body.service_id, 'boot:1');
  assert.equal(body.operation_id, 'edit:1');
});

test('an unrepresentable receipt remains visible and does not block a new command', async () => {
  const maxBytes = 240;
  let pollCount = 0;
  const retained = [];
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/v1/plugin/session')) return sessionResponse(maxBytes);
    if (url.endsWith('/v1/plugin/result')) {
      retained.push({ bytes: Buffer.byteLength(init.body, 'utf8'), body: JSON.parse(init.body) });
      return response({});
    }
    assert.ok(url.endsWith('/v1/plugin/poll'));
    if (++pollCount > 2) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: {
      id: pollCount === 1 ? 'edit:1' : 'read:2', session_id: 'session',
      kind: pollCount === 1 ? 'apply' : 'capture', root_id: 'root',
    } });
  });
  const first = elements.connect.onclick();
  await tick();
  deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: {
    status: 'applied', tree_json: 'large'.repeat(1000), completed: [{ op: 'create_frame', node_id: 'created:1' }],
  } });
  await tick();
  assert.equal(retained.length, 0, 'an oversized minimal receipt is never sent');
  assert.deepEqual(sent.map(command => command.id), ['edit:1', 'read:2']);
  assert.match(elements.uncertain.textContent, /Result not collected; document effects are unknown/);
  assert.match(elements.uncertain.textContent, /Operation: edit:1/);
  assert.match(elements.uncertain.textContent, /created:1/);
  deliver({ type: 'result', id: 'read:2', session_id: 'session', ok: true, payload: { tree_json: '{}' } });
  await first;
  assert.equal(retained.length, 1);
  assert.ok(retained[0].bytes <= maxBytes);
  assert.equal(retained[0].body.operation_id, 'read:2');
  assert.match(elements.uncertain.textContent, /Operation: edit:1/, 'later success does not erase the warning');
});

test('worker restart refusal retains uncertain details, then re-pairs and polls without replaying', { timeout: 2000 }, async () => {
  const receipts = [];
  let sessions = 0, polls = 0;
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/v1/plugin/session')) return sessionResponse(1000000, ++sessions === 1 ? 'boot:old' : 'boot:new');
    if (url.endsWith('/v1/plugin/result')) {
      receipts.push(init.body);
      const body = JSON.parse(init.body);
      if (receipts.length === 1)
        return refusal(502, { kind: 'transport_error', retryable: true, error: 'Worker connection lost' });
      if (body.operation_id === 'edit:1')
        return refusal(409, { kind: 'application_refused', retryable: false, error: 'Result belongs to another session/worker' });
      return response({});
    }
    assert.ok(url.endsWith('/v1/plugin/poll'));
    if (++polls > 2) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: {
      id: polls === 1 ? 'edit:1' : 'read:2', session_id: 'session',
      kind: polls === 1 ? 'apply' : 'capture', root_id: 'root',
    } });
  });
  const first = elements.connect.onclick();
  await tick();
  deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: {
    status: 'applied', completed: [{ op: 'create_frame', node_id: 'created:1' }],
  } });
  await first;
  assert.equal(polls, 1, 'transport errors preserve the pending delivery');
  assert.equal(JSON.parse(receipts[0]).service_id, 'boot:old');

  const second = elements.connect.onclick();
  await tick();
  assert.equal(sessions, 3, 'definitive refusal triggers a fresh registration before polling');
  assert.equal(receipts[1], receipts[0], 'the retry keeps the original worker and exact receipt');
  assert.deepEqual(sent.map(command => command.id), ['edit:1', 'read:2']);
  assert.match(elements.uncertain.textContent, /Result not collected; document effects are unknown/);
  assert.match(elements.uncertain.textContent, /Operation: edit:1/);
  assert.match(elements.uncertain.textContent, /Result belongs to another session\/worker/);
  assert.match(elements.uncertain.textContent, /created:1/);
  deliver({ type: 'result', id: 'read:2', session_id: 'session', ok: true, payload: { tree_json: '{}' } });
  await second;
  assert.equal(JSON.parse(receipts[2]).service_id, 'boot:new');
  assert.match(elements.uncertain.textContent, /Operation: edit:1/);
});

test('only explicitly permanent delivery failures release a pending result', { timeout: 2000 }, async () => {
  for (const firstReply of [
    refusal(409, { error: 'Unclassified conflict' }),
    refusal(413, { kind: 'request_too_large', retryable: true, error: 'Unclassified size failure' }),
  ]) {
    const receipts = [];
    let polls = 0;
    const { elements, deliver } = studioUI(async (url, init, fixture) => {
      if (url.endsWith('/v1/plugin/session')) return sessionResponse();
      if (url.endsWith('/v1/plugin/result')) {
        receipts.push(init.body);
        return receipts.length === 1 ? firstReply : response({});
      }
      if (++polls > 1) {
        fixture.elements.disconnect.onclick();
        return response({});
      }
      return response({ command: { id: 'edit:1', session_id: 'session', kind: 'apply', root_id: 'root' } });
    });
    const first = elements.connect.onclick();
    await tick();
    deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: { completed: [] } });
    await first;
    assert.equal(polls, 1);
    assert.equal(elements.uncertain.textContent, undefined);
    await elements.connect.onclick();
    assert.equal(receipts[1], receipts[0]);
  }
});

test('typed remote receipt-size refusal does not block later polling', { timeout: 2000 }, async () => {
  let polls = 0;
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/v1/plugin/session')) return sessionResponse();
    if (url.endsWith('/v1/plugin/result')) return refusal(413, {
      kind: 'request_too_large', retryable: false, error: 'MCP envelope cannot hold the receipt',
    });
    if (++polls > 1) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: { id: 'edit:1', session_id: 'session', kind: 'apply', root_id: 'root' } });
  });
  const connection = elements.connect.onclick();
  await tick();
  deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: {
    completed: [{ op: 'rename', node_id: 'changed:1' }],
  } });
  await connection;
  assert.equal(polls, 2);
  assert.deepEqual(sent.map(command => command.id), ['edit:1']);
  assert.match(elements.uncertain.textContent, /Receipt too large: MCP envelope cannot hold the receipt/);
  assert.match(elements.uncertain.textContent, /changed:1/);
});

test('Disconnect aborts a waiting poll and permits reconnect without a late command', { timeout: 2000 }, async () => {
  let polls = 0, blockedSignal;
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/v1/plugin/session')) return sessionResponse();
    if (url.endsWith('/v1/plugin/result')) return response({});
    if (++polls === 1) {
      blockedSignal = init.signal;
      return new Promise((resolve, reject) => init.signal.addEventListener('abort', () => reject(init.signal.reason), { once: true }));
    }
    if (polls > 2) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: { id: 'read:2', session_id: 'session', kind: 'capture', root_id: 'root' } });
  });
  const first = elements.connect.onclick();
  await tick();
  elements.disconnect.onclick();
  await first;
  assert.equal(blockedSignal.aborted, true);
  assert.equal(polls, 1);
  assert.deepEqual(sent, []);
  const second = elements.connect.onclick();
  await tick();
  deliver({ type: 'result', id: 'read:2', session_id: 'session', ok: true, payload: { tree_json: '{}' } });
  await second;
  assert.deepEqual(sent.map(command => command.id), ['read:2']);
});

test('Disconnect aborts result body consumption and reconnect retries the same receipt', { timeout: 2000 }, async () => {
  const receipts = [];
  let polls = 0, bodySignal;
  const { elements, sent, deliver } = studioUI(async (url, init, fixture) => {
    if (url.endsWith('/v1/plugin/session')) return sessionResponse();
    if (url.endsWith('/v1/plugin/result')) {
      receipts.push(init.body);
      if (receipts.length > 1) return response({});
      bodySignal = init.signal;
      return { ok: true, json: () => new Promise((resolve, reject) => {
        init.signal.addEventListener('abort', () => reject(init.signal.reason), { once: true });
      }) };
    }
    if (++polls > 1) {
      fixture.elements.disconnect.onclick();
      return response({});
    }
    return response({ command: { id: 'edit:1', session_id: 'session', kind: 'apply', root_id: 'root' } });
  });
  const first = elements.connect.onclick();
  await tick();
  deliver({ type: 'result', id: 'edit:1', session_id: 'session', ok: true, payload: {
    completed: [{ op: 'rename', node_id: 'changed:1' }],
  } });
  await tick();
  elements.disconnect.onclick();
  await first;
  assert.equal(bodySignal.aborted, true, 'abort remains attached while response.json is pending');
  assert.equal(polls, 1);
  await elements.connect.onclick();
  assert.equal(receipts[1], receipts[0]);
  assert.deepEqual(sent.map(command => command.id), ['edit:1']);
});
