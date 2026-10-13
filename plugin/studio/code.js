/* Figma Studio: the open document is the only canvas authority. */
'use strict';

figma.showUI(__html__, { width: 420, height: 460 });
const sessionId = `studio-${Date.now()}-${Math.random().toString(36).slice(2)}`;
const receipts = new Map();
let queue = Promise.resolve();

function object(value, keys, optional = []) {
  if (!value || typeof value !== 'object' || Array.isArray(value) ||
      keys.some(key => !Object.hasOwn(value, key)) ||
      Object.keys(value).some(key => !keys.includes(key) && !optional.includes(key))) {
    throw new Error(`Expected ${keys.join(', ')}; optional ${optional.join(', ')}`);
  }
  return value;
}
function string(value) {
  if (typeof value !== 'string' || !value.length) throw new Error('Expected nonempty string');
  return value;
}
function number(value, positive = false) {
  if (typeof value !== 'number' || !Number.isFinite(value) || (positive && value <= 0)) {
    throw new Error('Expected finite number with valid range');
  }
  return value;
}
function canonical(value) {
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  if (value && typeof value === 'object') {
    return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
  }
  const encoded = JSON.stringify(value);
  if (encoded === undefined || (typeof value === 'number' && !Number.isFinite(value))) {
    throw new Error('Native export is not finite JSON');
  }
  return encoded;
}
function base64(bytes) {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  let result = '';
  for (let i = 0; i < bytes.length; i += 3) {
    const a = bytes[i], b = bytes[i + 1] || 0, c = bytes[i + 2] || 0;
    result += chars[a >> 2] + chars[((a & 3) << 4) | (b >> 4)] +
      (i + 1 < bytes.length ? chars[((b & 15) << 2) | (c >> 6)] : '=') +
      (i + 2 < bytes.length ? chars[c & 63] : '=');
  }
  return result;
}
async function rootNode(id) {
  const root = await figma.getNodeByIdAsync(string(id));
  if (!root || root.removed || typeof root.exportAsync !== 'function') throw new Error('Exportable root not found');
  if (root.type === 'PAGE') await root.loadAsync();
  return root;
}
function inside(node, root) {
  for (let current = node; current; current = current.parent) if (current.id === root.id) return true;
  return false;
}
function owningPage(node) {
  for (let current = node; current; current = current.parent) if (current.type === 'PAGE') return current;
  throw new Error('Export root has no owning page');
}
async function nativeJson(root) {
  return canonical(await root.exportAsync({ format: 'JSON_REST_V1' }));
}
async function capture(root) {
  const beforePage = owningPage(root).id;
  const before = await nativeJson(root);
  let preview = null;
  const missing = ['font_files', 'variable_values', 'component_code_mapping'];
  try {
    preview = { mime_type: 'image/png', data_base64: base64(await root.exportAsync({ format: 'PNG' })) };
  } catch (error) {
    missing.push(`preview: ${String(error.message || error)}`);
  }
  const after = await nativeJson(root);
  const afterPage = owningPage(root).id;
  return { schema_version: 1, source: 'plugin_live', session_id: sessionId,
    root_id: root.id, page_id: afterPage, tree_json: after, preview,
    coverage: { stable: before === after && beforePage === afterPage, missing }, observed_at: Date.now() / 1000 };
}
function context() {
  return { session_id: sessionId, name: figma.root.name, page_id: figma.currentPage.id,
    selection: figma.currentPage.selection.map(node => ({ id: node.id, name: node.name, type: node.type })),
    editor_type: figma.editorType, mode: figma.mode,
    can_edit: figma.editorType === 'figma' && figma.mode === 'default' };
}

// Complete preflight, including fonts and virtual references, precedes all edits.
async function prepare(root, ops) {
  if (figma.editorType !== 'figma' || figma.mode !== 'default') throw new Error('Canvas edits require Figma Design mode');
  if (!Array.isArray(ops) || !ops.length) throw new Error('ops must be a nonempty array');
  const refs = new Map();
  const fonts = new Map();
  const plans = [];
  async function target(id, allowed) {
    string(id);
    const node = id.startsWith('@') ? refs.get(id) : await figma.getNodeByIdAsync(id);
    if (!node || (!id.startsWith('@') && (!inside(node, root) || node.removed || node.locked))) {
      throw new Error(`Unavailable or out-of-root node: ${id}`);
    }
    if (allowed && !allowed.includes(node.type)) throw new Error(`Invalid node type: ${node.type}`);
    return id;
  }
  function font(value) {
    object(value, ['family', 'style']); string(value.family); string(value.style);
    fonts.set(canonical(value), value);
  }
  for (const entry of ops) {
    object(entry, ['op', 'params']);
    const p = entry.params;
    switch (entry.op) {
      case 'create_frame':
      case 'create_text': {
        const isText = entry.op === 'create_text';
        object(p, isText ? ['ref', 'parent_id', 'text', 'font', 'font_size'] :
          ['ref', 'parent_id', 'name', 'width', 'height'], ['x', 'y']);
        string(p.ref);
        if (!p.ref.startsWith('@') || p.ref.length === 1 || refs.has(p.ref)) throw new Error('Creation ref must be a unique @name');
        await target(p.parent_id, ['PAGE', 'FRAME', 'COMPONENT']);
        if (isText) { if (typeof p.text !== 'string') throw new Error('text must be a string'); font(p.font); number(p.font_size, true); }
        else { string(p.name); number(p.width, true); number(p.height, true); }
        if (Object.hasOwn(p, 'x')) number(p.x);
        if (Object.hasOwn(p, 'y')) number(p.y);
        refs.set(p.ref, { type: isText ? 'TEXT' : 'FRAME' });
        break;
      }
      case 'set_text': {
        object(p, ['node_id', 'text']);
        if (typeof p.text !== 'string') throw new Error('text must be a string');
        await target(p.node_id, ['TEXT']);
        const node = p.node_id.startsWith('@') ? null : await figma.getNodeByIdAsync(p.node_id);
        if (node) {
          if (node.hasMissingFont) throw new Error('Text node has missing fonts');
          const names = node.characters.length ? node.getRangeAllFontNames(0, node.characters.length) : [node.fontName];
          names.forEach(font);
        }
        break;
      }
      case 'rename': object(p, ['node_id', 'name']); string(p.name); await target(p.node_id); break;
      case 'resize':
        object(p, ['node_id', 'width', 'height']); number(p.width, true); number(p.height, true);
        await target(p.node_id, ['FRAME', 'COMPONENT', 'RECTANGLE', 'ELLIPSE', 'TEXT', 'INSTANCE']); break;
      case 'move':
        object(p, ['node_id', 'x', 'y']); number(p.x); number(p.y);
        await target(p.node_id, ['FRAME', 'COMPONENT', 'RECTANGLE', 'ELLIPSE', 'TEXT', 'INSTANCE', 'GROUP']); break;
      case 'set_fill':
        object(p, ['node_id', 'color']); object(p.color, ['r', 'g', 'b']);
        for (const value of Object.values(p.color)) { number(value); if (value < 0 || value > 1) throw new Error('Color outside 0..1'); }
        await target(p.node_id, ['FRAME', 'COMPONENT', 'RECTANGLE', 'ELLIPSE', 'TEXT', 'INSTANCE']); break;
      case 'set_auto_layout':
        object(p, ['node_id', 'direction', 'spacing', 'padding']);
        if (!['HORIZONTAL', 'VERTICAL', 'NONE'].includes(p.direction)) throw new Error('Invalid layout direction');
        number(p.spacing); number(p.padding);
        if (p.spacing < 0 || p.padding < 0) throw new Error('Layout spacing/padding must be nonnegative');
        await target(p.node_id, ['FRAME', 'COMPONENT']); break;
      default: throw new Error(`Unsupported operation: ${entry.op}`);
    }
    plans.push(entry);
  }
  await Promise.all([...fonts.values()].map(value => figma.loadFontAsync(value)));
  // Recheck current document after all awaited preflight work.
  if (root.removed || figma.editorType !== 'figma' || figma.mode !== 'default') throw new Error('Editing context changed');
  return plans;
}
async function apply(root, request) {
  const plans = await prepare(root, request.ops);
  if (await nativeJson(root) !== request.expected_tree_json) throw new Error('Design changed since capture; capture again');
  const refs = new Map();
  const completed = [];
  let effectsStarted = false;
  try {
    for (const { op, params: p } of plans) {
      let node;
      const resolve = async id => id.startsWith('@') ? refs.get(id) : figma.getNodeByIdAsync(id);
      // Resolve and validate again: collaborators may change a node after preflight.
      const id = op.startsWith('create_') ? p.parent_id : p.node_id;
      const subject = await resolve(id);
      if (!subject || subject.removed || !inside(subject, root) || subject.locked) throw new Error('Edit target changed');
      effectsStarted = true;
      switch (op) {
        case 'create_frame':
          node = figma.createFrame(); refs.set(p.ref, node); completed.push({ op, node_id: node.id });
          subject.appendChild(node); node.name = p.name; node.resize(p.width, p.height); break;
        case 'create_text':
          node = figma.createText(); refs.set(p.ref, node); completed.push({ op, node_id: node.id });
          subject.appendChild(node); node.fontName = p.font; node.fontSize = p.font_size; node.characters = p.text; break;
        case 'set_text': subject.characters = p.text; break;
        case 'rename': subject.name = p.name; break;
        case 'resize': subject.resize(p.width, p.height); break;
        case 'move': subject.x = p.x; subject.y = p.y; break;
        case 'set_fill': subject.fills = [{ type: 'SOLID', color: p.color }]; break;
        case 'set_auto_layout':
          subject.layoutMode = p.direction; subject.itemSpacing = p.spacing;
          subject.paddingTop = subject.paddingRight = subject.paddingBottom = subject.paddingLeft = p.padding; break;
      }
      if (node) {
        if (Object.hasOwn(p, 'x')) node.x = p.x;
        if (Object.hasOwn(p, 'y')) node.y = p.y;
      } else completed.push({ op, node_id: subject.id });
    }
    figma.commitUndo();
    return { ok: true, payload: { ...await capture(root), completed } };
  } catch (error) {
    return { ok: false, payload: { status: effectsStarted ? 'outcome_unknown' : 'rejected',
      error: String(error.message || error), completed } };
  }
}

async function command(request) {
  object(request, ['id', 'session_id', 'kind'], ['root_id', 'expected_tree_json', 'ops']);
  string(request.id);
  const encoded = canonical(request);
  const prior = receipts.get(request.id);
  if (prior) {
    if (prior.request !== encoded) throw new Error('Operation ID already has different input');
    return prior.result;
  }
  if (request.session_id !== sessionId) throw new Error('Wrong Plugin session');
  const root = await rootNode(request.root_id);
  let result;
  if (request.kind === 'capture') result = { ok: true, payload: await capture(root) };
  else if (request.kind === 'apply') {
    string(request.expected_tree_json);
    result = await apply(root, request);
  } else throw new Error('Unknown Studio command');
  receipts.set(request.id, { request: encoded, result });
  return result;
}

figma.ui.onmessage = message => {
  if (!message || message.session_id !== sessionId) return;
  if (message.type === 'context') {
    figma.ui.postMessage({ type: 'context', ...context() });
    return;
  }
  if (message.type !== 'command') return;
  queue = queue.then(async () => {
    let result;
    try { result = await command(message.command); }
    catch (error) { result = { ok: false, payload: { status: 'rejected', error: String(error.message || error), completed: [] } }; }
    figma.ui.postMessage({ type: 'result', id: message.command.id, session_id: sessionId, ...result });
  }).catch(error => figma.notify(`Studio reply failed: ${String(error.message || error)}`));
};
figma.on('selectionchange', () => figma.ui.postMessage({ type: 'context', ...context() }));
figma.on('currentpagechange', () => figma.ui.postMessage({ type: 'context', ...context() }));
figma.ui.postMessage({ type: 'context', ...context() });
