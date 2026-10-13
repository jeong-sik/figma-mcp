const fs = require('node:fs');
const path = require('node:path');
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

module.exports = { studio, png };
