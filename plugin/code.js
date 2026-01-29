/**
 * Figma MCP Plugin - Refactored (Table-based handlers)
 * 100 actions, ~800 lines (vs 2500 lines before)
 */

figma.showUI(__html__, { width: 360, height: 600 });

const MIXED = figma.mixed;
const MAX_PAYLOAD_CHARS = 2000000;

// ============== Utilities ==============

function normalizeNodeId(value) {
  if (!value || typeof value !== "string") return value;
  if (value.includes(":")) return value;
  if (value.includes("-")) return value.replace(/-/g, ":");
  return value;
}

async function getNodeById(nodeId) {
  if (!nodeId) return null;
  const normalized = normalizeNodeId(nodeId);
  return typeof figma.getNodeByIdAsync === "function"
    ? await figma.getNodeByIdAsync(normalized)
    : figma.getNodeById(normalized);
}

async function getNodeOrSelection(p) {
  const nodeId = p.node_id || p.nodeId;
  if (nodeId) return await getNodeById(nodeId);
  const sel = figma.currentPage.selection;
  return sel.length > 0 ? sel[0] : null;
}

async function getNodes(p) {
  const ids = p.node_ids || p.nodeIds || [];
  const nodes = [];
  for (const id of ids) {
    const node = await getNodeById(id);
    if (node) nodes.push(node);
  }
  return nodes;
}

function serializeColor(c) {
  if (!c) return null;
  return { r: c.r, g: c.g, b: c.b, a: c.a !== undefined ? c.a : 1 };
}

function serializePaint(paint) {
  if (!paint) return null;
  const base = { type: paint.type, visible: paint.visible !== false, opacity: paint.opacity || 1 };
  if (paint.type === "SOLID") base.color = serializeColor(paint.color);
  if (paint.type === "IMAGE") base.imageHash = paint.imageHash;
  return base;
}

function serializePaints(paints) {
  if (paints === MIXED) return { mixed: true };
  return Array.isArray(paints) ? paints.map(serializePaint) : [];
}

function serializeEffects(effects) {
  if (effects === MIXED) return { mixed: true };
  return Array.isArray(effects) ? effects.map(e => ({
    type: e.type, visible: e.visible, radius: e.radius,
    color: serializeColor(e.color), offset: e.offset, spread: e.spread
  })) : [];
}

function safeStringify(value) {
  const seen = new WeakSet();
  return JSON.stringify(value, (key, val) => {
    if (val === MIXED) return { mixed: true };
    if (typeof val === "bigint") return Number(val);
    if (typeof val === "symbol" || typeof val === "function") return undefined;
    if (val && typeof val === "object") {
      if (seen.has(val)) return "[Circular]";
      seen.add(val);
    }
    return val;
  });
}

function parseColor(c) {
  if (!c) return { r: 0, g: 0, b: 0 };
  if (typeof c === "string" && c.startsWith("#")) {
    const hex = c.slice(1);
    return {
      r: parseInt(hex.substr(0, 2), 16) / 255,
      g: parseInt(hex.substr(2, 2), 16) / 255,
      b: parseInt(hex.substr(4, 2), 16) / 255
    };
  }
  return { r: c.r || 0, g: c.g || 0, b: c.b || 0 };
}

function serializeNode(node, depth = 1, maxDepth = 6) {
  if (!node || depth > maxDepth) return null;
  const base = { id: node.id, name: node.name, type: node.type };
  if ("visible" in node) base.visible = node.visible;
  if ("locked" in node) base.locked = node.locked;
  if ("x" in node) Object.assign(base, { x: node.x, y: node.y, width: node.width, height: node.height });
  if ("fills" in node) base.fills = serializePaints(node.fills);
  if ("strokes" in node) base.strokes = serializePaints(node.strokes);
  if ("effects" in node) base.effects = serializeEffects(node.effects);
  if ("opacity" in node) base.opacity = node.opacity;
  if ("cornerRadius" in node) base.cornerRadius = node.cornerRadius;
  if ("characters" in node) base.characters = node.characters;
  if ("children" in node && depth < maxDepth) {
    base.children = node.children.map(c => serializeNode(c, depth + 1, maxDepth));
  }
  return base;
}

// ============== Handler Wrappers ==============

const H = {
  // Simple handler (no node required)
  simple: (fn) => async (p) => {
    const result = await fn(p);
    return { ok: true, payload: result };
  },

  // Requires a node (from node_id or selection)
  node: (fn, opts = {}) => async (p) => {
    const node = await getNodeOrSelection(p);
    if (!node) return { ok: false, payload: { error: "Node not found" } };
    if (opts.type && node.type !== opts.type) {
      return { ok: false, payload: { error: `Node must be ${opts.type}` } };
    }
    if (opts.check && !opts.check(node)) {
      return { ok: false, payload: { error: opts.checkMsg || "Invalid node" } };
    }
    const result = await fn(node, p);
    return { ok: true, payload: result };
  },

  // Requires specific node_id
  nodeRequired: (fn) => async (p) => {
    const nodeId = p.node_id || p.nodeId;
    if (!nodeId) return { ok: false, payload: { error: "node_id required" } };
    const node = await getNodeById(nodeId);
    if (!node) return { ok: false, payload: { error: "Node not found" } };
    const result = await fn(node, p);
    return { ok: true, payload: result };
  },

  // Multiple nodes
  nodes: (fn) => async (p) => {
    const nodes = await getNodes(p);
    if (nodes.length === 0) return { ok: false, payload: { error: "No valid nodes" } };
    const result = await fn(nodes, p);
    return { ok: true, payload: result };
  },
};

// ============== Handler Table ==============

const handlers = {
  // === Connection (3) ===
  status: H.simple(() => ({ connected: true, page: figma.currentPage.name })),

  // === Pages (4) ===
  list_pages: H.simple(() => ({
    pages: figma.root.children.map(p => ({ id: p.id, name: p.name })),
    current: figma.currentPage.id
  })),

  switch_page: H.simple(async (p) => {
    const pageId = p.page_id || p.pageId;
    const page = pageId ? await getNodeById(pageId) : null;
    if (!page || page.type !== "PAGE") return { error: "Page not found" };
    figma.currentPage = page;
    return { page_id: page.id, page_name: page.name };
  }),

  create_page: H.simple((p) => {
    const page = figma.createPage();
    page.name = p.name || "New Page";
    return { page_id: page.id, page_name: page.name };
  }),

  delete_page: H.simple(async (p) => {
    const pageId = p.page_id || p.pageId;
    const page = pageId ? await getNodeById(pageId) : null;
    if (!page || page.type !== "PAGE") return { error: "Page not found" };
    if (figma.root.children.length <= 1) return { error: "Cannot delete last page" };
    page.remove();
    return { deleted: pageId };
  }),

  // === Document (1) ===
  get_doc_info: H.simple(() => ({
    name: figma.root.name,
    id: figma.root.id,
    page_count: figma.root.children.length,
    pages: figma.root.children.map(p => ({ id: p.id, name: p.name })),
    current_page: { id: figma.currentPage.id, name: figma.currentPage.name }
  })),

  // === Query (20) ===
  read_selection: H.simple(() => {
    const sel = figma.currentPage.selection;
    return { count: sel.length, nodes: sel.map(n => serializeNode(n, 1, 3)) };
  }),

  get_node: H.nodeRequired((node, p) => {
    const depth = p.depth || 6;
    return serializeNode(node, 1, depth);
  }),

  get_viewport: H.simple(() => ({
    center: figma.viewport.center,
    zoom: figma.viewport.zoom,
    bounds: figma.viewport.bounds
  })),

  get_local_styles: H.simple(() => {
    var safeMap = function(fn) {
      try { return fn().map(function(s) { return { id: s.id, name: s.name }; }); }
      catch (e) { return []; }
    };
    return {
      paint: safeMap(function() { return figma.getLocalPaintStyles(); }),
      text: safeMap(function() { return figma.getLocalTextStyles(); }),
      effect: safeMap(function() { return figma.getLocalEffectStyles(); }),
      grid: safeMap(function() { return figma.getLocalGridStyles(); })
    };
  }),

  get_variables: H.simple(async () => {
    var result = { collections: [], variables: [] };
    try {
      if (typeof figma.variables !== "undefined") {
        var collections = await figma.variables.getLocalVariableCollectionsAsync();
        result.collections = collections.map(function(c) { return { id: c.id, name: c.name, modes: c.modes }; });
        var variables = await figma.variables.getLocalVariablesAsync();
        result.variables = variables.map(function(v) { return { id: v.id, name: v.name, resolvedType: v.resolvedType }; });
      }
    } catch (e) { result.error = String(e); }
    return result;
  }),

  export_tokens: H.simple(() => {
    var tokens = { colors: [], typography: [] };
    try {
      var paintStyles = figma.getLocalPaintStyles();
      for (var i = 0; i < paintStyles.length; i++) {
        var ps = paintStyles[i];
        if (ps.paints && ps.paints.length > 0 && ps.paints[0].type === "SOLID") {
          var c = ps.paints[0].color;
          tokens.colors.push({
            name: ps.name,
            hex: "#" + Math.round(c.r * 255).toString(16).padStart(2, "0") +
                      Math.round(c.g * 255).toString(16).padStart(2, "0") +
                      Math.round(c.b * 255).toString(16).padStart(2, "0")
          });
        }
      }
      var textStyles = figma.getLocalTextStyles();
      for (var j = 0; j < textStyles.length; j++) {
        var ts = textStyles[j];
        tokens.typography.push({
          name: ts.name,
          fontSize: ts.fontSize,
          fontFamily: ts.fontName ? ts.fontName.family : null,
          fontWeight: ts.fontName ? ts.fontName.style : null
        });
      }
    } catch (e) { tokens.error = String(e); }
    return tokens;
  }),

  get_selection_colors: H.simple(() => {
    const colors = [];
    for (const node of figma.currentPage.selection) {
      if ("fills" in node && node.fills !== MIXED) {
        for (const fill of node.fills) {
          if (fill.type === "SOLID") colors.push({ type: "fill", color: serializeColor(fill.color) });
        }
      }
      if ("strokes" in node) {
        for (const stroke of node.strokes) {
          if (stroke.type === "SOLID") colors.push({ type: "stroke", color: serializeColor(stroke.color) });
        }
      }
    }
    return { colors };
  }),

  get_fonts: H.simple(async () => {
    const fonts = new Map();
    const textNodes = figma.currentPage.findAllWithCriteria({ types: ["TEXT"] });
    for (const node of textNodes) {
      const fontName = node.fontName;
      if (fontName !== MIXED) {
        const key = fontName.family + "|" + fontName.style;
        if (!fonts.has(key)) fonts.set(key, { family: fontName.family, style: fontName.style });
      }
    }
    return { fonts: Array.from(fonts.values()) };
  }),

  get_absolute_bounds: H.node((node) => ({
    node_id: node.id,
    absolute_bounds: node.absoluteBoundingBox,
    absolute_transform: node.absoluteTransform
  })),

  get_plugin_data: H.node((node, p) => {
    const key = p.key;
    if (!key) {
      const keys = node.getPluginDataKeys();
      const data = {};
      for (const k of keys) data[k] = node.getPluginData(k);
      return { node_id: node.id, data };
    }
    return { node_id: node.id, key, value: node.getPluginData(key) };
  }),

  get_shared_plugin_data: H.node((node, p) => {
    const ns = p.namespace || "shared";
    const key = p.key;
    if (!key) {
      const keys = node.getSharedPluginDataKeys(ns);
      const data = {};
      for (const k of keys) data[k] = node.getSharedPluginData(ns, k);
      return { node_id: node.id, namespace: ns, data };
    }
    return { node_id: node.id, namespace: ns, key, value: node.getSharedPluginData(ns, key) };
  }),

  get_reactions: H.node((node) => {
    if (!("reactions" in node)) return { error: "Node does not support reactions" };
    return { node_id: node.id, reactions: node.reactions };
  }, { check: n => "reactions" in n, checkMsg: "Node does not support reactions" }),

  get_overrides: H.node((node) => ({
    node_id: node.id,
    main_component: node.mainComponent ? { id: node.mainComponent.id, name: node.mainComponent.name } : null,
    overrides: node.overrides || []
  }), { type: "INSTANCE" }),

  get_layer_list: H.simple((p) => {
    var node = p.node_id ? figma.getNodeById(p.node_id) : figma.currentPage;
    if (!node || !("children" in node)) return { error: "Node has no children" };
    return {
      parent_id: node.id,
      layers: node.children.map(function(c, i) { return { index: i, id: c.id, name: c.name, type: c.type, visible: c.visible }; })
    };
  }),

  get_paint_styles: H.node((node) => ({
    node_id: node.id,
    fills: "fills" in node ? node.fills : null,
    strokes: "strokes" in node ? node.strokes : null,
    fill_style_id: "fillStyleId" in node ? node.fillStyleId : null,
    stroke_style_id: "strokeStyleId" in node ? node.strokeStyleId : null
  })),

  get_stroke_details: H.node((node) => ({
    node_id: node.id,
    strokes: node.strokes,
    stroke_weight: node.strokeWeight,
    stroke_align: node.strokeAlign,
    stroke_cap: node.strokeCap,
    stroke_join: node.strokeJoin,
    dash_pattern: node.dashPattern
  }), { check: n => "strokes" in n, checkMsg: "Node does not have strokes" }),

  get_characters: H.node(async (node) => {
    const chars = node.characters;
    const styles = [];
    let start = 0, currentStyle = null;
    for (let i = 0; i <= chars.length; i++) {
      let style = null;
      if (i < chars.length) {
        const fontSize = node.getRangeFontSize(i, i + 1);
        const fontName = node.getRangeFontName(i, i + 1);
        style = { fontSize, fontFamily: fontName === MIXED ? "mixed" : fontName.family };
      }
      if (i === chars.length || JSON.stringify(style) !== JSON.stringify(currentStyle)) {
        if (currentStyle) {
          var styleEntry = { start: start, end: i, text: chars.substring(start, i) };
          if (currentStyle.fontSize !== undefined) styleEntry.fontSize = currentStyle.fontSize;
          if (currentStyle.fontFamily !== undefined) styleEntry.fontFamily = currentStyle.fontFamily;
          styles.push(styleEntry);
        }
        currentStyle = style;
        start = i;
      }
    }
    return { node_id: node.id, characters: chars, length: chars.length, styles };
  }, { type: "TEXT" }),

  get_styles_by_type: H.simple((p) => {
    const type = (p.style_type || "FILL").toUpperCase();
    let styles = [];
    if (type === "FILL" || type === "PAINT") styles = figma.getLocalPaintStyles();
    else if (type === "TEXT") styles = figma.getLocalTextStyles();
    else if (type === "EFFECT") styles = figma.getLocalEffectStyles();
    else if (type === "GRID") styles = figma.getLocalGridStyles();
    return { style_type: type, count: styles.length, styles: styles.map(s => ({ id: s.id, name: s.name, key: s.key })) };
  }),

  get_all_local_variables: H.simple(async () => {
    const variables = await figma.variables.getLocalVariablesAsync();
    const collections = await figma.variables.getLocalVariableCollectionsAsync();
    return {
      variables: variables.map(v => ({ id: v.id, name: v.name, resolvedType: v.resolvedType })),
      collections: collections.map(c => ({ id: c.id, name: c.name, modes: c.modes }))
    };
  }),

  list_components: H.simple(() => {
    const components = figma.currentPage.findAllWithCriteria({ types: ["COMPONENT"] });
    return { count: components.length, components: components.map(c => ({ id: c.id, name: c.name })) };
  }),

  find_all: H.simple((p) => {
    const types = p.types || ["FRAME", "COMPONENT", "INSTANCE"];
    const nodes = figma.currentPage.findAllWithCriteria({ types });
    return { count: nodes.length, nodes: nodes.slice(0, 100).map(n => ({ id: n.id, name: n.name, type: n.type })) };
  }),

  // === Create (11) ===
  create_frame: H.simple((p) => {
    const frame = figma.createFrame();
    frame.name = p.name || "Frame";
    frame.resize(p.width || 100, p.height || 100);
    if (p.x !== undefined) frame.x = p.x;
    if (p.y !== undefined) frame.y = p.y;
    return { node_id: frame.id, name: frame.name };
  }),

  create_rectangle: H.simple((p) => {
    const rect = figma.createRectangle();
    rect.name = p.name || "Rectangle";
    rect.resize(p.width || 100, p.height || 100);
    if (p.x !== undefined) rect.x = p.x;
    if (p.y !== undefined) rect.y = p.y;
    if (p.corner_radius) rect.cornerRadius = p.corner_radius;
    return { node_id: rect.id, name: rect.name };
  }),

  create_ellipse: H.simple((p) => {
    const ellipse = figma.createEllipse();
    ellipse.name = p.name || "Ellipse";
    ellipse.resize(p.width || 100, p.height || 100);
    if (p.x !== undefined) ellipse.x = p.x;
    if (p.y !== undefined) ellipse.y = p.y;
    return { node_id: ellipse.id, name: ellipse.name };
  }),

  create_text: H.simple(async (p) => {
    const text = figma.createText();
    await figma.loadFontAsync({ family: "Inter", style: "Regular" });
    text.characters = p.text || p.characters || "Text";
    text.name = p.name || "Text";
    if (p.x !== undefined) text.x = p.x;
    if (p.y !== undefined) text.y = p.y;
    if (p.font_size) text.fontSize = p.font_size;
    return { node_id: text.id, name: text.name };
  }),

  create_line: H.simple((p) => {
    const line = figma.createLine();
    line.name = p.name || "Line";
    if (p.length) line.resize(p.length, 0);
    if (p.x !== undefined) line.x = p.x;
    if (p.y !== undefined) line.y = p.y;
    return { node_id: line.id, name: line.name };
  }),

  create_polygon: H.simple((p) => {
    const polygon = figma.createPolygon();
    polygon.name = p.name || "Polygon";
    polygon.pointCount = p.point_count || p.sides || 3;
    polygon.resize(p.width || 100, p.height || 100);
    return { node_id: polygon.id, name: polygon.name };
  }),

  create_star: H.simple((p) => {
    const star = figma.createStar();
    star.name = p.name || "Star";
    star.pointCount = p.point_count || 5;
    star.resize(p.width || 100, p.height || 100);
    return { node_id: star.id, name: star.name };
  }),

  create_vector: H.simple((p) => {
    const vector = figma.createVector();
    vector.name = p.name || "Vector";
    if (p.width && p.height) vector.resize(p.width, p.height);
    return { node_id: vector.id, name: vector.name };
  }),

  create_component: H.node((node) => {
    const component = figma.createComponentFromNode(node);
    return { component_id: component.id, name: component.name };
  }),

  create_component_set: H.simple(async (p) => {
    const ids = p.component_ids || [];
    if (ids.length < 2) return { error: "At least 2 components required" };
    const components = [];
    for (const id of ids) {
      const node = await getNodeById(id);
      if (node && node.type === "COMPONENT") components.push(node);
    }
    if (components.length < 2) return { error: "At least 2 valid components required" };
    const set = figma.combineAsVariants(components, figma.currentPage);
    if (p.name) set.name = p.name;
    return { component_set_id: set.id, name: set.name };
  }),

  create_slice: H.simple((p) => {
    const slice = figma.createSlice();
    slice.name = p.name || "Slice";
    if (p.width && p.height) slice.resize(p.width, p.height);
    if (p.x !== undefined) slice.x = p.x;
    if (p.y !== undefined) slice.y = p.y;
    return { node_id: slice.id, name: slice.name };
  }),

  // === Edit (9) ===
  clone: H.nodeRequired(async (node) => {
    const cloned = node.clone();
    return { node_id: cloned.id, name: cloned.name };
  }),

  duplicate: H.nodeRequired((node) => {
    const dup = node.clone();
    dup.x += 20;
    dup.y += 20;
    return { node_id: dup.id, name: dup.name };
  }),

  delete_node: H.nodeRequired((node) => {
    const id = node.id;
    node.remove();
    return { deleted: id };
  }),

  group: H.nodes((nodes) => {
    const group = figma.group(nodes, figma.currentPage);
    return { group_id: group.id, count: nodes.length };
  }),

  ungroup: H.node((node) => {
    if (node.type !== "GROUP") return { error: "Node is not a group" };
    const children = node.children.slice();
    const parent = node.parent;
    for (const child of children) parent.appendChild(child);
    node.remove();
    return { ungrouped: children.length };
  }),

  flatten: H.nodes((nodes) => {
    const flat = figma.flatten(nodes);
    return { node_id: flat.id };
  }),

  detach_instance: H.node((node) => {
    const detached = node.detachInstance();
    return { node_id: detached.id, name: detached.name };
  }, { type: "INSTANCE" }),

  set_parent: H.simple(async (p) => {
    const nodeId = p.node_id || p.nodeId;
    const parentId = p.parent_id || p.parentId;
    const node = await getNodeById(nodeId);
    const parent = await getNodeById(parentId);
    if (!node) return { error: "Node not found" };
    if (!parent || !("children" in parent)) return { error: "Invalid parent" };
    parent.appendChild(node);
    return { node_id: node.id, new_parent_id: parent.id };
  }),

  insert_child: H.simple(async (p) => {
    const nodeId = p.node_id || p.nodeId;
    const parentId = p.parent_id || p.parentId;
    const index = p.index || 0;
    const node = await getNodeById(nodeId);
    const parent = await getNodeById(parentId);
    if (!node || !parent) return { error: "Node or parent not found" };
    parent.insertChild(index, node);
    return { node_id: node.id, parent_id: parent.id, index };
  }),

  // === Transform (7) ===
  resize: H.node((node, p) => {
    const w = p.width !== undefined ? p.width : node.width;
    const h = p.height !== undefined ? p.height : node.height;
    node.resize(w, h);
    return { node_id: node.id, width: w, height: h };
  }),

  move: H.node((node, p) => {
    if (p.x !== undefined) node.x = p.x;
    if (p.y !== undefined) node.y = p.y;
    return { node_id: node.id, x: node.x, y: node.y };
  }),

  rotate: H.node((node, p) => {
    const angle = p.angle || 0;
    node.rotation = angle;
    return { node_id: node.id, rotation: angle };
  }),

  flip: H.node((node, p) => {
    const dir = p.flip_direction || p.direction || "horizontal";
    if (dir === "horizontal") node.rescale(-1, 1);
    else node.rescale(1, -1);
    return { node_id: node.id, flipped: dir };
  }),

  reorder: H.node((node, p) => {
    const index = p.index || 0;
    const parent = node.parent;
    if (parent && "insertChild" in parent) parent.insertChild(index, node);
    return { node_id: node.id, index };
  }),

  resize_to_fit: H.node((node, p) => {
    if ("layoutMode" in node && node.layoutMode !== "NONE") {
      node.primaryAxisSizingMode = "AUTO";
      node.counterAxisSizingMode = "AUTO";
    }
    return { node_id: node.id, width: node.width, height: node.height };
  }),

  scroll_and_zoom: H.simple((p) => {
    if (p.x !== undefined && p.y !== undefined) figma.viewport.center = { x: p.x, y: p.y };
    if (p.zoom !== undefined) figma.viewport.zoom = p.zoom;
    return { center: figma.viewport.center, zoom: figma.viewport.zoom };
  }),

  // === Boolean (4) ===
  boolean_union: H.nodes((nodes) => {
    const result = figma.union(nodes, figma.currentPage);
    return { node_id: result.id };
  }),

  boolean_subtract: H.nodes((nodes) => {
    const result = figma.subtract(nodes, figma.currentPage);
    return { node_id: result.id };
  }),

  boolean_intersect: H.nodes((nodes) => {
    const result = figma.intersect(nodes, figma.currentPage);
    return { node_id: result.id };
  }),

  boolean_exclude: H.nodes((nodes) => {
    const result = figma.exclude(nodes, figma.currentPage);
    return { node_id: result.id };
  }),

  // === Align (2) ===
  align: H.simple((p) => {
    const sel = figma.currentPage.selection;
    if (sel.length < 2) return { error: "Select at least 2 nodes" };
    const dir = (p.direction || "LEFT").toUpperCase();
    // Simplified alignment
    let target;
    if (dir === "LEFT") target = Math.min.apply(null, sel.map(function(n) { return n.x; }));
    else if (dir === "RIGHT") target = Math.max.apply(null, sel.map(function(n) { return n.x + n.width; }));
    else if (dir === "TOP") target = Math.min.apply(null, sel.map(function(n) { return n.y; }));
    else if (dir === "BOTTOM") target = Math.max.apply(null, sel.map(function(n) { return n.y + n.height; }));
    else if (dir === "CENTER_H") target = sel.reduce((s, n) => s + n.x + n.width / 2, 0) / sel.length;
    else if (dir === "CENTER_V") target = sel.reduce((s, n) => s + n.y + n.height / 2, 0) / sel.length;

    for (const node of sel) {
      if (dir === "LEFT") node.x = target;
      else if (dir === "RIGHT") node.x = target - node.width;
      else if (dir === "TOP") node.y = target;
      else if (dir === "BOTTOM") node.y = target - node.height;
      else if (dir === "CENTER_H") node.x = target - node.width / 2;
      else if (dir === "CENTER_V") node.y = target - node.height / 2;
    }
    return { aligned: sel.length, direction: dir };
  }),

  distribute: H.simple((p) => {
    const sel = figma.currentPage.selection;
    if (sel.length < 3) return { error: "Select at least 3 nodes" };
    const axis = (p.axis || "horizontal").toLowerCase();
    const sorted = sel.slice().sort(function(a, b) { return axis === "horizontal" ? a.x - b.x : a.y - b.y; });
    const first = sorted[0], last = sorted[sorted.length - 1];
    const total = axis === "horizontal"
      ? (last.x + last.width) - first.x - sorted.reduce((s, n) => s + n.width, 0)
      : (last.y + last.height) - first.y - sorted.reduce((s, n) => s + n.height, 0);
    const gap = total / (sorted.length - 1);
    let pos = axis === "horizontal" ? first.x + first.width : first.y + first.height;
    for (let i = 1; i < sorted.length - 1; i++) {
      pos += gap;
      if (axis === "horizontal") sorted[i].x = pos;
      else sorted[i].y = pos;
      pos += axis === "horizontal" ? sorted[i].width : sorted[i].height;
    }
    return { distributed: sel.length, axis };
  }),

  // === Style (21) ===
  set_fill: H.node((node, p) => {
    const color = parseColor(p.color);
    const opacity = p.opacity !== undefined ? p.opacity : 1;
    node.fills = [{ type: "SOLID", color, opacity }];
    return { node_id: node.id, fill: color };
  }),

  set_stroke: H.node((node, p) => {
    const color = parseColor(p.color);
    const weight = p.weight || p.stroke_weight || 1;
    node.strokes = [{ type: "SOLID", color }];
    node.strokeWeight = weight;
    return { node_id: node.id, stroke: color, weight };
  }),

  set_stroke_weight: H.node((node, p) => {
    node.strokeWeight = p.weight || p.stroke_weight || 1;
    return { node_id: node.id, stroke_weight: node.strokeWeight };
  }),

  set_opacity: H.node((node, p) => {
    node.opacity = p.opacity !== undefined ? p.opacity : 1;
    return { node_id: node.id, opacity: node.opacity };
  }),

  set_corner_radius: H.node((node, p) => {
    const radius = p.radius || p.corner_radius || 0;
    node.cornerRadius = radius;
    return { node_id: node.id, corner_radius: radius };
  }),

  set_effects: H.node((node, p) => {
    const effects = p.effects || [];
    node.effects = effects;
    return { node_id: node.id, effects: node.effects };
  }),

  set_blend_mode: H.node((node, p) => {
    const mode = p.blend_mode || "NORMAL";
    node.blendMode = mode;
    return { node_id: node.id, blend_mode: mode };
  }),

  set_constraints: H.node((node, p) => {
    const h = p.constraint_horizontal || p.horizontal;
    const v = p.constraint_vertical || p.vertical;
    const current = node.constraints;
    node.constraints = { horizontal: h || current.horizontal, vertical: v || current.vertical };
    return { node_id: node.id, constraints: node.constraints };
  }),

  outline_stroke: H.node((node) => {
    const outlined = node.outlineStroke();
    return { node_id: outlined ? outlined.id : null };
  }),

  swap_fill_stroke: H.node((node) => {
    const fills = node.fills;
    const strokes = node.strokes;
    node.fills = strokes;
    node.strokes = fills;
    return { node_id: node.id };
  }),

  copy_style: H.simple(async (p) => {
    const sourceId = p.source_id || p.sourceId;
    const targetId = p.target_id || p.targetId;
    const source = await getNodeById(sourceId);
    const target = await getNodeById(targetId);
    if (!source || !target) return { error: "Source or target not found" };
    if ("fills" in source && "fills" in target) target.fills = source.fills;
    if ("strokes" in source && "strokes" in target) target.strokes = source.strokes;
    if ("effects" in source && "effects" in target) target.effects = source.effects;
    if ("opacity" in source && "opacity" in target) target.opacity = source.opacity;
    return { source_id: sourceId, target_id: targetId };
  }),

  set_image_fill: H.node(async (node, p) => {
    let imageHash = p.image_hash || p.imageHash;
    if (!imageHash && p.base64) {
      const binary = atob(p.base64);
      const bytes = new Uint8Array(binary.length);
      for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
      const image = figma.createImage(bytes);
      imageHash = image.hash;
    }
    if (!imageHash) return { error: "No image_hash or base64 provided" };
    node.fills = [{ type: "IMAGE", imageHash, scaleMode: p.scale_mode || "FILL" }];
    return { node_id: node.id, image_hash: imageHash };
  }),

  apply_style: H.node((node, p) => {
    const styleId = p.style_id || p.styleId;
    const type = (p.style_type || "FILL").toUpperCase();
    if (type === "FILL" && "fillStyleId" in node) node.fillStyleId = styleId;
    else if (type === "STROKE" && "strokeStyleId" in node) node.strokeStyleId = styleId;
    else if (type === "TEXT" && "textStyleId" in node) node.textStyleId = styleId;
    else if (type === "EFFECT" && "effectStyleId" in node) node.effectStyleId = styleId;
    return { node_id: node.id, style_id: styleId, style_type: type };
  }),

  set_plugin_data: H.node((node, p) => {
    const key = p.key || p.data_key;
    const value = p.value || p.data_value || "";
    if (!key) return { error: "key required" };
    node.setPluginData(key, value);
    return { node_id: node.id, key, value };
  }),

  set_shared_plugin_data: H.node((node, p) => {
    const ns = p.namespace || "shared";
    const key = p.key || p.data_key;
    const value = p.value || p.data_value || "";
    if (!key) return { error: "key required" };
    node.setSharedPluginData(ns, key, value);
    return { node_id: node.id, namespace: ns, key, value };
  }),

  rename: H.node((node, p) => {
    node.name = p.name || node.name;
    return { node_id: node.id, name: node.name };
  }),

  set_locked: H.node((node, p) => {
    node.locked = p.locked !== undefined ? p.locked : true;
    return { node_id: node.id, locked: node.locked };
  }),

  set_visible: H.node((node, p) => {
    node.visible = p.visible !== undefined ? p.visible : true;
    return { node_id: node.id, visible: node.visible };
  }),

  set_selection: H.simple(async (p) => {
    const ids = p.node_ids || p.nodeIds || [];
    const nodes = [];
    for (const id of ids) {
      const node = await getNodeById(id);
      if (node) nodes.push(node);
    }
    figma.currentPage.selection = nodes;
    return { selected: nodes.length };
  }),

  zoom_to: H.node((node) => {
    figma.viewport.scrollAndZoomIntoView([node]);
    return { node_id: node.id };
  }),

  // === Text (5) ===
  set_text: H.node(async (node, p) => {
    await figma.loadFontAsync(node.fontName);
    node.characters = p.text || p.characters || "";
    return { node_id: node.id, text: node.characters };
  }, { type: "TEXT" }),

  set_text_case: H.node(async (node, p) => {
    await figma.loadFontAsync(node.fontName);
    node.textCase = p.text_case || "ORIGINAL";
    return { node_id: node.id, text_case: node.textCase };
  }, { type: "TEXT" }),

  set_range_fills: H.node(async (node, p) => {
    const start = p.start || 0;
    const end = p.end !== undefined ? p.end : node.characters.length;
    const color = parseColor(p.color);
    await figma.loadFontAsync(node.getRangeFontName(start, end));
    node.setRangeFills(start, end, [{ type: "SOLID", color }]);
    return { node_id: node.id, start, end };
  }, { type: "TEXT" }),

  set_range_font_size: H.node(async (node, p) => {
    const start = p.start || 0;
    const end = p.end !== undefined ? p.end : node.characters.length;
    const size = p.font_size || p.fontSize;
    if (!size) return { error: "font_size required" };
    await figma.loadFontAsync(node.getRangeFontName(start, end));
    node.setRangeFontSize(start, end, size);
    return { node_id: node.id, start, end, font_size: size };
  }, { type: "TEXT" }),

  // === Layout (4) ===
  set_auto_layout: H.node((node, p) => {
    const mode = (p.layout_mode || p.layoutMode || "HORIZONTAL").toUpperCase();
    node.layoutMode = mode;
    if (p.padding !== undefined) {
      node.paddingTop = node.paddingBottom = node.paddingLeft = node.paddingRight = p.padding;
    }
    if (p.item_spacing !== undefined || p.itemSpacing !== undefined) {
      node.itemSpacing = p.item_spacing || p.itemSpacing;
    }
    return { node_id: node.id, layout_mode: mode };
  }),

  remove_auto_layout: H.node((node) => {
    node.layoutMode = "NONE";
    return { node_id: node.id, layout_mode: "NONE" };
  }),

  set_grid: H.node((node, p) => {
    const pattern = (p.pattern || "GRID").toUpperCase();
    const grid = { pattern, visible: p.visible !== false, color: { r: 1, g: 0, b: 0, a: 0.1 } };
    if (pattern === "COLUMNS" || pattern === "ROWS") {
      grid.count = p.count || 12;
      grid.gutterSize = p.gutter || 20;
      grid.alignment = p.alignment || "STRETCH";
    } else {
      grid.sectionSize = p.size || 10;
    }
    node.layoutGrids = p.append ? node.layoutGrids.concat([grid]) : [grid];
    return { node_id: node.id, grids: node.layoutGrids };
  }),

  set_viewport: H.simple((p) => {
    if (p.x !== undefined && p.y !== undefined) figma.viewport.center = { x: p.x, y: p.y };
    if (p.zoom !== undefined) figma.viewport.zoom = p.zoom;
    return { center: figma.viewport.center, zoom: figma.viewport.zoom };
  }),

  // === Component (4) ===
  swap_component: H.node(async (node, p) => {
    const compId = p.component_id || p.componentId;
    const comp = await getNodeById(compId);
    if (!comp || comp.type !== "COMPONENT") return { error: "Component not found" };
    node.swapComponent(comp);
    return { node_id: node.id, new_component_id: compId };
  }, { type: "INSTANCE" }),

  reset_overrides: H.node((node) => {
    node.resetOverrides();
    return { node_id: node.id, reset: true };
  }, { type: "INSTANCE" }),

  // === Export (2) ===
  export_image: H.node(async (node, p) => {
    const format = (p.format || "PNG").toUpperCase();
    const scale = p.scale || 1;
    const bytes = await node.exportAsync({ format, constraint: { type: "SCALE", value: scale } });
    let binary = "";
    for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
    return { node_id: node.id, format, size: bytes.length, base64: btoa(binary) };
  }),

  rasterize: H.node(async (node, p) => {
    const format = (p.format || "PNG").toUpperCase();
    const scale = p.scale || 1;
    const bytes = await node.exportAsync({ format, constraint: { type: "SCALE", value: scale } });
    let binary = "";
    for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
    return { node_id: node.id, format, scale, size: bytes.length, base64: btoa(binary) };
  }),

  set_export_settings: H.node((node, p) => {
    const format = (p.format || p.export_format || "PNG").toUpperCase();
    const scale = p.scale || 1;
    const settings = [{ format, suffix: p.suffix || "", constraint: { type: "SCALE", value: scale } }];
    node.exportSettings = p.append ? node.exportSettings.concat(settings) : settings;
    return { node_id: node.id, export_settings: node.exportSettings };
  }),

  // === Prototype (2) ===
  set_reactions: H.node(async (node, p) => {
    const targetId = p.target_id || p.targetId;
    if (!targetId) return { error: "target_id required" };
    const trigger = p.trigger || "ON_CLICK";
    node.reactions = [{
      trigger: { type: trigger },
      actions: [{ type: "NODE", destinationId: targetId, navigation: p.navigation || "NAVIGATE" }]
    }];
    return { node_id: node.id, target_id: targetId };
  }),

  // === Layer (4) ===
  bring_to_front: H.node((node) => {
    const parent = node.parent;
    if (parent && "insertChild" in parent) {
      parent.insertChild(parent.children.length - 1, node);
    }
    return { node_id: node.id };
  }),

  send_to_back: H.node((node) => {
    const parent = node.parent;
    if (parent && "insertChild" in parent) {
      parent.insertChild(0, node);
    }
    return { node_id: node.id };
  }),

  collapse_layer: H.node((node, p) => {
    if (!("expanded" in node)) return { error: "Node does not support collapse" };
    node.expanded = p.expand !== undefined ? p.expand : false;
    return { node_id: node.id, expanded: node.expanded };
  }),

  // === Misc ===
  notify: H.simple((p) => {
    figma.notify(p.message || "Notification", { timeout: p.timeout || 2000 });
    return { notified: true };
  }),
};

// ============== Command Dispatcher ==============

async function handleCommand(command) {
  const handler = handlers[command.name];

  if (!handler) {
    return { ok: false, payload: { error: "Unknown command: " + command.name } };
  }

  try {
    const result = await handler(command.payload || {});
    return result;
  } catch (err) {
    return { ok: false, payload: { error: String(err) } };
  }
}

// ============== Message Handler ==============

// Helper: Get selection data for inspector
function getSelectionData() {
  var sel = figma.currentPage.selection;
  if (sel.length === 0) return { count: 0, nodes: [] };
  return {
    count: sel.length,
    nodes: sel.slice(0, 5).map(function(n) {
      var data = {
        id: n.id,
        name: n.name,
        type: n.type,
        x: n.x,
        y: n.y,
        width: n.width,
        height: n.height
      };
      if ("opacity" in n) data.opacity = n.opacity;
      if ("fills" in n && n.fills !== MIXED) data.fills = n.fills.length;
      return data;
    })
  };
}

// Helper: Extract design tokens (colors from styles)
function extractTokens() {
  var colors = [];
  try {
    var paintStyles = figma.getLocalPaintStyles();
    for (var i = 0; i < paintStyles.length; i++) {
      var style = paintStyles[i];
      if (style.paints && style.paints.length > 0) {
        var paint = style.paints[0];
        if (paint.type === "SOLID") {
          var c = paint.color;
          var hex = "#" + Math.round(c.r * 255).toString(16).padStart(2, "0") +
                         Math.round(c.g * 255).toString(16).padStart(2, "0") +
                         Math.round(c.b * 255).toString(16).padStart(2, "0");
          colors.push(hex);
        }
      }
    }
  } catch (e) {}
  return colors;
}

// Selection change listener
figma.on("selectionchange", function() {
  figma.ui.postMessage({ type: "selection_update", selection: getSelectionData() });
});

figma.ui.onmessage = async (msg) => {
  // Handle selection request
  if (msg.type === "request_selection") {
    figma.ui.postMessage({ type: "selection_update", selection: getSelectionData() });
    figma.ui.postMessage({ type: "tokens_update", tokens: extractTokens() });
    return;
  }

  if (msg.type === "command") {
    const command = msg.command;
    const result = await handleCommand(command);

    figma.ui.postMessage({
      type: "command_result",
      command_id: command.id,
      action: command.name,
      ok: result.ok,
      payload_json: (() => {
        try {
          const json = safeStringify(result.payload);
          if (json && json.length > MAX_PAYLOAD_CHARS) {
            return safeStringify({ error: "Payload too large", size: json.length });
          }
          return json;
        } catch (e) {
          return safeStringify({ error: "Serialize error: " + String(e) });
        }
      })()
    });
  }
};
