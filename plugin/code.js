figma.showUI(__html__, { width: 360, height: 260 });

const MIXED = figma.mixed;
const MAX_PAYLOAD_CHARS = 2000000;

function normalizeNodeId(value) {
  if (!value || typeof value !== "string") return value;
  if (value.includes(":")) return value;
  if (value.includes("-")) return value.replace(/-/g, ":");
  return value;
}

async function getNodeById(nodeId) {
  if (!nodeId) return null;
  const normalized = normalizeNodeId(nodeId);
  if (typeof figma.getNodeByIdAsync === "function") {
    return await figma.getNodeByIdAsync(normalized);
  }
  return figma.getNodeById(normalized);
}

function serializeColor(color) {
  if (!color || typeof color !== "object") return null;
  return {
    r: color.r,
    g: color.g,
    b: color.b,
    a: typeof color.a === "number" ? color.a : 1
  };
}

function serializePaint(paint) {
  if (!paint || typeof paint !== "object") return null;
  const base = {
    type: paint.type,
    visible: paint.visible !== false,
    opacity: typeof paint.opacity === "number" ? paint.opacity : 1
  };
  if (paint.type === "SOLID") {
    base.color = serializeColor(paint.color);
  }
  if (paint.type && paint.type.startsWith("GRADIENT")) {
    base.gradientStops = paint.gradientStops || [];
    base.gradientTransform = paint.gradientTransform || null;
  }
  if (paint.type === "IMAGE") {
    base.scaleMode = paint.scaleMode;
    base.imageTransform = paint.imageTransform || null;
    base.imageHash = paint.imageHash || null;
  }
  return base;
}

function safeStringify(value) {
  const seen = new WeakSet();
  return JSON.stringify(value, (key, val) => {
    if (val === MIXED) return { mixed: true };
    if (typeof val === "bigint") return Number(val);
    if (typeof val === "symbol") return String(val);
    if (typeof val === "function") return undefined;
    if (val && typeof val === "object") {
      if (seen.has(val)) return "[Circular]";
      seen.add(val);
    }
    return val;
  });
}

function serializePaints(paints) {
  if (paints === MIXED) return { mixed: true };
  if (!Array.isArray(paints)) return [];
  return paints.map(serializePaint).filter(Boolean);
}

function serializeEffect(effect) {
  if (!effect || typeof effect !== "object") return null;
  const base = {
    type: effect.type,
    visible: effect.visible !== false,
    radius: effect.radius
  };
  if (effect.color) base.color = serializeColor(effect.color);
  if (typeof effect.offset === "object") base.offset = effect.offset;
  if (typeof effect.spread === "number") base.spread = effect.spread;
  if (effect.blendMode) base.blendMode = effect.blendMode;
  return base;
}

function serializeEffects(effects) {
  if (effects === MIXED) return { mixed: true };
  if (!Array.isArray(effects)) return [];
  return effects.map(serializeEffect).filter(Boolean);
}

function base64Encode(bytes) {
  if (typeof figma.base64Encode === "function") {
    return figma.base64Encode(bytes);
  }
  let binary = "";
  for (const b of bytes) {
    binary += String.fromCharCode(b);
  }
  return btoa(binary);
}

function normalizeExportFormat(value) {
  if (!value) return "PNG";
  const upper = String(value).toUpperCase();
  if (upper === "JPG" || upper === "JPEG") return "JPG";
  if (upper === "PNG" || upper === "SVG" || upper === "PDF") return upper;
  return null;
}

function mimeForFormat(format) {
  switch (format) {
    case "JPG":
      return "image/jpeg";
    case "SVG":
      return "image/svg+xml";
    case "PDF":
      return "application/pdf";
    default:
      return "image/png";
  }
}

async function exportNodeImage(node, payload) {
  if (!node || typeof node.exportAsync !== "function") {
    return { error: "Node is not exportable" };
  }
  const format = normalizeExportFormat(payload && payload.format);
  if (!format) return { error: "Unsupported format" };
  const scale = payload && typeof payload.scale === "number" ? payload.scale : 1;
  const constraint = { type: "SCALE", value: Math.max(0.1, scale) };
  const bytes = await node.exportAsync({ format, constraint });
  return {
    node_id: node.id,
    format: format.toLowerCase(),
    scale,
    bytes: bytes.length,
    mime: mimeForFormat(format),
    base64: base64Encode(bytes)
  };
}

function serializeTextNode(node) {
  const text = {
    characters: node.characters,
    textAlignHorizontal: node.textAlignHorizontal,
    textAlignVertical: node.textAlignVertical
  };
  if (node.fontName === MIXED) {
    text.fontName = { mixed: true };
  } else {
    text.fontName = node.fontName;
  }
  if (typeof node.fontSize === "number") text.fontSize = node.fontSize;
  if (node.lineHeight !== MIXED) text.lineHeight = node.lineHeight;
  if (node.letterSpacing !== MIXED) text.letterSpacing = node.letterSpacing;
  if (node.textCase !== MIXED) text.textCase = node.textCase;
  if (node.textDecoration !== MIXED) text.textDecoration = node.textDecoration;
  const segments = serializeTextSegments(node);
  if (segments) text.segments = segments;
  return text;
}

function safeRangeBounds(node, start, end) {
  try {
    if (typeof node.getRangeBoundingBox === "function") {
      return node.getRangeBoundingBox(start, end);
    }
    if (typeof node.getRangeBounds === "function") {
      return node.getRangeBounds(start, end);
    }
  } catch (err) {
    return { error: String(err) };
  }
  return null;
}

function serializeSegmentValue(value) {
  if (value === MIXED) return { mixed: true };
  return value;
}

function serializeTextSegments(node) {
  if (typeof node.getStyledTextSegments !== "function") return null;
  try {
    const segments = node.getStyledTextSegments([
      "fontName",
      "fontSize",
      "fontWeight",
      "textDecoration",
      "textCase",
      "lineHeight",
      "letterSpacing",
      "fills",
      "textStyleId",
      "paragraphSpacing",
      "paragraphIndent"
    ]);
    return segments.map(seg => {
      let fills = null;
      if (seg.fills === MIXED) {
        fills = { mixed: true };
      } else if (Array.isArray(seg.fills)) {
        fills = serializePaints(seg.fills);
      }
      const bounds = safeRangeBounds(node, seg.start, seg.end);
      return {
        start: seg.start,
        end: seg.end,
        characters: seg.characters,
        fontName: serializeSegmentValue(seg.fontName),
        fontSize: serializeSegmentValue(seg.fontSize),
        fontWeight: serializeSegmentValue(seg.fontWeight),
        textDecoration: serializeSegmentValue(seg.textDecoration),
        textCase: serializeSegmentValue(seg.textCase),
        lineHeight: serializeSegmentValue(seg.lineHeight),
        letterSpacing: serializeSegmentValue(seg.letterSpacing),
        paragraphSpacing: serializeSegmentValue(seg.paragraphSpacing),
        paragraphIndent: serializeSegmentValue(seg.paragraphIndent),
        textStyleId: serializeSegmentValue(seg.textStyleId),
        fills,
        bounds
      };
    });
  } catch (err) {
    return { error: String(err) };
  }
}

async function serializeVariables() {
  if (!figma.variables || typeof figma.variables.getLocalVariablesAsync !== "function") {
    return { error: "Variables API not available" };
  }
  const [collections, variables] = await Promise.all([
    figma.variables.getLocalVariableCollectionsAsync(),
    figma.variables.getLocalVariablesAsync()
  ]);

  const collectionMap = {};
  for (const col of collections) {
    collectionMap[col.id] = {
      id: col.id,
      name: col.name,
      modes: col.modes,
      defaultModeId: col.defaultModeId,
      remote: col.remote
    };
  }

  const variablesMap = {};
  for (const v of variables) {
    variablesMap[v.id] = {
      id: v.id,
      name: v.name,
      resolvedType: v.resolvedType,
      variableCollectionId: v.variableCollectionId,
      valuesByMode: v.valuesByMode,
      scopes: v.scopes,
      description: v.description || null,
      remote: v.remote
    };
  }

  return { collections: collectionMap, variables: variablesMap };
}

function serializeLayout(node) {
  if (!("layoutMode" in node)) return null;
  return {
    layoutMode: node.layoutMode,
    primaryAxisSizingMode: node.primaryAxisSizingMode,
    counterAxisSizingMode: node.counterAxisSizingMode,
    primaryAxisAlignItems: node.primaryAxisAlignItems,
    counterAxisAlignItems: node.counterAxisAlignItems,
    layoutWrap: node.layoutWrap,
    paddingLeft: node.paddingLeft,
    paddingRight: node.paddingRight,
    paddingTop: node.paddingTop,
    paddingBottom: node.paddingBottom,
    itemSpacing: node.itemSpacing,
    layoutGrids: node.layoutGrids || null,
    strokesIncludedInLayout: node.strokesIncludedInLayout,
    clipsContent: node.clipsContent
  };
}

function serializeNode(node, depth, maxDepth, options) {
  const includeGeometry = !(options && options.includeGeometry === false);
  const base = {
    id: node.id,
    name: node.name,
    type: node.type,
    visible: node.visible !== false,
    locked: node.locked === true
  };

  if ("x" in node) base.x = node.x;
  if ("y" in node) base.y = node.y;
  if ("width" in node) base.width = node.width;
  if ("height" in node) base.height = node.height;
  if ("rotation" in node) base.rotation = node.rotation;
  if ("opacity" in node) base.opacity = node.opacity;
  if ("blendMode" in node) base.blendMode = node.blendMode;
  if ("clipsContent" in node) base.clipsContent = node.clipsContent;
  if ("isMask" in node) base.isMask = node.isMask;

  if (node.absoluteRenderBounds) base.absoluteRenderBounds = node.absoluteRenderBounds;
  if (node.absoluteBoundingBox) base.absoluteBoundingBox = node.absoluteBoundingBox;
  if (node.absoluteTransform) base.absoluteTransform = node.absoluteTransform;

  if ("fills" in node) base.fills = serializePaints(node.fills);
  if ("strokes" in node) base.strokes = serializePaints(node.strokes);
  if ("strokeWeight" in node) base.strokeWeight = node.strokeWeight;
  if ("strokeAlign" in node) base.strokeAlign = node.strokeAlign;
  if ("strokeCap" in node) base.strokeCap = node.strokeCap;
  if ("strokeJoin" in node) base.strokeJoin = node.strokeJoin;
  if (includeGeometry) {
    if ("strokeGeometry" in node) base.strokeGeometry = node.strokeGeometry;
    if ("fillGeometry" in node) base.fillGeometry = node.fillGeometry;
    if ("vectorPaths" in node) base.vectorPaths = node.vectorPaths;
    if ("vectorNetwork" in node) base.vectorNetwork = node.vectorNetwork;
  }

  if ("effects" in node) base.effects = serializeEffects(node.effects);

  if ("cornerRadius" in node) base.cornerRadius = node.cornerRadius;
  if ("topLeftRadius" in node || "topRightRadius" in node || "bottomRightRadius" in node || "bottomLeftRadius" in node) {
    base.cornerRadii = {
      topLeftRadius: node.topLeftRadius,
      topRightRadius: node.topRightRadius,
      bottomRightRadius: node.bottomRightRadius,
      bottomLeftRadius: node.bottomLeftRadius
    };
  }

  if ("layoutAlign" in node) base.layoutAlign = node.layoutAlign;
  if ("layoutGrow" in node) base.layoutGrow = node.layoutGrow;
  if ("layoutPositioning" in node) base.layoutPositioning = node.layoutPositioning;
  if ("constraints" in node) base.constraints = node.constraints;

  const layout = serializeLayout(node);
  if (layout) base.layout = layout;

  if (node.type === "TEXT") {
    base.text = serializeTextNode(node);
  }

  if ("componentProperties" in node) base.componentProperties = node.componentProperties;
  if ("variantProperties" in node) base.variantProperties = node.variantProperties;

  if (depth < maxDepth && "children" in node && Array.isArray(node.children)) {
    base.children = node.children.map(child => serializeNode(child, depth + 1, maxDepth, options));
  }

  return base;
}

async function applyTextProps(node, props) {
  let fontName = null;
  if (props.fontName && typeof props.fontName === "object") {
    fontName = props.fontName;
  } else if (node.fontName !== MIXED) {
    fontName = node.fontName;
  }
  if (fontName) {
    try {
      await figma.loadFontAsync(fontName);
      node.fontName = fontName;
    } catch (err) {
      // Ignore font errors to keep ops resilient.
    }
  }
  if (typeof props.characters === "string") node.characters = props.characters;
  if (typeof props.fontSize === "number") node.fontSize = props.fontSize;
  if (props.textAlignHorizontal) node.textAlignHorizontal = props.textAlignHorizontal;
  if (props.textAlignVertical) node.textAlignVertical = props.textAlignVertical;
  if (props.lineHeight && props.lineHeight !== MIXED) node.lineHeight = props.lineHeight;
  if (props.letterSpacing && props.letterSpacing !== MIXED) node.letterSpacing = props.letterSpacing;
  if (props.textCase && props.textCase !== MIXED) node.textCase = props.textCase;
  if (props.textDecoration && props.textDecoration !== MIXED) node.textDecoration = props.textDecoration;
}

// Extract props from op object, supporting both direct fields and nested props
function extractProps(op) {
  const reserved = ['action', 'type', 'node_type', 'nodeType', 'parent_id', 'parentId', 'node_id', 'nodeId', 'props'];
  const props = {};
  for (const key of Object.keys(op)) {
    if (!reserved.includes(key)) {
      props[key] = op[key];
    }
  }
  // Merge nested props (higher priority)
  if (op.props && typeof op.props === 'object') {
    Object.assign(props, op.props);
  }
  return props;
}

async function applyProps(node, props) {
  if (!props || typeof props !== "object") return;

  if (typeof props.name === "string") node.name = props.name;
  if (typeof props.x === "number") node.x = props.x;
  if (typeof props.y === "number") node.y = props.y;
  if (typeof props.width === "number") node.resize(props.width, node.height);
  if (typeof props.height === "number") node.resize(node.width, props.height);
  if (typeof props.rotation === "number") node.rotation = props.rotation;
  if (typeof props.opacity === "number") node.opacity = props.opacity;
  if (props.blendMode) node.blendMode = props.blendMode;

  if ("fills" in props && "fills" in node) node.fills = props.fills;
  if ("strokes" in props && "strokes" in node) node.strokes = props.strokes;
  if ("strokeWeight" in props && "strokeWeight" in node) node.strokeWeight = props.strokeWeight;
  if ("strokeAlign" in props && "strokeAlign" in node) node.strokeAlign = props.strokeAlign;

  if ("effects" in props && "effects" in node) node.effects = props.effects;

  // Vector drawing support (SVG path syntax)
  if ("vectorPaths" in props && "vectorPaths" in node) node.vectorPaths = props.vectorPaths;

  if ("cornerRadius" in props && "cornerRadius" in node) node.cornerRadius = props.cornerRadius;

  if ("layoutMode" in props && "layoutMode" in node) node.layoutMode = props.layoutMode;
  if ("primaryAxisSizingMode" in props && "primaryAxisSizingMode" in node) node.primaryAxisSizingMode = props.primaryAxisSizingMode;
  if ("counterAxisSizingMode" in props && "counterAxisSizingMode" in node) node.counterAxisSizingMode = props.counterAxisSizingMode;
  if ("primaryAxisAlignItems" in props && "primaryAxisAlignItems" in node) node.primaryAxisAlignItems = props.primaryAxisAlignItems;
  if ("counterAxisAlignItems" in props && "counterAxisAlignItems" in node) node.counterAxisAlignItems = props.counterAxisAlignItems;
  if ("itemSpacing" in props && "itemSpacing" in node) node.itemSpacing = props.itemSpacing;
  if ("paddingLeft" in props && "paddingLeft" in node) node.paddingLeft = props.paddingLeft;
  if ("paddingRight" in props && "paddingRight" in node) node.paddingRight = props.paddingRight;
  if ("paddingTop" in props && "paddingTop" in node) node.paddingTop = props.paddingTop;
  if ("paddingBottom" in props && "paddingBottom" in node) node.paddingBottom = props.paddingBottom;

  if (node.type === "TEXT") {
    await applyTextProps(node, props);
  }
}

function createNode(nodeType) {
  switch (nodeType) {
    case "FRAME":
      return figma.createFrame();
    case "RECTANGLE":
      return figma.createRectangle();
    case "ELLIPSE":
      return figma.createEllipse();
    case "LINE":
      return figma.createLine();
    case "TEXT":
      return figma.createText();
    case "VECTOR":
      return figma.createVector();
    case "STAR":
      return figma.createStar();
    case "COMPONENT":
      return figma.createComponent();
    default:
      return figma.createFrame();
  }
}

async function applyOps(ops) {
  const results = [];
  if (!Array.isArray(ops)) {
    return { error: "ops must be an array" };
  }

  for (const op of ops) {
    if (!op || typeof op !== "object") continue;
    const action = op.action || op.type;
    try {
      if (action === "create") {
        const nodeType = op.node_type || op.nodeType || "FRAME";
        const parentId = op.parent_id || op.parentId;
        const parent = parentId ? await getNodeById(parentId) : figma.currentPage;
        if (!parent || !("appendChild" in parent)) {
          results.push({ action, status: "error", message: "Invalid parent" });
          continue;
        }
        const node = createNode(nodeType);
        parent.appendChild(node);
        await applyProps(node, extractProps(op));
        results.push({ action, status: "ok", node_id: node.id });
      } else if (action === "update") {
        const nodeId = op.node_id || op.nodeId;
        const node = nodeId ? await getNodeById(nodeId) : null;
        if (!node) {
          results.push({ action, status: "error", message: "Node not found" });
          continue;
        }
        await applyProps(node, extractProps(op));
        results.push({ action, status: "ok", node_id: node.id });
      } else if (action === "delete") {
        const nodeId = op.node_id || op.nodeId;
        const node = nodeId ? await getNodeById(nodeId) : null;
        if (!node) {
          results.push({ action, status: "error", message: "Node not found" });
          continue;
        }
        node.remove();
        results.push({ action, status: "ok", node_id: nodeId });
      } else if (action === "convert_to_component") {
        const nodeId = op.node_id || op.nodeId;
        const node = nodeId ? await getNodeById(nodeId) : null;
        if (!node) {
          results.push({ action, status: "error", message: "Node not found" });
          continue;
        }
        if (node.type !== "FRAME") {
          results.push({ action, status: "error", message: "Only FRAME can be converted to component" });
          continue;
        }
        const component = figma.createComponentFromNode(node);
        results.push({ action, status: "ok", node_id: component.id, old_node_id: nodeId });
      } else if (action === "create_instance") {
        // Create instance from existing component
        const componentId = op.component_id || op.componentId;
        const component = componentId ? await getNodeById(componentId) : null;
        if (!component) {
          results.push({ action, status: "error", message: "Component not found" });
          continue;
        }
        if (component.type !== "COMPONENT") {
          results.push({ action, status: "error", message: "Node is not a COMPONENT, got: " + component.type });
          continue;
        }
        const instance = component.createInstance();
        const props = op.props || {};
        if (typeof props.x === "number") instance.x = props.x;
        if (typeof props.y === "number") instance.y = props.y;
        if (typeof props.width === "number") instance.resize(props.width, instance.height);
        if (typeof props.height === "number") instance.resize(instance.width, props.height);
        if (props.name) instance.name = props.name;
        results.push({ action, status: "ok", node_id: instance.id, component_id: componentId });
      } else {
        results.push({ action, status: "error", message: "Unknown action" });
      }
    } catch (err) {
      results.push({ action, status: "error", message: String(err) });
    }
  }

  return { results };
}

figma.ui.onmessage = async (msg) => {
  if (!msg || msg.type !== "command") return;
  const command = msg.command;
  if (!command) return;

  let ok = true;
  let payload = null;

  try {
    if (command.name === "read_selection") {
      const depth = command.payload && typeof command.payload.depth === "number" ? command.payload.depth : 6;
      const nodes = figma.currentPage.selection.map(node => serializeNode(node, 0, depth));
      payload = {
        selectionCount: figma.currentPage.selection.length,
        nodes
      };
    } else if (command.name === "get_node") {
      const nodeId = command.payload && command.payload.node_id ? command.payload.node_id : null;
      const depth = command.payload && typeof command.payload.depth === "number" ? command.payload.depth : 6;
      const node = nodeId ? await getNodeById(nodeId) : null;
      const includeGeometry = command.payload && command.payload.include_geometry !== false;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else {
        payload = serializeNode(node, 0, depth, { includeGeometry });
      }
    } else if (command.name === "export_node_image") {
      const nodeId = command.payload && command.payload.node_id ? command.payload.node_id : null;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else {
        payload = await exportNodeImage(node, command.payload || {});
      }
    } else if (command.name === "get_variables") {
      payload = await serializeVariables();
    } else if (command.name === "apply_ops") {
      payload = await applyOps(command.payload && command.payload.ops ? command.payload.ops : []);
    } else if (command.name === "list_pages") {
      // List all pages in the document
      payload = {
        pages: figma.root.children.map(page => ({
          id: page.id,
          name: page.name,
          isCurrent: page.id === figma.currentPage.id
        })),
        currentPageId: figma.currentPage.id
      };
    } else if (command.name === "switch_page") {
      // Switch to a different page (async for dynamic-page mode)
      const pageId = command.payload && command.payload.page_id;
      const page = pageId ? figma.root.children.find(p => p.id === pageId) : null;
      if (!page) {
        ok = false;
        payload = { error: "Page not found" };
      } else {
        await figma.setCurrentPageAsync(page);
        payload = { page_id: page.id, page_name: page.name };
      }
    } else if (command.name === "list_components") {
      // List all local components in the file (with dynamic page loading)
      await figma.loadAllPagesAsync();
      const components = [];
      function findComponents(node) {
        if (node.type === "COMPONENT") {
          components.push({
            id: node.id,
            name: node.name,
            description: node.description || "",
            width: node.width,
            height: node.height,
            page: node.parent && node.parent.type === "PAGE" ? node.parent.name : null
          });
        }
        if ("children" in node) {
          for (const child of node.children) {
            findComponents(child);
          }
        }
      }
      for (const page of figma.root.children) {
        findComponents(page);
      }
      payload = { components, count: components.length };
    } else if (command.name === "clone") {
      // Clone a node
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else {
        const clone = node.clone();
        const offsetX = command.payload && typeof command.payload.offset_x === "number" ? command.payload.offset_x : 20;
        const offsetY = command.payload && typeof command.payload.offset_y === "number" ? command.payload.offset_y : 20;
        clone.x = node.x + offsetX;
        clone.y = node.y + offsetY;
        if (command.payload && command.payload.name) clone.name = command.payload.name;
        payload = { node_id: clone.id, original_id: nodeId };
      }
    } else if (command.name === "group") {
      // Group selected nodes or specified nodes
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
      } else {
        nodes = [...figma.currentPage.selection];
      }
      if (nodes.length < 1) {
        ok = false;
        payload = { error: "No nodes to group" };
      } else {
        const group = figma.group(nodes, figma.currentPage);
        if (command.payload && command.payload.name) group.name = command.payload.name;
        payload = { group_id: group.id, children_count: nodes.length };
      }
    } else if (command.name === "ungroup") {
      // Ungroup a group node
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "GROUP") {
        ok = false;
        payload = { error: "Node is not a GROUP" };
      } else {
        const children = [...node.children];
        const parent = node.parent;
        const childIds = children.map(c => c.id);
        for (const child of children) {
          parent.appendChild(child);
        }
        // Figma auto-removes empty groups, so skip explicit remove
        try { node.remove(); } catch (e) { /* already removed */ }
        payload = { ungrouped_ids: childIds };
      }
    } else if (command.name === "set_selection") {
      // Set the current selection
      const nodeIds = command.payload && command.payload.node_ids;
      if (!nodeIds || !Array.isArray(nodeIds)) {
        ok = false;
        payload = { error: "node_ids array required" };
      } else {
        const nodes = [];
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
        figma.currentPage.selection = nodes;
        payload = { selected_count: nodes.length, node_ids: nodes.map(n => n.id) };
      }
    } else if (command.name === "zoom_to") {
      // Zoom viewport to fit specific nodes
      const nodeIds = command.payload && command.payload.node_ids;
      const nodeId = command.payload && command.payload.node_id;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
      } else if (nodeId) {
        const n = await getNodeById(nodeId);
        if (n) nodes.push(n);
      } else {
        nodes = [...figma.currentPage.selection];
      }
      if (nodes.length === 0) {
        ok = false;
        payload = { error: "No nodes to zoom to" };
      } else {
        figma.viewport.scrollAndZoomIntoView(nodes);
        payload = { zoomed_to: nodes.map(n => n.id), viewport: { x: figma.viewport.center.x, y: figma.viewport.center.y, zoom: figma.viewport.zoom } };
      }
    } else if (command.name === "reorder") {
      // Change z-order: "front", "back", "forward", "backward"
      const nodeId = command.payload && command.payload.node_id;
      const direction = command.payload && command.payload.direction;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!node.parent || !("children" in node.parent)) {
        ok = false;
        payload = { error: "Node has no reorderable parent" };
      } else {
        const parent = node.parent;
        const siblings = parent.children;
        const idx = siblings.indexOf(node);
        if (direction === "front") {
          parent.insertChild(siblings.length - 1, node);
        } else if (direction === "back") {
          parent.insertChild(0, node);
        } else if (direction === "forward" && idx < siblings.length - 1) {
          parent.insertChild(idx + 1, node);
        } else if (direction === "backward" && idx > 0) {
          parent.insertChild(idx - 1, node);
        }
        payload = { node_id: node.id, direction, new_index: parent.children.indexOf(node) };
      }
    } else if (command.name === "set_locked") {
      // Lock or unlock a node
      const nodeId = command.payload && command.payload.node_id;
      const locked = command.payload && command.payload.locked;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("locked" in node)) {
        ok = false;
        payload = { error: "Node cannot be locked" };
      } else {
        node.locked = locked !== false;
        payload = { node_id: node.id, locked: node.locked };
      }
    } else if (command.name === "set_visible") {
      // Show or hide a node
      const nodeId = command.payload && command.payload.node_id;
      const visible = command.payload && command.payload.visible;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("visible" in node)) {
        ok = false;
        payload = { error: "Node cannot be hidden" };
      } else {
        node.visible = visible !== false;
        payload = { node_id: node.id, visible: node.visible };
      }
    } else if (command.name === "flatten") {
      // Flatten a vector node
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (typeof node.flatten !== "function") {
        ok = false;
        payload = { error: "Node cannot be flattened" };
      } else {
        const flattened = figma.flatten([node]);
        payload = { original_id: nodeId, flattened_id: flattened.id };
      }
    } else if (command.name === "set_auto_layout") {
      // Enable or modify auto layout on a frame
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "FRAME" && node.type !== "COMPONENT") {
        ok = false;
        payload = { error: "Only FRAME or COMPONENT can have auto layout" };
      } else {
        const p = command.payload;
        // Support both camelCase and snake_case
        const layoutMode = p.layoutMode || p.layout_mode;
        const itemSpacing = (p.itemSpacing !== undefined) ? p.itemSpacing : p.item_spacing;
        const padding = p.padding;
        const primaryAlignment = p.primaryAxisAlignItems || p.primary_alignment;
        const counterAlignment = p.counterAxisAlignItems || p.counter_alignment;

        if (layoutMode) node.layoutMode = layoutMode; // "HORIZONTAL" | "VERTICAL" | "NONE"
        if (typeof itemSpacing === "number") node.itemSpacing = itemSpacing;
        // Handle single padding value or individual values
        if (typeof padding === "number") {
          node.paddingLeft = node.paddingRight = node.paddingTop = node.paddingBottom = padding;
        } else {
          if (typeof p.paddingLeft === "number") node.paddingLeft = p.paddingLeft;
          if (typeof p.paddingRight === "number") node.paddingRight = p.paddingRight;
          if (typeof p.paddingTop === "number") node.paddingTop = p.paddingTop;
          if (typeof p.paddingBottom === "number") node.paddingBottom = p.paddingBottom;
        }
        if (primaryAlignment) node.primaryAxisAlignItems = primaryAlignment;
        if (counterAlignment) node.counterAxisAlignItems = counterAlignment;
        if (p.primaryAxisSizingMode) node.primaryAxisSizingMode = p.primaryAxisSizingMode;
        if (p.counterAxisSizingMode) node.counterAxisSizingMode = p.counterAxisSizingMode;
        payload = {
          node_id: node.id,
          layoutMode: node.layoutMode,
          itemSpacing: node.itemSpacing,
          padding: { left: node.paddingLeft, right: node.paddingRight, top: node.paddingTop, bottom: node.paddingBottom }
        };
      }
    } else if (command.name === "get_viewport") {
      // Get current viewport info
      payload = {
        center: figma.viewport.center,
        zoom: figma.viewport.zoom,
        bounds: figma.viewport.bounds
      };
    } else if (command.name === "set_viewport") {
      // Set viewport center and zoom
      const center = command.payload && command.payload.center;
      const zoom = command.payload && command.payload.zoom;
      if (center && typeof center.x === "number" && typeof center.y === "number") {
        figma.viewport.center = center;
      }
      if (typeof zoom === "number") {
        figma.viewport.zoom = zoom;
      }
      payload = {
        center: figma.viewport.center,
        zoom: figma.viewport.zoom
      };
    } else if (command.name === "rename") {
      // Rename a node
      const nodeId = command.payload && command.payload.node_id;
      const newName = command.payload && command.payload.name;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!newName) {
        ok = false;
        payload = { error: "name is required" };
      } else {
        const oldName = node.name;
        node.name = newName;
        payload = { node_id: node.id, old_name: oldName, new_name: node.name };
      }
    } else if (command.name === "resize") {
      // Resize a node
      const nodeId = command.payload && command.payload.node_id;
      const width = command.payload && command.payload.width;
      const height = command.payload && command.payload.height;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("resize" in node)) {
        ok = false;
        payload = { error: "Node cannot be resized" };
      } else {
        const oldWidth = node.width;
        const oldHeight = node.height;
        if (typeof width === "number" && typeof height === "number") {
          node.resize(width, height);
        } else if (typeof width === "number") {
          node.resize(width, node.height);
        } else if (typeof height === "number") {
          node.resize(node.width, height);
        }
        payload = { node_id: node.id, old: { width: oldWidth, height: oldHeight }, new: { width: node.width, height: node.height } };
      }
    } else if (command.name === "move") {
      // Move a node (change x, y position)
      const nodeId = command.payload && command.payload.node_id;
      const x = command.payload && command.payload.x;
      const y = command.payload && command.payload.y;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("x" in node)) {
        ok = false;
        payload = { error: "Node cannot be moved" };
      } else {
        const oldX = node.x;
        const oldY = node.y;
        if (typeof x === "number") node.x = x;
        if (typeof y === "number") node.y = y;
        payload = { node_id: node.id, old: { x: oldX, y: oldY }, new: { x: node.x, y: node.y } };
      }
    } else if (command.name === "set_opacity") {
      // Set node opacity
      const nodeId = command.payload && command.payload.node_id;
      const opacity = command.payload && command.payload.opacity;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("opacity" in node)) {
        ok = false;
        payload = { error: "Node does not support opacity" };
      } else if (typeof opacity !== "number") {
        ok = false;
        payload = { error: "opacity must be a number (0-1)" };
      } else {
        const oldOpacity = node.opacity;
        node.opacity = Math.max(0, Math.min(1, opacity));
        payload = { node_id: node.id, old_opacity: oldOpacity, new_opacity: node.opacity };
      }
    } else if (command.name === "set_corner_radius") {
      // Set corner radius on a frame/rectangle
      const nodeId = command.payload && command.payload.node_id;
      const radius = command.payload && command.payload.radius;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("cornerRadius" in node)) {
        ok = false;
        payload = { error: "Node does not support corner radius" };
      } else {
        const oldRadius = node.cornerRadius;
        if (typeof radius === "number") {
          node.cornerRadius = radius;
        }
        // Support individual corners
        const p = command.payload;
        if (typeof p.topLeft === "number") node.topLeftRadius = p.topLeft;
        if (typeof p.topRight === "number") node.topRightRadius = p.topRight;
        if (typeof p.bottomLeft === "number") node.bottomLeftRadius = p.bottomLeft;
        if (typeof p.bottomRight === "number") node.bottomRightRadius = p.bottomRight;
        payload = {
          node_id: node.id,
          cornerRadius: node.cornerRadius,
          corners: { topLeft: node.topLeftRadius, topRight: node.topRightRadius, bottomLeft: node.bottomLeftRadius, bottomRight: node.bottomRightRadius }
        };
      }
    } else if (command.name === "set_fill") {
      // Set fill color on a node
      const nodeId = command.payload && command.payload.node_id;
      const color = command.payload && command.payload.color;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("fills" in node)) {
        ok = false;
        payload = { error: "Node does not support fills" };
      } else if (!color) {
        ok = false;
        payload = { error: "color is required (e.g., {r:1, g:0, b:0})" };
      } else {
        const fill = { type: "SOLID", color: { r: color.r || 0, g: color.g || 0, b: color.b || 0 } };
        if (typeof color.a === "number") fill.opacity = color.a;
        node.fills = [fill];
        payload = { node_id: node.id, fill: fill };
      }
    } else if (command.name === "set_stroke") {
      // Set stroke on a node
      const nodeId = command.payload && command.payload.node_id;
      const color = command.payload && command.payload.color;
      const weight = command.payload && command.payload.weight;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("strokes" in node)) {
        ok = false;
        payload = { error: "Node does not support strokes" };
      } else {
        if (color) {
          const stroke = { type: "SOLID", color: { r: color.r || 0, g: color.g || 0, b: color.b || 0 } };
          node.strokes = [stroke];
        }
        if (typeof weight === "number") {
          node.strokeWeight = weight;
        }
        payload = { node_id: node.id, strokes: node.strokes, strokeWeight: node.strokeWeight };
      }
    } else if (command.name === "set_effects") {
      // Set effects (shadow, blur) on a node
      const nodeId = command.payload && command.payload.node_id;
      const effects = command.payload && command.payload.effects;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("effects" in node)) {
        ok = false;
        payload = { error: "Node does not support effects" };
      } else if (!Array.isArray(effects)) {
        ok = false;
        payload = { error: "effects must be an array" };
      } else {
        // Ensure all effects have required fields
        const validEffects = effects.map(e => {
          if (e.type === "DROP_SHADOW" || e.type === "INNER_SHADOW") {
            return {
              type: e.type,
              color: e.color || { r: 0, g: 0, b: 0, a: 0.25 },
              offset: e.offset || { x: 0, y: 4 },
              radius: e.radius || 4,
              spread: e.spread || 0,
              visible: e.visible !== false,
              blendMode: e.blendMode || "NORMAL"
            };
          } else if (e.type === "LAYER_BLUR" || e.type === "BACKGROUND_BLUR") {
            return { type: e.type, radius: e.radius || 10, visible: e.visible !== false };
          }
          return e;
        });
        node.effects = validEffects;
        payload = { node_id: node.id, effects: node.effects };
      }
    } else if (command.name === "create_component") {
      // Create a component from a node
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type === "COMPONENT" || node.type === "COMPONENT_SET") {
        ok = false;
        payload = { error: "Node is already a component" };
      } else if (!node.parent) {
        ok = false;
        payload = { error: "Node has no parent" };
      } else {
        const component = figma.createComponent();
        component.name = node.name;
        component.resize(node.width, node.height);
        // Clone children or the node itself
        if ("children" in node) {
          for (const child of node.children) {
            component.appendChild(child.clone());
          }
        }
        component.x = node.x;
        component.y = node.y;
        const parent = node.parent;
        const index = parent.children.indexOf(node);
        parent.insertChild(index, component);
        node.remove();
        payload = { component_id: component.id, component_name: component.name };
      }
    } else if (command.name === "detach_instance") {
      // Detach an instance to a frame
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "INSTANCE") {
        ok = false;
        payload = { error: "Node is not an instance" };
      } else {
        const detached = node.detachInstance();
        payload = { detached_id: detached.id, detached_name: detached.name };
      }
    } else if (command.name === "set_text") {
      // Set text content on a text node
      const nodeId = command.payload && command.payload.node_id;
      const text = command.payload && command.payload.text;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "TEXT") {
        ok = false;
        payload = { error: "Node is not a text node" };
      } else if (typeof text !== "string") {
        ok = false;
        payload = { error: "text must be a string" };
      } else {
        // Load the font first
        await figma.loadFontAsync(node.fontName);
        const oldText = node.characters;
        node.characters = text;
        payload = { node_id: node.id, old_text: oldText, new_text: node.characters };
      }
    } else if (command.name === "find_all") {
      // Find all nodes matching criteria
      const nodeType = command.payload && command.payload.type;
      const name = command.payload && command.payload.name;
      const nameContains = command.payload && command.payload.name_contains;
      let nodes = figma.currentPage.findAll();
      if (nodeType) {
        nodes = nodes.filter(n => n.type === nodeType);
      }
      if (name) {
        nodes = nodes.filter(n => n.name === name);
      }
      if (nameContains) {
        nodes = nodes.filter(n => n.name.includes(nameContains));
      }
      payload = {
        count: nodes.length,
        nodes: nodes.slice(0, 100).map(n => ({ id: n.id, name: n.name, type: n.type }))
      };
    } else if (command.name === "notify") {
      // Show a notification in Figma
      const message = command.payload && command.payload.message;
      const timeout = command.payload && command.payload.timeout;
      if (!message) {
        ok = false;
        payload = { error: "message is required" };
      } else {
        figma.notify(message, { timeout: timeout || 3000 });
        payload = { notified: true, message: message };
      }
    } else if (command.name === "create_frame") {
      // Create a new frame
      const p = command.payload || {};
      const frame = figma.createFrame();
      frame.name = p.name || "New Frame";
      frame.x = p.x || 0;
      frame.y = p.y || 0;
      frame.resize(p.width || 100, p.height || 100);
      if (p.fill) {
        frame.fills = [{ type: "SOLID", color: { r: p.fill.r || 0, g: p.fill.g || 0, b: p.fill.b || 0 } }];
      }
      payload = { id: frame.id, name: frame.name, x: frame.x, y: frame.y, width: frame.width, height: frame.height };
    } else if (command.name === "create_rectangle") {
      // Create a rectangle
      const p = command.payload || {};
      const rect = figma.createRectangle();
      rect.name = p.name || "Rectangle";
      rect.x = p.x || 0;
      rect.y = p.y || 0;
      rect.resize(p.width || 100, p.height || 100);
      if (p.fill) {
        rect.fills = [{ type: "SOLID", color: { r: p.fill.r || 0, g: p.fill.g || 0, b: p.fill.b || 0 } }];
      }
      if (typeof p.cornerRadius === "number") {
        rect.cornerRadius = p.cornerRadius;
      }
      payload = { id: rect.id, name: rect.name };
    } else if (command.name === "create_ellipse") {
      // Create an ellipse
      const p = command.payload || {};
      const ellipse = figma.createEllipse();
      ellipse.name = p.name || "Ellipse";
      ellipse.x = p.x || 0;
      ellipse.y = p.y || 0;
      ellipse.resize(p.width || 100, p.height || 100);
      if (p.fill) {
        ellipse.fills = [{ type: "SOLID", color: { r: p.fill.r || 0, g: p.fill.g || 0, b: p.fill.b || 0 } }];
      }
      payload = { id: ellipse.id, name: ellipse.name };
    } else if (command.name === "create_text") {
      // Create a text node
      const p = command.payload || {};
      const text = figma.createText();
      // Load font first
      await figma.loadFontAsync({ family: p.fontFamily || "Inter", style: p.fontStyle || "Regular" });
      text.name = p.name || "Text";
      text.x = p.x || 0;
      text.y = p.y || 0;
      text.characters = p.text || "Hello";
      if (typeof p.fontSize === "number") text.fontSize = p.fontSize;
      if (p.fill) {
        text.fills = [{ type: "SOLID", color: { r: p.fill.r || 0, g: p.fill.g || 0, b: p.fill.b || 0 } }];
      }
      payload = { id: text.id, name: text.name, characters: text.characters };
    } else if (command.name === "create_line") {
      // Create a line
      const p = command.payload || {};
      const line = figma.createLine();
      line.name = p.name || "Line";
      line.x = p.x || 0;
      line.y = p.y || 0;
      line.resize(p.length || 100, 0);
      if (typeof p.rotation === "number") line.rotation = p.rotation;
      line.strokes = [{ type: "SOLID", color: { r: p.stroke_r || 0, g: p.stroke_g || 0, b: p.stroke_b || 0 } }];
      line.strokeWeight = p.stroke_weight || 1;
      payload = { id: line.id, name: line.name };
    } else if (command.name === "create_polygon") {
      // Create a polygon
      const p = command.payload || {};
      const polygon = figma.createPolygon();
      polygon.name = p.name || "Polygon";
      polygon.x = p.x || 0;
      polygon.y = p.y || 0;
      polygon.resize(p.width || 100, p.height || 100);
      if (typeof p.pointCount === "number") polygon.pointCount = p.pointCount;
      if (p.fill) {
        polygon.fills = [{ type: "SOLID", color: { r: p.fill.r || 0, g: p.fill.g || 0, b: p.fill.b || 0 } }];
      }
      payload = { id: polygon.id, name: polygon.name, pointCount: polygon.pointCount };
    } else if (command.name === "create_star") {
      // Create a star
      const p = command.payload || {};
      const star = figma.createStar();
      star.name = p.name || "Star";
      star.x = p.x || 0;
      star.y = p.y || 0;
      star.resize(p.width || 100, p.height || 100);
      if (typeof p.pointCount === "number") star.pointCount = p.pointCount;
      if (typeof p.innerRadius === "number") star.innerRadius = p.innerRadius;
      if (p.fill) {
        star.fills = [{ type: "SOLID", color: { r: p.fill.r || 0, g: p.fill.g || 0, b: p.fill.b || 0 } }];
      }
      payload = { id: star.id, name: star.name, pointCount: star.pointCount };
    } else if (command.name === "delete_node") {
      // Delete a node
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type === "PAGE" || node.type === "DOCUMENT") {
        ok = false;
        payload = { error: "Cannot delete PAGE or DOCUMENT" };
      } else {
        const name = node.name;
        const type = node.type;
        node.remove();
        payload = { deleted: true, node_id: nodeId, name: name, type: type };
      }
    } else if (command.name === "duplicate") {
      // Duplicate a node in place
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("clone" in node)) {
        ok = false;
        payload = { error: "Node cannot be duplicated" };
      } else {
        const dup = node.clone();
        const p = command.payload;
        if (typeof p.offset_x === "number") dup.x = node.x + p.offset_x;
        if (typeof p.offset_y === "number") dup.y = node.y + p.offset_y;
        if (p.name) dup.name = p.name;
        payload = { original_id: node.id, duplicate_id: dup.id, name: dup.name };
      }
    } else if (command.name === "align") {
      // Align selected nodes or specified nodes
      const direction = command.payload && command.payload.direction; // "left", "center", "right", "top", "middle", "bottom"
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n && "x" in n) nodes.push(n);
        }
      } else {
        nodes = figma.currentPage.selection.filter(n => "x" in n);
      }
      if (nodes.length < 2) {
        ok = false;
        payload = { error: "Need at least 2 nodes to align" };
      } else if (!direction) {
        ok = false;
        payload = { error: "direction required: left, center, right, top, middle, bottom" };
      } else {
        const bounds = nodes.reduce((acc, n) => ({
          minX: Math.min(acc.minX, n.x),
          maxX: Math.max(acc.maxX, n.x + n.width),
          minY: Math.min(acc.minY, n.y),
          maxY: Math.max(acc.maxY, n.y + n.height)
        }), { minX: Infinity, maxX: -Infinity, minY: Infinity, maxY: -Infinity });

        for (const n of nodes) {
          if (direction === "left") n.x = bounds.minX;
          else if (direction === "right") n.x = bounds.maxX - n.width;
          else if (direction === "center") n.x = bounds.minX + (bounds.maxX - bounds.minX) / 2 - n.width / 2;
          else if (direction === "top") n.y = bounds.minY;
          else if (direction === "bottom") n.y = bounds.maxY - n.height;
          else if (direction === "middle") n.y = bounds.minY + (bounds.maxY - bounds.minY) / 2 - n.height / 2;
        }
        payload = { aligned: direction, node_count: nodes.length };
      }
    } else if (command.name === "distribute") {
      // Distribute nodes evenly
      const direction = command.payload && command.payload.direction; // "horizontal" or "vertical"
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n && "x" in n) nodes.push(n);
        }
      } else {
        nodes = figma.currentPage.selection.filter(n => "x" in n);
      }
      if (nodes.length < 3) {
        ok = false;
        payload = { error: "Need at least 3 nodes to distribute" };
      } else if (!direction || (direction !== "horizontal" && direction !== "vertical")) {
        ok = false;
        payload = { error: "direction required: horizontal or vertical" };
      } else {
        if (direction === "horizontal") {
          nodes.sort((a, b) => a.x - b.x);
          const totalWidth = nodes.reduce((sum, n) => sum + n.width, 0);
          const totalSpace = nodes[nodes.length - 1].x + nodes[nodes.length - 1].width - nodes[0].x;
          const gap = (totalSpace - totalWidth) / (nodes.length - 1);
          let currentX = nodes[0].x;
          for (const n of nodes) {
            n.x = currentX;
            currentX += n.width + gap;
          }
        } else {
          nodes.sort((a, b) => a.y - b.y);
          const totalHeight = nodes.reduce((sum, n) => sum + n.height, 0);
          const totalSpace = nodes[nodes.length - 1].y + nodes[nodes.length - 1].height - nodes[0].y;
          const gap = (totalSpace - totalHeight) / (nodes.length - 1);
          let currentY = nodes[0].y;
          for (const n of nodes) {
            n.y = currentY;
            currentY += n.height + gap;
          }
        }
        payload = { distributed: direction, node_count: nodes.length };
      }
    } else if (command.name === "boolean_union") {
      // Boolean union of selected nodes
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
      } else {
        nodes = [...figma.currentPage.selection];
      }
      if (nodes.length < 2) {
        ok = false;
        payload = { error: "Need at least 2 nodes for boolean operation" };
      } else {
        const result = figma.union(nodes, figma.currentPage);
        payload = { result_id: result.id, result_name: result.name, input_count: nodes.length };
      }
    } else if (command.name === "boolean_subtract") {
      // Boolean subtract
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
      } else {
        nodes = [...figma.currentPage.selection];
      }
      if (nodes.length < 2) {
        ok = false;
        payload = { error: "Need at least 2 nodes for boolean operation" };
      } else {
        const result = figma.subtract(nodes, figma.currentPage);
        payload = { result_id: result.id, result_name: result.name, input_count: nodes.length };
      }
    } else if (command.name === "boolean_intersect") {
      // Boolean intersect
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
      } else {
        nodes = [...figma.currentPage.selection];
      }
      if (nodes.length < 2) {
        ok = false;
        payload = { error: "Need at least 2 nodes for boolean operation" };
      } else {
        const result = figma.intersect(nodes, figma.currentPage);
        payload = { result_id: result.id, result_name: result.name, input_count: nodes.length };
      }
    } else if (command.name === "boolean_exclude") {
      // Boolean exclude
      const nodeIds = command.payload && command.payload.node_ids;
      let nodes = [];
      if (nodeIds && Array.isArray(nodeIds)) {
        for (const id of nodeIds) {
          const n = await getNodeById(id);
          if (n) nodes.push(n);
        }
      } else {
        nodes = [...figma.currentPage.selection];
      }
      if (nodes.length < 2) {
        ok = false;
        payload = { error: "Need at least 2 nodes for boolean operation" };
      } else {
        const result = figma.exclude(nodes, figma.currentPage);
        payload = { result_id: result.id, result_name: result.name, input_count: nodes.length };
      }
    } else if (command.name === "get_local_styles") {
      // Get all local styles
      const paintStyles = figma.getLocalPaintStyles().map(s => ({ id: s.id, name: s.name, type: "PAINT" }));
      const textStyles = figma.getLocalTextStyles().map(s => ({ id: s.id, name: s.name, type: "TEXT" }));
      const effectStyles = figma.getLocalEffectStyles().map(s => ({ id: s.id, name: s.name, type: "EFFECT" }));
      const gridStyles = figma.getLocalGridStyles().map(s => ({ id: s.id, name: s.name, type: "GRID" }));
      payload = {
        paint: paintStyles,
        text: textStyles,
        effect: effectStyles,
        grid: gridStyles,
        total: paintStyles.length + textStyles.length + effectStyles.length + gridStyles.length
      };
    } else if (command.name === "set_constraints") {
      // Set constraints on a node
      const nodeId = command.payload && command.payload.node_id;
      const horizontal = command.payload && command.payload.horizontal; // "MIN", "CENTER", "MAX", "STRETCH", "SCALE"
      const vertical = command.payload && command.payload.vertical;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("constraints" in node)) {
        ok = false;
        payload = { error: "Node does not support constraints" };
      } else {
        const currentConstraints = node.constraints;
        const newConstraints = {
          horizontal: horizontal || currentConstraints.horizontal,
          vertical: vertical || currentConstraints.vertical
        };
        node.constraints = newConstraints;
        payload = { node_id: node.id, constraints: node.constraints };
      }
    } else if (command.name === "create_page") {
      // Create a new page
      const p = command.payload || {};
      const page = figma.createPage();
      page.name = p.name || "New Page";
      payload = { page_id: page.id, page_name: page.name };
    } else if (command.name === "delete_page") {
      // Delete a page (cannot delete last page)
      const pageId = command.payload && command.payload.page_id;
      const page = pageId ? figma.root.findChild(n => n.id === pageId && n.type === "PAGE") : null;
      if (!page) {
        ok = false;
        payload = { error: "Page not found" };
      } else if (figma.root.children.length <= 1) {
        ok = false;
        payload = { error: "Cannot delete the last page" };
      } else {
        const pageName = page.name;
        page.remove();
        payload = { deleted: true, page_id: pageId, page_name: pageName };
      }
    } else if (command.name === "rotate") {
      // Rotate a node
      const nodeId = command.payload && command.payload.node_id;
      const angle = command.payload && command.payload.angle;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("rotation" in node)) {
        ok = false;
        payload = { error: "Node does not support rotation" };
      } else if (typeof angle !== "number") {
        ok = false;
        payload = { error: "angle is required (degrees)" };
      } else {
        const oldRotation = node.rotation;
        node.rotation = angle;
        payload = { node_id: node.id, old_rotation: oldRotation, new_rotation: node.rotation };
      }
    } else if (command.name === "flip") {
      // Flip a node horizontally or vertically
      const nodeId = command.payload && command.payload.node_id;
      const direction = command.payload && command.payload.direction; // "horizontal" or "vertical"
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("rescale" in node)) {
        ok = false;
        payload = { error: "Node does not support flip" };
      } else if (!direction || (direction !== "horizontal" && direction !== "vertical")) {
        ok = false;
        payload = { error: "direction required: horizontal or vertical" };
      } else {
        if (direction === "horizontal") {
          node.rescale(-1, 1);
        } else {
          node.rescale(1, -1);
        }
        payload = { node_id: node.id, flipped: direction };
      }
    } else if (command.name === "outline_stroke") {
      // Convert stroke to outlines
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("outlineStroke" in node) || typeof node.outlineStroke !== "function") {
        ok = false;
        payload = { error: "Node does not support outline stroke" };
      } else {
        const outlined = node.outlineStroke();
        if (!outlined) {
          ok = false;
          payload = { error: "Failed to outline stroke (no stroke or unsupported)" };
        } else {
          payload = { original_id: node.id, outlined_id: outlined.id, outlined_name: outlined.name };
        }
      }
    } else if (command.name === "set_blend_mode") {
      // Set blend mode on a node
      const nodeId = command.payload && command.payload.node_id;
      const blendMode = command.payload && command.payload.blend_mode;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("blendMode" in node)) {
        ok = false;
        payload = { error: "Node does not support blend mode" };
      } else if (!blendMode) {
        ok = false;
        payload = { error: "blend_mode required" };
      } else {
        const oldMode = node.blendMode;
        node.blendMode = blendMode;
        payload = { node_id: node.id, old_blend_mode: oldMode, new_blend_mode: node.blendMode };
      }
    } else if (command.name === "get_selection_colors") {
      // Extract all colors from selection
      const selection = figma.currentPage.selection;
      if (selection.length === 0) {
        ok = false;
        payload = { error: "No nodes selected" };
      } else {
        const colors = [];
        const extractColors = (node) => {
          if ("fills" in node && Array.isArray(node.fills)) {
            for (const fill of node.fills) {
              if (fill.type === "SOLID" && fill.color) {
                colors.push({ type: "fill", color: fill.color, nodeId: node.id, nodeName: node.name });
              }
            }
          }
          if ("strokes" in node && Array.isArray(node.strokes)) {
            for (const stroke of node.strokes) {
              if (stroke.type === "SOLID" && stroke.color) {
                colors.push({ type: "stroke", color: stroke.color, nodeId: node.id, nodeName: node.name });
              }
            }
          }
        };
        for (const node of selection) {
          extractColors(node);
          if ("children" in node) {
            for (const child of node.children) {
              extractColors(child);
            }
          }
        }
        payload = { count: colors.length, colors: colors.slice(0, 50) };
      }
    } else if (command.name === "swap_fill_stroke") {
      // Swap fill and stroke colors
      const nodeId = command.payload && command.payload.node_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("fills" in node) || !("strokes" in node)) {
        ok = false;
        payload = { error: "Node does not support fills/strokes" };
      } else {
        const oldFills = [...node.fills];
        const oldStrokes = [...node.strokes];
        // Convert fills to strokes and vice versa
        node.fills = oldStrokes.map(s => ({ ...s }));
        node.strokes = oldFills.map(f => ({ ...f }));
        payload = { node_id: node.id, swapped: true };
      }
    } else if (command.name === "copy_style") {
      // Copy style from one node to another
      const sourceId = command.payload && command.payload.source_id;
      const targetId = command.payload && command.payload.target_id;
      const source = sourceId ? await getNodeById(sourceId) : null;
      const target = targetId ? await getNodeById(targetId) : null;
      if (!source || !target) {
        ok = false;
        payload = { error: "Source or target node not found" };
      } else {
        // Copy fills
        if ("fills" in source && "fills" in target) {
          target.fills = [...source.fills];
        }
        // Copy strokes
        if ("strokes" in source && "strokes" in target) {
          target.strokes = [...source.strokes];
          if ("strokeWeight" in source && "strokeWeight" in target) {
            target.strokeWeight = source.strokeWeight;
          }
        }
        // Copy effects
        if ("effects" in source && "effects" in target) {
          target.effects = [...source.effects];
        }
        // Copy corner radius
        if ("cornerRadius" in source && "cornerRadius" in target) {
          target.cornerRadius = source.cornerRadius;
        }
        // Copy opacity
        if ("opacity" in source && "opacity" in target) {
          target.opacity = source.opacity;
        }
        payload = { source_id: source.id, target_id: target.id, copied: true };
      }
    } else if (command.name === "get_fonts") {
      // Get all fonts used in the document
      const fonts = new Set();
      const collectFonts = (node) => {
        if (node.type === "TEXT") {
          const fontName = node.fontName;
          if (fontName && fontName !== figma.mixed) {
            fonts.add(JSON.stringify(fontName));
          }
        }
        if ("children" in node) {
          for (const child of node.children) {
            collectFonts(child);
          }
        }
      };
      collectFonts(figma.currentPage);
      const fontList = Array.from(fonts).map(f => JSON.parse(f));
      payload = { count: fontList.length, fonts: fontList };
    } else if (command.name === "set_parent") {
      // Move a node to a new parent
      const nodeId = command.payload && command.payload.node_id;
      const parentId = command.payload && command.payload.parent_id;
      const node = nodeId ? await getNodeById(nodeId) : null;
      const newParent = parentId ? await getNodeById(parentId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!newParent) {
        ok = false;
        payload = { error: "Parent node not found" };
      } else if (!("children" in newParent)) {
        ok = false;
        payload = { error: "Target parent cannot contain children" };
      } else {
        const oldParentId = node.parent ? node.parent.id : null;
        newParent.appendChild(node);
        payload = { node_id: node.id, old_parent_id: oldParentId, new_parent_id: newParent.id };
      }
    } else if (command.name === "create_vector") {
      // Create a vector node with optional path data
      const p = command.payload || {};
      const vector = figma.createVector();
      if (p.name) vector.name = p.name;
      if (p.x !== undefined) vector.x = p.x;
      if (p.y !== undefined) vector.y = p.y;
      // vectorPaths is readonly in recent API, use vectorNetwork instead
      if (p.path_data) {
        try {
          // SVG path data to vectorNetwork conversion would require complex parsing
          // For now, just set basic properties
          vector.resize(p.width || 100, p.height || 100);
        } catch (e) {
          // Ignore path parsing errors
        }
      }
      payload = { node_id: vector.id, name: vector.name };
    } else if (command.name === "set_image_fill") {
      // Set an image fill from base64 or image hash
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("fills" in node)) {
        ok = false;
        payload = { error: "Node does not support fills" };
      } else {
        try {
          let imageHash = p.image_hash || p.imageHash;
          if (!imageHash && (p.base64 || p.image_base64)) {
            // Decode base64 to Uint8Array
            const base64Data = p.base64 || p.image_base64;
            const binaryString = atob(base64Data);
            const bytes = new Uint8Array(binaryString.length);
            for (let i = 0; i < binaryString.length; i++) {
              bytes[i] = binaryString.charCodeAt(i);
            }
            const image = figma.createImage(bytes);
            imageHash = image.hash;
          }
          if (imageHash) {
            const imageFill = {
              type: "IMAGE",
              imageHash: imageHash,
              scaleMode: p.scale_mode || p.scaleMode || "FILL"
            };
            node.fills = [imageFill];
            payload = { node_id: node.id, image_hash: imageHash };
          } else {
            ok = false;
            payload = { error: "No image_hash or base64 provided" };
          }
        } catch (e) {
          ok = false;
          payload = { error: String(e) };
        }
      }
    } else if (command.name === "get_plugin_data") {
      // Get plugin data from a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const key = p.key;
      const node = nodeId ? await getNodeById(nodeId) : figma.currentPage;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!key) {
        // Get all plugin data keys
        const keys = node.getPluginDataKeys();
        const data = {};
        for (const k of keys) {
          data[k] = node.getPluginData(k);
        }
        payload = { node_id: node.id, data: data };
      } else {
        const value = node.getPluginData(key);
        payload = { node_id: node.id, key: key, value: value };
      }
    } else if (command.name === "set_plugin_data") {
      // Set plugin data on a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const key = p.key;
      const value = p.value || "";
      const node = nodeId ? await getNodeById(nodeId) : figma.currentPage;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!key) {
        ok = false;
        payload = { error: "Missing required parameter: key" };
      } else {
        node.setPluginData(key, value);
        payload = { node_id: node.id, key: key, value: value };
      }
    } else if (command.name === "get_doc_info") {
      // Get document information
      const doc = figma.root;
      payload = {
        name: doc.name,
        id: doc.id,
        type: doc.type,
        page_count: doc.children.length,
        pages: doc.children.map(p => ({ id: p.id, name: p.name })),
        current_page: { id: figma.currentPage.id, name: figma.currentPage.name }
      };
    } else if (command.name === "get_absolute_bounds") {
      // Get absolute bounding box of a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("absoluteBoundingBox" in node)) {
        ok = false;
        payload = { error: "Node does not have absoluteBoundingBox" };
      } else {
        const bounds = node.absoluteBoundingBox;
        const transform = node.absoluteTransform;
        payload = {
          node_id: node.id,
          absolute_bounds: bounds ? { x: bounds.x, y: bounds.y, width: bounds.width, height: bounds.height } : null,
          absolute_transform: transform ? [[transform[0][0], transform[0][1], transform[0][2]], [transform[1][0], transform[1][1], transform[1][2]]] : null
        };
      }
    } else if (command.name === "create_component_set") {
      // Create a component set from existing components (for variants)
      const p = command.payload || {};
      const componentIds = p.component_ids || p.componentIds || [];
      if (componentIds.length < 2) {
        ok = false;
        payload = { error: "At least 2 component IDs required to create a component set" };
      } else {
        const components = [];
        for (const id of componentIds) {
          const node = await getNodeById(id);
          if (node && node.type === "COMPONENT") {
            components.push(node);
          }
        }
        if (components.length < 2) {
          ok = false;
          payload = { error: "At least 2 valid components required" };
        } else {
          const componentSet = figma.combineAsVariants(components, figma.currentPage);
          if (p.name) componentSet.name = p.name;
          payload = {
            component_set_id: componentSet.id,
            name: componentSet.name,
            variant_count: componentSet.children.length
          };
        }
      }
    } else if (command.name === "remove_auto_layout") {
      // Remove auto layout from a frame
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "FRAME" && node.type !== "COMPONENT" && node.type !== "INSTANCE") {
        ok = false;
        payload = { error: "Node must be a frame, component, or instance" };
      } else {
        // Setting layoutMode to NONE removes auto layout
        node.layoutMode = "NONE";
        payload = { node_id: node.id, layout_mode: "NONE" };
      }
    } else if (command.name === "create_slice") {
      // Create a slice for export
      const p = command.payload || {};
      const slice = figma.createSlice();
      if (p.name) slice.name = p.name;
      if (p.x !== undefined) slice.x = p.x;
      if (p.y !== undefined) slice.y = p.y;
      if (p.width !== undefined && p.height !== undefined) {
        slice.resize(p.width, p.height);
      }
      payload = { node_id: slice.id, name: slice.name, type: "SLICE" };
    } else if (command.name === "set_export_settings") {
      // Set export settings on a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("exportSettings" in node)) {
        ok = false;
        payload = { error: "Node does not support export settings" };
      } else {
        const format = (p.format || "PNG").toUpperCase();
        const scale = p.scale || 1;
        const suffix = p.suffix || "";
        const constraint = p.constraint || { type: "SCALE", value: scale };
        const newSettings = [{
          format: format,
          suffix: suffix,
          constraint: constraint
        }];
        // Append or replace
        if (p.append) {
          node.exportSettings = [...node.exportSettings, ...newSettings];
        } else {
          node.exportSettings = newSettings;
        }
        payload = { node_id: node.id, export_settings: node.exportSettings };
      }
    } else if (command.name === "get_reactions") {
      // Get prototype reactions from a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("reactions" in node)) {
        ok = false;
        payload = { error: "Node does not support reactions" };
      } else {
        const reactions = node.reactions.map(r => ({
          trigger: r.trigger,
          actions: r.actions
        }));
        payload = { node_id: node.id, reactions: reactions };
      }
    } else if (command.name === "set_reactions") {
      // Set prototype reactions on a node (simplified: navigate to node)
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const targetId = p.target_id || p.targetId;
      const trigger = p.trigger || "ON_CLICK";
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("reactions" in node)) {
        ok = false;
        payload = { error: "Node does not support reactions" };
      } else if (!targetId) {
        ok = false;
        payload = { error: "target_id required for navigation" };
      } else {
        const targetNode = await getNodeById(targetId);
        if (!targetNode) {
          ok = false;
          payload = { error: "Target node not found" };
        } else {
          // Create a simple navigate reaction
          const reaction = {
            trigger: { type: trigger },
            actions: [{
              type: "NODE",
              destinationId: targetId,
              navigation: p.navigation || "NAVIGATE",
              transition: p.transition || null,
              preserveScrollPosition: p.preserve_scroll || false
            }]
          };
          node.reactions = [reaction];
          payload = { node_id: node.id, target_id: targetId, trigger: trigger };
        }
      }
    } else if (command.name === "rasterize") {
      // Rasterize a node to image bytes
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("exportAsync" in node)) {
        ok = false;
        payload = { error: "Node does not support export" };
      } else {
        const format = (p.format || "PNG").toUpperCase();
        const scale = p.scale || 1;
        try {
          const bytes = await node.exportAsync({
            format: format,
            constraint: { type: "SCALE", value: scale }
          });
          // Convert to base64
          let binary = "";
          for (let i = 0; i < bytes.length; i++) {
            binary += String.fromCharCode(bytes[i]);
          }
          const base64 = btoa(binary);
          payload = {
            node_id: node.id,
            format: format,
            scale: scale,
            size: bytes.length,
            base64: base64
          };
        } catch (e) {
          ok = false;
          payload = { error: String(e) };
        }
      }
    } else if (command.name === "get_shared_plugin_data") {
      // Get shared plugin data (accessible across plugins)
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const namespace = p.namespace || "shared";
      const key = p.key;
      const node = nodeId ? await getNodeById(nodeId) : figma.currentPage;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!key) {
        // Get all shared plugin data keys for namespace
        const keys = node.getSharedPluginDataKeys(namespace);
        const data = {};
        for (const k of keys) {
          data[k] = node.getSharedPluginData(namespace, k);
        }
        payload = { node_id: node.id, namespace: namespace, data: data };
      } else {
        const value = node.getSharedPluginData(namespace, key);
        payload = { node_id: node.id, namespace: namespace, key: key, value: value };
      }
    } else if (command.name === "set_shared_plugin_data") {
      // Set shared plugin data
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const namespace = p.namespace || "shared";
      const key = p.key;
      const value = p.value || "";
      const node = nodeId ? await getNodeById(nodeId) : figma.currentPage;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!key) {
        ok = false;
        payload = { error: "Missing required parameter: key" };
      } else {
        node.setSharedPluginData(namespace, key, value);
        payload = { node_id: node.id, namespace: namespace, key: key, value: value };
      }
    } else if (command.name === "swap_component") {
      // Swap an instance's master component
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const componentId = p.component_id || p.componentId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "INSTANCE") {
        ok = false;
        payload = { error: "Node must be an instance" };
      } else if (!componentId) {
        ok = false;
        payload = { error: "component_id required" };
      } else {
        const newComponent = await getNodeById(componentId);
        if (!newComponent || newComponent.type !== "COMPONENT") {
          ok = false;
          payload = { error: "Component not found" };
        } else {
          node.swapComponent(newComponent);
          payload = { node_id: node.id, new_component_id: componentId };
        }
      }
    } else if (command.name === "resize_to_fit") {
      // Resize frame/auto-layout to fit contents
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("resize" in node)) {
        ok = false;
        payload = { error: "Node does not support resize" };
      } else {
        // For auto-layout frames, set sizing mode to HUG
        if ("layoutMode" in node && node.layoutMode !== "NONE") {
          if (p.axis === "horizontal" || p.axis === "both" || !p.axis) {
            node.primaryAxisSizingMode = "AUTO";
          }
          if (p.axis === "vertical" || p.axis === "both" || !p.axis) {
            node.counterAxisSizingMode = "AUTO";
          }
        }
        // For regular frames, calculate bounds from children
        if ("children" in node && node.children.length > 0 && (!("layoutMode" in node) || node.layoutMode === "NONE")) {
          let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
          for (const child of node.children) {
            if ("x" in child && "y" in child && "width" in child && "height" in child) {
              minX = Math.min(minX, child.x);
              minY = Math.min(minY, child.y);
              maxX = Math.max(maxX, child.x + child.width);
              maxY = Math.max(maxY, child.y + child.height);
            }
          }
          if (minX !== Infinity) {
            const padding = p.padding || 0;
            node.resize(maxX - minX + padding * 2, maxY - minY + padding * 2);
          }
        }
        payload = { node_id: node.id, width: node.width, height: node.height };
      }
    } else if (command.name === "get_characters") {
      // Get text node characters with styles
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "TEXT") {
        ok = false;
        payload = { error: "Node must be a text node" };
      } else {
        const characters = node.characters;
        const length = characters.length;
        // Get unique styles by checking ranges
        const styles = [];
        let currentStyle = null;
        let start = 0;
        for (let i = 0; i <= length; i++) {
          let style = null;
          if (i < length) {
            const fontSize = node.getRangeFontSize(i, i + 1);
            const fontName = node.getRangeFontName(i, i + 1);
            const fills = node.getRangeFills(i, i + 1);
            style = {
              fontSize: fontSize === figma.mixed ? "mixed" : fontSize,
              fontFamily: fontName === figma.mixed ? "mixed" : fontName.family,
              fontStyle: fontName === figma.mixed ? "mixed" : fontName.style,
              fills: fills === figma.mixed ? "mixed" : fills
            };
          }
          if (i === length || JSON.stringify(style) !== JSON.stringify(currentStyle)) {
            if (currentStyle !== null) {
              styles.push({ start: start, end: i, text: characters.substring(start, i), ...currentStyle });
            }
            currentStyle = style;
            start = i;
          }
        }
        payload = { node_id: node.id, characters: characters, length: length, styles: styles };
      }
    } else if (command.name === "set_range_fills") {
      // Set fills for a text range
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const start = p.start || 0;
      const end = p.end;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "TEXT") {
        ok = false;
        payload = { error: "Node must be a text node" };
      } else {
        const actualEnd = end !== undefined ? end : node.characters.length;
        const color = p.color || { r: 0, g: 0, b: 0 };
        const fills = [{ type: "SOLID", color: { r: color.r || 0, g: color.g || 0, b: color.b || 0 }, opacity: color.a !== undefined ? color.a : 1 }];
        await figma.loadFontAsync(node.getRangeFontName(start, actualEnd));
        node.setRangeFills(start, actualEnd, fills);
        payload = { node_id: node.id, start: start, end: actualEnd, color: color };
      }
    } else if (command.name === "set_range_font_size") {
      // Set font size for a text range
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const start = p.start || 0;
      const end = p.end;
      const fontSize = p.font_size || p.fontSize;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "TEXT") {
        ok = false;
        payload = { error: "Node must be a text node" };
      } else if (!fontSize) {
        ok = false;
        payload = { error: "font_size required" };
      } else {
        const actualEnd = end !== undefined ? end : node.characters.length;
        await figma.loadFontAsync(node.getRangeFontName(start, actualEnd));
        node.setRangeFontSize(start, actualEnd, fontSize);
        payload = { node_id: node.id, start: start, end: actualEnd, font_size: fontSize };
      }
    } else if (command.name === "insert_child") {
      // Insert a node at specific index in parent
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const parentId = p.parent_id || p.parentId;
      const index = p.index !== undefined ? p.index : 0;
      const node = nodeId ? await getNodeById(nodeId) : null;
      const parent = parentId ? await getNodeById(parentId) : null;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!parent) {
        ok = false;
        payload = { error: "Parent not found" };
      } else if (!("insertChild" in parent)) {
        ok = false;
        payload = { error: "Parent does not support insertChild" };
      } else {
        parent.insertChild(index, node);
        payload = { node_id: node.id, parent_id: parent.id, index: index };
      }
    } else if (command.name === "get_all_local_variables") {
      // Get all local variables in the document
      try {
        const variables = await figma.variables.getLocalVariablesAsync();
        const collections = await figma.variables.getLocalVariableCollectionsAsync();
        payload = {
          variables: variables.map(v => ({
            id: v.id,
            name: v.name,
            key: v.key,
            resolvedType: v.resolvedType,
            valuesByMode: v.valuesByMode
          })),
          collections: collections.map(c => ({
            id: c.id,
            name: c.name,
            key: c.key,
            modes: c.modes,
            variableIds: c.variableIds
          }))
        };
      } catch (e) {
        ok = false;
        payload = { error: String(e) };
      }
    } else if (command.name === "get_styles_by_type") {
      // Get local styles by type
      const p = command.payload || {};
      const styleType = (p.style_type || p.styleType || "FILL").toUpperCase();
      let styles = [];
      if (styleType === "FILL" || styleType === "PAINT") {
        styles = figma.getLocalPaintStyles();
      } else if (styleType === "TEXT") {
        styles = figma.getLocalTextStyles();
      } else if (styleType === "EFFECT") {
        styles = figma.getLocalEffectStyles();
      } else if (styleType === "GRID") {
        styles = figma.getLocalGridStyles();
      }
      payload = {
        style_type: styleType,
        count: styles.length,
        styles: styles.map(s => ({
          id: s.id,
          name: s.name,
          key: s.key,
          description: s.description
        }))
      };
    } else if (command.name === "apply_style") {
      // Apply a style to a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const styleId = p.style_id || p.styleId;
      const styleType = (p.style_type || p.styleType || "FILL").toUpperCase();
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!styleId) {
        ok = false;
        payload = { error: "style_id required" };
      } else {
        try {
          if (styleType === "FILL" || styleType === "PAINT") {
            if ("fillStyleId" in node) node.fillStyleId = styleId;
          } else if (styleType === "STROKE") {
            if ("strokeStyleId" in node) node.strokeStyleId = styleId;
          } else if (styleType === "TEXT") {
            if ("textStyleId" in node) node.textStyleId = styleId;
          } else if (styleType === "EFFECT") {
            if ("effectStyleId" in node) node.effectStyleId = styleId;
          } else if (styleType === "GRID") {
            if ("gridStyleId" in node) node.gridStyleId = styleId;
          }
          payload = { node_id: node.id, style_id: styleId, style_type: styleType };
        } catch (e) {
          ok = false;
          payload = { error: String(e) };
        }
      }
    } else if (command.name === "get_overrides") {
      // Get instance overrides
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "INSTANCE") {
        ok = false;
        payload = { error: "Node must be an instance" };
      } else {
        const overrides = node.overrides || [];
        payload = {
          node_id: node.id,
          main_component: node.mainComponent ? { id: node.mainComponent.id, name: node.mainComponent.name } : null,
          overrides: overrides
        };
      }
    } else if (command.name === "reset_overrides") {
      // Reset instance overrides
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "INSTANCE") {
        ok = false;
        payload = { error: "Node must be an instance" };
      } else {
        node.resetOverrides();
        payload = { node_id: node.id, reset: true };
      }
    } else if (command.name === "bring_to_front") {
      // Bring node to front (top of layer order)
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!node.parent || !("children" in node.parent)) {
        ok = false;
        payload = { error: "Cannot reorder this node" };
      } else {
        const parent = node.parent;
        const lastIndex = parent.children.length - 1;
        parent.insertChild(lastIndex, node);
        payload = { node_id: node.id, new_index: lastIndex };
      }
    } else if (command.name === "send_to_back") {
      // Send node to back (bottom of layer order)
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!node.parent || !("children" in node.parent)) {
        ok = false;
        payload = { error: "Cannot reorder this node" };
      } else {
        const parent = node.parent;
        parent.insertChild(0, node);
        payload = { node_id: node.id, new_index: 0 };
      }
    } else if (command.name === "set_grid") {
      // Set grid layout on a frame
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("layoutGrids" in node)) {
        ok = false;
        payload = { error: "Node does not support grids" };
      } else {
        const gridType = (p.pattern || "GRID").toUpperCase();
        const count = p.count || 12;
        const gutterSize = p.gutter || 20;
        const offset = p.offset || 0;
        const alignment = (p.alignment || "STRETCH").toUpperCase();
        const grid = {
          pattern: gridType,
          visible: p.visible !== false,
          color: p.color || { r: 1, g: 0, b: 0, a: 0.1 }
        };
        if (gridType === "COLUMNS" || gridType === "ROWS") {
          grid.count = count;
          grid.gutterSize = gutterSize;
          grid.offset = offset;
          grid.alignment = alignment;
        } else {
          grid.sectionSize = p.size || 10;
        }
        if (p.append) {
          node.layoutGrids = node.layoutGrids.concat([grid]);
        } else {
          node.layoutGrids = [grid];
        }
        payload = { node_id: node.id, grids: node.layoutGrids };
      }
    } else if (command.name === "get_layer_list") {
      // Get layer list (z-order) of a container
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : figma.currentPage;
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("children" in node)) {
        ok = false;
        payload = { error: "Node has no children" };
      } else {
        const layers = node.children.map((child, index) => ({
          index: index,
          id: child.id,
          name: child.name,
          type: child.type,
          visible: child.visible,
          locked: child.locked
        }));
        payload = { parent_id: node.id, layer_count: layers.length, layers: layers };
      }
    } else if (command.name === "scroll_and_zoom") {
      // Scroll and zoom viewport simultaneously
      const p = command.payload || {};
      const x = p.x !== undefined ? p.x : figma.viewport.center.x;
      const y = p.y !== undefined ? p.y : figma.viewport.center.y;
      const zoom = p.zoom !== undefined ? p.zoom : figma.viewport.zoom;
      figma.viewport.center = { x: x, y: y };
      figma.viewport.zoom = zoom;
      payload = { center: { x: x, y: y }, zoom: zoom };
    } else if (command.name === "get_paint_styles") {
      // Get detailed paint/fill information from a node
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else {
        const result = { node_id: node.id };
        if ("fills" in node && node.fills !== figma.mixed) {
          result.fills = node.fills;
        }
        if ("strokes" in node) {
          result.strokes = node.strokes;
        }
        if ("fillStyleId" in node) {
          result.fill_style_id = node.fillStyleId;
        }
        if ("strokeStyleId" in node) {
          result.stroke_style_id = node.strokeStyleId;
        }
        payload = result;
      }
    } else if (command.name === "set_text_case") {
      // Set text case transformation
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const textCase = (p.text_case || p.textCase || "ORIGINAL").toUpperCase();
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (node.type !== "TEXT") {
        ok = false;
        payload = { error: "Node must be a text node" };
      } else {
        await figma.loadFontAsync(node.fontName);
        node.textCase = textCase;
        payload = { node_id: node.id, text_case: textCase };
      }
    } else if (command.name === "get_stroke_details") {
      // Get detailed stroke information
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("strokes" in node)) {
        ok = false;
        payload = { error: "Node does not have strokes" };
      } else {
        payload = {
          node_id: node.id,
          strokes: node.strokes,
          stroke_weight: "strokeWeight" in node ? node.strokeWeight : null,
          stroke_align: "strokeAlign" in node ? node.strokeAlign : null,
          stroke_cap: "strokeCap" in node ? node.strokeCap : null,
          stroke_join: "strokeJoin" in node ? node.strokeJoin : null,
          stroke_miter_limit: "strokeMiterLimit" in node ? node.strokeMiterLimit : null,
          dash_pattern: "dashPattern" in node ? node.dashPattern : null
        };
      }
    } else if (command.name === "set_stroke_weight") {
      // Set stroke weight
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const weight = p.weight || p.stroke_weight;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("strokeWeight" in node)) {
        ok = false;
        payload = { error: "Node does not support stroke weight" };
      } else if (weight === undefined) {
        ok = false;
        payload = { error: "weight required" };
      } else {
        node.strokeWeight = weight;
        payload = { node_id: node.id, stroke_weight: weight };
      }
    } else if (command.name === "collapse_layer") {
      // Collapse a layer in the layers panel (for frames/groups)
      const p = command.payload || {};
      const nodeId = p.node_id || p.nodeId;
      const node = nodeId ? await getNodeById(nodeId) : (figma.currentPage.selection[0] || null);
      if (!node) {
        ok = false;
        payload = { error: "Node not found" };
      } else if (!("expanded" in node)) {
        ok = false;
        payload = { error: "Node does not support collapse/expand" };
      } else {
        const expand = p.expand !== undefined ? p.expand : false;
        node.expanded = expand;
        payload = { node_id: node.id, expanded: expand };
      }
    } else {
      ok = false;
      payload = { error: "Unknown command" };
    }
  } catch (err) {
    ok = false;
    payload = { error: String(err) };
  }

  figma.ui.postMessage({
    type: "command_result",
    command_id: command.id,
    ok,
    payload_json: (() => {
      try {
        const json = safeStringify(payload);
        if (json && json.length > MAX_PAYLOAD_CHARS) {
          return safeStringify({
            error: "Payload too large",
            size: json.length,
            hint: "Reduce depth or request a smaller selection"
          });
        }
        return json;
      } catch (err) {
        return safeStringify({ error: "Failed to serialize payload", details: String(err) });
      }
    })()
  });
};
