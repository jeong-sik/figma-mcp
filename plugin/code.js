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
