(** Figma JSON 파서 - 타입 안전한 변환 *)

open Figma_types

(** ============== JSON 헬퍼 ============== *)

let ( >>= ) opt f = Option.bind opt f
let ( |? ) opt default = Option.value opt ~default

let get_string json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

let get_float json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`Float f) -> Some f
      | Some (`Int i) -> Some (float_of_int i)
      | _ -> None)
  | _ -> None

let get_int json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`Int i) -> Some i
      | Some (`Float f) -> Some (int_of_float f)
      | _ -> None)
  | _ -> None

let get_bool json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`Bool b) -> Some b
      | _ -> None)
  | _ -> None

let get_list json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`List l) -> Some l
      | _ -> None)
  | _ -> None

let get_assoc json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`Assoc _ as a) -> Some a
      | _ -> None)
  | _ -> None

(** ============== RGBA 파싱 ============== *)
let parse_rgba json =
  {
    r = get_float json "r" |? 0.;
    g = get_float json "g" |? 0.;
    b = get_float json "b" |? 0.;
    a = get_float json "a" |? 1.;
  }

(** ============== Paint 파싱 ============== *)
let parse_paint_type = function
  | "SOLID" -> Solid
  | "GRADIENT_LINEAR" -> GradientLinear
  | "GRADIENT_RADIAL" -> GradientRadial
  | "GRADIENT_ANGULAR" -> GradientAngular
  | "GRADIENT_DIAMOND" -> GradientDiamond
  | "IMAGE" -> Image
  | "EMOJI" -> Emoji
  | _ -> Solid

let parse_paint json =
  let paint_type =
    get_string json "type"
    |> Option.map parse_paint_type
    |? Solid
  in
  let visible = get_bool json "visible" |? true in
  let opacity = get_float json "opacity" |? 1. in
  let color = get_assoc json "color" |> Option.map parse_rgba in
  let gradient_stops =
    get_list json "gradientStops"
    |> Option.map (List.filter_map (fun stop ->
        match get_float stop "position", get_assoc stop "color" with
        | Some pos, Some c -> Some (pos, parse_rgba c)
        | _ -> None
      ))
    |? []
  in
  let image_ref = get_string json "imageRef" in
  let scale_mode = get_string json "scaleMode" in
  { paint_type; visible; opacity; color; gradient_stops; image_ref; scale_mode }

let parse_paints json key =
  get_list json key
  |> Option.map (List.filter_map (fun p ->
      match p with
      | `Assoc _ -> Some (parse_paint p)
      | _ -> None
    ))
  |? []

(** ============== Effect 파싱 ============== *)
let parse_effect_type = function
  | "INNER_SHADOW" -> InnerShadow
  | "DROP_SHADOW" -> DropShadow
  | "LAYER_BLUR" -> LayerBlur
  | "BACKGROUND_BLUR" -> BackgroundBlur
  | _ -> DropShadow

let parse_fx json =
  let fx_type =
    get_string json "type"
    |> Option.map parse_effect_type
    |? DropShadow
  in
  let visible = get_bool json "visible" |? true in
  let radius = get_float json "radius" |? 0. in
  let color = get_assoc json "color" |> Option.map parse_rgba in
  let offset =
    match get_assoc json "offset" with
    | Some o -> Some (get_float o "x" |? 0., get_float o "y" |? 0.)
    | None -> None
  in
  let spread = get_float json "spread" in
  { fx_type; visible; radius; color; offset; spread }

let parse_effects json =
  get_list json "effects"
  |> Option.map (List.filter_map (fun e ->
      match e with
      | `Assoc _ -> Some (parse_fx e)
      | _ -> None
    ))
  |? []

(** ============== BBox 파싱 ============== *)
let parse_bbox json =
  get_assoc json "absoluteBoundingBox" >>= fun b ->
  match get_float b "x", get_float b "y", get_float b "width", get_float b "height" with
  | Some x, Some y, Some w, Some h -> Some { x; y; width = w; height = h }
  | _ -> None

(** ============== 레이아웃 파싱 ============== *)
let parse_layout_mode json =
  match get_string json "layoutMode" with
  | Some "VERTICAL" -> Vertical
  | Some "HORIZONTAL" -> Horizontal
  | _ -> None'

let parse_layout_align = function
  | "MIN" -> Min
  | "CENTER" -> Center
  | "MAX" -> Max
  | "SPACE_BETWEEN" -> SpaceBetween
  | "BASELINE" -> Baseline
  | _ -> Min

let parse_layout_sizing = function
  | "FIXED" -> Fixed
  | "HUG" -> Hug
  | "FILL" -> Fill
  | _ -> Fixed

(** ============== 타이포그래피 파싱 ============== *)
let parse_text_align_h = function
  | "LEFT" -> Left
  | "CENTER" -> Center
  | "RIGHT" -> Right
  | "JUSTIFIED" -> Justified
  | _ -> Left

let parse_text_align_v = function
  | "TOP" -> Top
  | "CENTER" -> Center
  | "BOTTOM" -> Bottom
  | _ -> Top

let parse_text_decoration = function
  | "UNDERLINE" -> Underline
  | "STRIKETHROUGH" -> Strikethrough
  | "NONE" -> NoDeco
  | _ -> NoDeco

let parse_text_case = function
  | "UPPER" -> Upper
  | "LOWER" -> Lower
  | "TITLE" -> Title
  | "SMALL_CAPS" -> SmallCaps
  | "SMALL_CAPS_FORCED" -> SmallCapsForced
  | "ORIGINAL" -> Original
  | _ -> Original

let parse_typography json =
  get_assoc json "style" >>= fun s ->
  Some {
    font_family = get_string s "fontFamily" |? "Inter";
    font_size = get_float s "fontSize" |? 14.;
    font_weight = get_int s "fontWeight" |? 400;
    font_style = get_string s "fontPostScriptName" |? "normal";
    line_height = get_float s "lineHeightPx";
    letter_spacing = get_float s "letterSpacing";
    text_align_h = get_string s "textAlignHorizontal" |> Option.map parse_text_align_h |? Left;
    text_align_v = get_string s "textAlignVertical" |> Option.map parse_text_align_v |? Top;
    text_decoration = get_string s "textDecoration" |> Option.map parse_text_decoration |? NoDeco;
    text_case = get_string s "textCase" |> Option.map parse_text_case |? Original;
  }

(** ============== 메인 노드 파싱 ============== *)
let rec parse_node ?(depth=0) ?(max_depth=20) json =
  if depth > max_depth then None
  else
    let id = get_string json "id" |? "" in
    let name = get_string json "name" |? "" in
    let node_type =
      get_string json "type"
      |> Option.map node_type_of_string
      |? Frame
    in
    let visible = get_bool json "visible" |? true in
    let locked = get_bool json "locked" |? false in

    (* 보이지 않는 노드 스킵 (옵션) *)
    (* if not visible then None else *)

    let bbox = parse_bbox json in
    let rotation = get_float json "rotation" |? 0. in

    (* 스타일 *)
    let fills = parse_paints json "fills" in
    let strokes = parse_paints json "strokes" in
    let stroke_weight = get_float json "strokeWeight" |? 0. in
    let effects = parse_effects json in
    let opacity = get_float json "opacity" |? 1. in
    let border_radius = get_float json "cornerRadius" |? 0. in
    let border_radii =
      match get_list json "rectangleCornerRadii" with
      | Some [`Float tl; `Float tr; `Float br; `Float bl] ->
          Some (tl, tr, br, bl)
      | Some [`Int tl; `Int tr; `Int br; `Int bl] ->
          Some (float_of_int tl, float_of_int tr, float_of_int br, float_of_int bl)
      | _ -> None
    in

    (* 레이아웃 *)
    let layout_mode = parse_layout_mode json in
    let pt = get_float json "paddingTop" |? 0. in
    let pr = get_float json "paddingRight" |? 0. in
    let pb = get_float json "paddingBottom" |? 0. in
    let pl = get_float json "paddingLeft" |? 0. in
    let gap = get_float json "itemSpacing" |? 0. in
    let primary_axis_align =
      get_string json "primaryAxisAlignItems"
      |> Option.map parse_layout_align
      |? Min
    in
    let counter_axis_align =
      get_string json "counterAxisAlignItems"
      |> Option.map parse_layout_align
      |? Min
    in
    let layout_sizing_h =
      get_string json "layoutSizingHorizontal"
      |> Option.map parse_layout_sizing
      |? Fixed
    in
    let layout_sizing_v =
      get_string json "layoutSizingVertical"
      |> Option.map parse_layout_sizing
      |? Fixed
    in

    (* 텍스트 *)
    let characters = get_string json "characters" in
    let typography =
      if node_type = Text then parse_typography json
      else None
    in

    (* 컴포넌트 *)
    let component_id = get_string json "componentId" in

    (* 자식 노드 재귀 파싱 *)
    let children =
      get_list json "children"
      |> Option.map (List.filter_map (parse_node ~depth:(depth+1) ~max_depth))
      |? []
    in

    Some {
      id; name; node_type; visible; locked;
      bbox; rotation;
      fills; strokes; stroke_weight; effects;
      opacity; border_radius; border_radii;
      layout_mode; padding = (pt, pr, pb, pl); gap;
      primary_axis_align; counter_axis_align;
      layout_sizing_h; layout_sizing_v;
      characters; typography; component_id;
      children;
    }

(** JSON 문자열에서 파싱 *)
let parse_json_string s =
  try
    let json = Yojson.Safe.from_string s in
    parse_node json
  with _ -> None

(** JSON에서 파싱 *)
let parse_json json = parse_node json
