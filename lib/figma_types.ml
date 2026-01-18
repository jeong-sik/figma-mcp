(** Figma API 타입 정의 - OCaml 5.x Variant 기반 *)

(** ============== 색상 ============== *)
type rgba = {
  r: float;  (** 0.0 ~ 1.0 *)
  g: float;
  b: float;
  a: float;
}

(** RGBA → HEX 변환 (토큰 절약, 반올림 사용) *)
let rgba_to_hex { r; g; b; a } =
  if a < 1. then
    Printf.sprintf "#%02X%02X%02X%02X"
      (int_of_float (Float.round (r *. 255.)))
      (int_of_float (Float.round (g *. 255.)))
      (int_of_float (Float.round (b *. 255.)))
      (int_of_float (Float.round (a *. 255.)))
  else
    Printf.sprintf "#%02X%02X%02X"
      (int_of_float (Float.round (r *. 255.)))
      (int_of_float (Float.round (g *. 255.)))
      (int_of_float (Float.round (b *. 255.)))

(** RGBA → rgb() 변환 (픽셀 정확도: 반올림 사용) *)
let rgba_to_rgb { r; g; b; a } =
  let ri = int_of_float (Float.round (r *. 255.)) in
  let gi = int_of_float (Float.round (g *. 255.)) in
  let bi = int_of_float (Float.round (b *. 255.)) in
  if a < 1. then
    Printf.sprintf "rgba(%d,%d,%d,%.2f)" ri gi bi a
  else
    Printf.sprintf "rgb(%d,%d,%d)" ri gi bi

(** ============== 페인트 (fills, strokes) ============== *)
type paint_type =
  | Solid
  | GradientLinear
  | GradientRadial
  | GradientAngular
  | GradientDiamond
  | Image
  | Emoji

type paint = {
  paint_type: paint_type;
  visible: bool;
  opacity: float;
  color: rgba option;           (** Solid 전용 *)
  gradient_stops: (float * rgba) list;  (** Gradient 전용 *)
  image_ref: string option;     (** Image 전용 *)
  scale_mode: string option;    (** FILL, FIT, TILE, STRETCH *)
}

(** ============== 이펙트 ============== *)
type effect_type =
  | InnerShadow
  | DropShadow
  | LayerBlur
  | BackgroundBlur

type fx = {
  fx_type: effect_type;
  visible: bool;
  radius: float;
  color: rgba option;
  offset: (float * float) option;  (** x, y *)
  spread: float option;
}

(** ============== 레이아웃 ============== *)
type layout_mode =
  | None'        (** 자유 배치 (None과 충돌 방지 위해 ') *)
  | Horizontal
  | Vertical

type layout_align =
  | Min         (** flex-start *)
  | Center
  | Max         (** flex-end *)
  | SpaceBetween
  | Baseline

type layout_sizing =
  | Fixed
  | Hug         (** fit-content *)
  | Fill        (** flex: 1 *)

type constraints = {
  horizontal: [`Left | `Right | `LeftRight | `Center | `Scale];
  vertical: [`Top | `Bottom | `TopBottom | `Center | `Scale];
}

(** ============== 타이포그래피 ============== *)
type text_align_h = Left | Center | Right | Justified
type text_align_v = Top | Center | Bottom

type text_decoration = NoDeco | Underline | Strikethrough

type text_case = Original | Upper | Lower | Title | SmallCaps | SmallCapsForced

type typography = {
  font_family: string;
  font_size: float;
  font_weight: int;              (** 100~900 *)
  font_style: string;            (** normal, italic *)
  line_height: float option;     (** px 또는 % *)
  letter_spacing: float option;  (** em *)
  text_align_h: text_align_h;
  text_align_v: text_align_v;
  text_decoration: text_decoration;
  text_case: text_case;
}

(** 기본 타이포그래피 *)
let default_typography = {
  font_family = "Inter";
  font_size = 14.;
  font_weight = 400;
  font_style = "normal";
  line_height = None;
  letter_spacing = None;
  text_align_h = Left;
  text_align_v = Top;
  text_decoration = NoDeco;
  text_case = Original;
}

(** ============== 바운딩 박스 ============== *)
type bbox = {
  x: float;
  y: float;
  width: float;
  height: float;
}

(** ============== 노드 타입 (핵심 Variant!) ============== *)
type node_type =
  | Document
  | Canvas
  | Frame
  | Group
  | Vector
  | BooleanOperation
  | Star
  | Line
  | Ellipse
  | RegularPolygon
  | Rectangle
  | Text
  | Slice
  | Component
  | ComponentSet
  | Instance
  | Sticky
  | Section
  | Unknown of string

(** 문자열 → 노드 타입 *)
let node_type_of_string = function
  | "DOCUMENT" -> Document
  | "CANVAS" -> Canvas
  | "FRAME" -> Frame
  | "GROUP" -> Group
  | "VECTOR" -> Vector
  | "BOOLEAN_OPERATION" -> BooleanOperation
  | "STAR" -> Star
  | "LINE" -> Line
  | "ELLIPSE" -> Ellipse
  | "REGULAR_POLYGON" -> RegularPolygon
  | "RECTANGLE" -> Rectangle
  | "TEXT" -> Text
  | "SLICE" -> Slice
  | "COMPONENT" -> Component
  | "COMPONENT_SET" -> ComponentSet
  | "INSTANCE" -> Instance
  | "STICKY" -> Sticky
  | "SECTION" -> Section
  | s -> Unknown s

(** ============== UI 노드 (파싱 결과) ============== *)
type ui_node = {
  id: string;
  name: string;
  node_type: node_type;
  visible: bool;
  locked: bool;
  (* 크기/위치 *)
  bbox: bbox option;
  rotation: float;
  (* 스타일 *)
  fills: paint list;
  strokes: paint list;
  stroke_weight: float;
  effects: fx list;
  opacity: float;
  border_radius: float;
  border_radii: (float * float * float * float) option;
  (* 레이아웃 *)
  layout_mode: layout_mode;
  padding: float * float * float * float;
  gap: float;
  primary_axis_align: layout_align;
  counter_axis_align: layout_align;
  layout_sizing_h: layout_sizing;
  layout_sizing_v: layout_sizing;
  (* 텍스트 *)
  characters: string option;
  typography: typography option;
  (* 컴포넌트 *)
  component_id: string option;
  (* 자식 *)
  children: ui_node list;
}

(** 기본 노드 *)
let default_node = {
  id = "";
  name = "";
  node_type = Frame;
  visible = true;
  locked = false;
  bbox = None;
  rotation = 0.;
  fills = [];
  strokes = [];
  stroke_weight = 0.;
  effects = [];
  opacity = 1.;
  border_radius = 0.;
  border_radii = None;
  layout_mode = None';
  padding = (0., 0., 0., 0.);
  gap = 0.;
  primary_axis_align = Min;
  counter_axis_align = Min;
  layout_sizing_h = Fixed;
  layout_sizing_v = Fixed;
  characters = None;
  typography = None;
  component_id = None;
  children = [];
}

(** ============== 압축 DSL 출력용 타입 ============== *)

(**
   토큰 최적화 DSL 포맷:
   - 프레임: F(w,h)[bg:색][r:반경][p:패딩][g:갭]{children}
   - 텍스트: T"내용"[s:크기][w:굵기][c:색]
   - 이미지: I(w,h)[id:노드ID]
   - 버튼: B"라벨"[bg:색][r:반경]
   - 인풋: N"플레이스홀더"[w:넓이]

   예시:
   F(375,812)[bg:#FFF][p:16]{
     F(row)[g:8]{T"제목"[s:18][w:700] T"부제"[s:14][c:#888]}
     I(343,200)[id:1:234]
     B"확인"[bg:#007AFF][r:8]
   }
*)
type compact_node =
  | CFrame of {
      direction: [`Col | `Row];
      size: (float * float) option;
      bg: string option;
      radius: float option;
      padding: string option;  (** "16" 또는 "16,8,16,8" *)
      gap: float option;
      children: compact_node list;
    }
  | CText of {
      content: string;
      size: float option;
      weight: int option;
      color: string option;
      align: string option;
    }
  | CImage of {
      size: (float * float) option;
      node_id: string;
    }
  | CButton of {
      label: string;
      bg: string option;
      radius: float option;
    }
  | CInput of {
      placeholder: string;
      width: float option;
    }
  | CVector of {
      name: string;
      size: (float * float) option;
    }
