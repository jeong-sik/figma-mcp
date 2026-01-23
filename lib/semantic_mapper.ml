(** Semantic Style DSL Mapper - UIFormer 영감의 토큰 최적화 DSL

    연구 기반: arXiv 2512.13438 (UIFormer)
    - UI 표현이 전체 토큰의 80-99% 차지
    - Filter/Merge/Pass-through 3가지 연산으로 최적화
    - 48-56% 토큰 절감 목표

    ROI 기반 Tier 분류:
    - Tier 1 (High ROI, SSIM 80%+): layout, size, gap, padding, bg, radius
    - Tier 2 (Medium ROI, +10%): align, text, shadow, border
    - Tier 3 (Low ROI, +5%): font-weight, letter-spacing, etc.

    DSL 예시:
    - 현재: F(320×200)[class:"flex flex-row justify-center"]
    - 변경: F(320×200 row align:center cross:center)
*)

open Figma_types

(** ============== Helper Functions ============== *)

(** Float formatter - sub-pixel 지원, trailing zeros 제거 *)
let fmt v =
  if v = Float.round v then Printf.sprintf "%.0f" v
  else
    let s = Printf.sprintf "%.2g" v in
    if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
    else s

(** ============== Tier 1: High ROI (SSIM 80%+) ============== *)

(** Layout Direction - 구조 결정, 최고 ROI *)
let layout_mode = function
  | Horizontal -> "row"
  | Vertical -> "col"
  | None' -> ""

(** Gap - 아이템 간격, 밀도 결정 (g: = 2 chars vs gap: = 4 chars) *)
let gap v =
  if v > 0. then "g:" ^ fmt v else ""

(** Padding - 4-value shorthand 지원 *)
let padding (t, r, b, l) =
  if t = 0. && r = 0. && b = 0. && l = 0. then ""
  else if t = r && r = b && b = l then
    Printf.sprintf "p:%.0f" t
  else if t = b && r = l then
    Printf.sprintf "p:%.0f,%.0f" t r
  else
    Printf.sprintf "p:%.0f,%.0f,%.0f,%.0f" t r b l

(** Background Color - 즉시 인지 가능 *)
let bg color =
  if color = "" then "" else Printf.sprintf "bg:%s" color

(** Border Radius - 형태 인식 *)
let radius r =
  if r > 0. then Printf.sprintf "r:%.0f" r else ""

(** Border Radii (개별 모서리) - TL, TR, BR, BL *)
let radii = function
  | None -> ""
  | Some (tl, tr, br, bl) ->
    if tl = tr && tr = br && br = bl then
      if tl > 0. then Printf.sprintf "r:%.0f" tl else ""
    else
      Printf.sprintf "r:%.0f,%.0f,%.0f,%.0f" tl tr br bl

(** Size (width × height) *)
let size w h = Printf.sprintf "%.0f×%.0f" w h

(** ============== Tier 2: Medium ROI (SSIM +10%) ============== *)

(** Primary Axis Alignment *)
let align = function
  | Min -> "align:start"
  | Center -> "align:center"
  | Max -> "align:end"
  | SpaceBetween -> "align:between"
  | Baseline -> "align:baseline"

(** Cross Axis Alignment *)
let cross = function
  | Min -> "cross:start"
  | Center -> "cross:center"
  | Max -> "cross:end"
  | SpaceBetween -> "cross:between"
  | Baseline -> "cross:baseline"

(** Layout Sizing *)
let sizing axis = function
  | Fixed -> ""  (* 기본값, 생략 *)
  | Hug -> Printf.sprintf "%s:hug" axis
  | Fill -> Printf.sprintf "%s:fill" axis

(** Shadow - CSS box-shadow 스타일 (sh: = 3 chars vs shadow: = 7 chars)
    - inset → sh:i, prefix로 일관성 유지
    - zero-value 체크로 토큰 낭비 방지 *)
let shadow ?(inset=false) ~x ~y ~blur ~spread ~color () =
  (* Skip zero-value or transparent shadows *)
  if (x = 0. && y = 0. && blur = 0. && spread = 0.) || color = "" then ""
  else
    let prefix = if inset then "sh:i," else "sh:" in
    if spread > 0. then
      Printf.sprintf "%s%s,%s,%s,%s,%s" prefix (fmt x) (fmt y) (fmt blur) (fmt spread) color
    else
      Printf.sprintf "%s%s,%s,%s,%s" prefix (fmt x) (fmt y) (fmt blur) color

(** Border (b: = 2 chars vs border: = 7 chars) *)
let border ~weight ~color =
  if weight > 0. then
    Printf.sprintf "b:%s,%s" (fmt weight) color
  else ""

(** Text Color *)
let text_color c =
  if c = "" then "" else Printf.sprintf "c:%s" c

(** Font Size *)
let font_size s =
  if s > 0. then Printf.sprintf "s:%.0f" s else ""

(** ============== Tier 3: Low ROI (SSIM +5%) ============== *)

(** Font Weight *)
let font_weight w =
  if w <> 400 then Printf.sprintf "w:%d" w else ""

(** Letter Spacing *)
let letter_spacing = function
  | None -> ""
  | Some ls when ls <> 0. -> Printf.sprintf "ls:%.2f" ls
  | _ -> ""

(** Line Height *)
let line_height = function
  | None -> ""
  | Some lh when lh > 0. -> Printf.sprintf "lh:%.0f" lh
  | _ -> ""

(** Text Align Horizontal *)
let text_align_h = function
  | Left -> ""  (* 기본값 *)
  | Center -> "ta:center"
  | Right -> "ta:right"
  | Justified -> "ta:justify"

(** Text Align Vertical *)
let text_align_v = function
  | Top -> ""  (* 기본값 *)
  | Figma_types.Center -> "va:center"
  | Bottom -> "va:bottom"

(** Text Decoration *)
let text_decoration = function
  | NoDeco -> ""
  | Underline -> "u"
  | Strikethrough -> "s"

(** Opacity *)
let opacity o =
  if o < 1. then Printf.sprintf "o:%.2f" o else ""

(** ============== Tier 4: Specialist ============== *)

(** Blend Mode *)
let blend_mode mode =
  if mode = "" || mode = "NORMAL" || mode = "PASS_THROUGH" then ""
  else Printf.sprintf "blend:%s" (String.lowercase_ascii mode)

(** Constraints (Absolute positioning) *)
let constraint_h = function
  | `Left -> "pin:left"
  | `Right -> "pin:right"
  | `LeftRight -> "pin:lr"
  | `Center -> "pin:hcenter"
  | `Scale -> "pin:hscale"

let constraint_v = function
  | `Top -> "pin:top"
  | `Bottom -> "pin:bottom"
  | `TopBottom -> "pin:tb"
  | `Center -> "pin:vcenter"
  | `Scale -> "pin:vscale"

(** ============== Grid Layout (NEW - api_types.ts 기반) ============== *)

type grid_pattern = Columns | Rows | Grid

let grid_pattern_to_semantic count = function
  | Columns -> Printf.sprintf "gcols:%d" count
  | Rows -> Printf.sprintf "grows:%d" count
  | Grid -> Printf.sprintf "ggrid:%d" count

(** ============== Variable Binding (Design Tokens) ============== *)

let var_binding prefix var_id =
  Printf.sprintf "$%s:%s" prefix var_id

(** ============== 복합 DSL 생성 헬퍼 ============== *)

(** 비어있지 않은 속성만 공백으로 연결 *)
let join_attrs attrs =
  attrs
  |> List.filter (fun s -> s <> "")
  |> String.concat " "

(** 노드를 Semantic DSL로 변환 (메인 함수) *)
let node_to_semantic (node : ui_node) =
  let parts = [
    (* Tier 1: High ROI *)
    layout_mode node.layout_mode;
    (match node.bbox with
     | Some bbox -> size bbox.width bbox.height
     | None -> "");
    gap node.gap;
    padding node.padding;
    radii node.border_radii;
    radius node.border_radius;

    (* Tier 2: Medium ROI *)
    (if node.layout_mode <> None' then begin
      let primary = align node.primary_axis_align in
      let counter = cross node.counter_axis_align in
      join_attrs [primary; counter]
    end else "");
    sizing "w" node.layout_sizing_h;
    sizing "h" node.layout_sizing_v;
    opacity node.opacity;

    (* Typography (if text node) *)
    (match node.typography with
     | Some typo ->
       join_attrs [
         font_size typo.font_size;
         font_weight typo.font_weight;
         letter_spacing typo.letter_spacing;
         line_height typo.line_height;
         text_align_h typo.text_align_h;
         text_align_v typo.text_align_v;
         text_decoration typo.text_decoration;
       ]
     | None -> "");
  ] in
  join_attrs parts

(** Frame DSL 출력 예시: F(320×200 row gap:12 align:center bg:#FFF r:8) *)
let frame_dsl (node : ui_node) =
  let attrs = node_to_semantic node in
  let bg_attr =
    match node.fills with
    | [] -> ""
    | fill :: _ ->
      (match fill.color with
       | Some c -> bg (rgba_to_hex c)
       | None -> "")
  in
  let all_attrs = join_attrs [attrs; bg_attr] in
  Printf.sprintf "F(%s)" all_attrs

(** Text DSL 출력 예시: T"Hello"[s:16 w:700 c:#333] *)
let text_dsl (node : ui_node) =
  let content = Option.value ~default:"" node.characters in
  let attrs =
    match node.typography with
    | Some typo ->
      let color_attr =
        match node.fills with
        | fill :: _ ->
          (match fill.color with
           | Some c -> text_color (rgba_to_hex c)
           | None -> "")
        | [] -> ""
      in
      join_attrs [
        font_size typo.font_size;
        font_weight typo.font_weight;
        color_attr;
      ]
    | None -> ""
  in
  if attrs = "" then
    Printf.sprintf "T\"%s\"" content
  else
    Printf.sprintf "T\"%s\"[%s]" content attrs
