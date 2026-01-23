(** 토큰 최적화 DSL 생성기 *)

open Figma_types
open Printf

(** Option 기본값 연산자 *)
let ( |? ) opt default = Option.value opt ~default

(** ============== 숫자 포맷팅 (소수점 최소화) ============== *)
let fmt_num f =
  if Float.is_integer f then sprintf "%.0f" f
  else sprintf "%.1f" f

let fmt_num_opt = function
  | Some f -> fmt_num f
  | None -> ""

(** ============== 스타일 속성 압축 ============== *)

(** P0-4: Gradient stops → CSS linear-gradient 변환 *)
let gradient_to_css ?(precise=false) (stops : (float * rgba) list) =
  if stops = [] then None
  else
    let stop_strs = stops |> List.map (fun (pos, color) ->
      let color_str =
        if precise then rgba_to_rgb color
        else rgba_to_hex color
      in
      sprintf "%s %.0f%%" color_str (pos *. 100.)
    ) in
    Some (sprintf "linear-gradient(to right,%s)" (String.concat "," stop_strs))

(** 배경색 추출 (Solid 또는 Gradient) - HEX 포맷 *)
let get_bg_color (fills : paint list) =
  match List.find_opt (fun (p : paint) -> p.visible) fills with
  | None -> None
  | Some p ->
      match p.paint_type with
      | Solid -> Option.map rgba_to_hex p.color
      | GradientLinear -> gradient_to_css p.gradient_stops
      | GradientRadial | GradientAngular | GradientDiamond ->
          (* Radial 등은 linear로 fallback - 향후 개선 가능 *)
          gradient_to_css p.gradient_stops
      | Image | Emoji -> None

(** 배경색 추출 (Solid 또는 Gradient) - RGB 포맷 (픽셀 정확도) *)
let get_bg_color_precise (fills : paint list) =
  match List.find_opt (fun (p : paint) -> p.visible) fills with
  | None -> None
  | Some p ->
      match p.paint_type with
      | Solid -> Option.map rgba_to_rgb p.color
      | GradientLinear -> gradient_to_css ~precise:true p.gradient_stops
      | GradientRadial | GradientAngular | GradientDiamond ->
          gradient_to_css ~precise:true p.gradient_stops
      | Image | Emoji -> None

let get_bg_fidelity (fills : paint list) =
  match List.find_opt (fun (p : paint) -> p.visible) fills with
  | None -> None
  | Some p ->
      (match p.paint_type with
       | Solid -> Option.map rgba_to_hex p.color
       | GradientLinear | GradientRadial | GradientAngular | GradientDiamond -> Some "grad"
       | Image -> Some "img"
       | Emoji -> Some "emoji")

(** 테두리색 추출 *)
let get_stroke_color (strokes : paint list) =
  match List.find_opt (fun (p : paint) -> p.paint_type = Solid && p.visible) strokes with
  | Some p -> Option.map rgba_to_hex p.color
  | None -> None

(** 패딩 압축: "16" 또는 "16,8,16,8" *)
let fmt_padding (t, r, b, l) =
  if t = 0. && r = 0. && b = 0. && l = 0. then None
  else if t = r && r = b && b = l then Some (fmt_num t)
  else Some (sprintf "%s,%s,%s,%s" (fmt_num t) (fmt_num r) (fmt_num b) (fmt_num l))

(** ============== Style Registry (Memoization) ============== *)
(** 중복 스타일을 변수로 추출하여 토큰 절약 *)

module StyleRegistry = struct
  type t = {
    colors: (string, string) Hashtbl.t;        (** hex → variable name *)
    fonts: (float, string) Hashtbl.t;          (** font_size → variable name *)
    weights: (int, string) Hashtbl.t;          (** font_weight → variable name *)
    mutable color_count: int;
    mutable font_count: int;
    mutable weight_count: int;
    threshold: int;  (** 이 횟수 이상 반복되면 변수화 *)
  }

  (** 사용 빈도 카운팅용 임시 해시 *)
  type counter = {
    color_freq: (string, int) Hashtbl.t;
    font_freq: (float, int) Hashtbl.t;
    weight_freq: (int, int) Hashtbl.t;
  }

  let create_counter () = {
    color_freq = Hashtbl.create 32;
    font_freq = Hashtbl.create 16;
    weight_freq = Hashtbl.create 8;
  }

  let create ?(threshold=2) () = {
    colors = Hashtbl.create 32;
    fonts = Hashtbl.create 16;
    weights = Hashtbl.create 8;
    color_count = 0;
    font_count = 0;
    weight_count = 0;
    threshold;
  }

  (** 빈도 카운트 증가 *)
  let count_color counter hex =
    let n = Hashtbl.find_opt counter.color_freq hex |> Option.value ~default:0 in
    Hashtbl.replace counter.color_freq hex (n + 1)

  let count_font counter size =
    let n = Hashtbl.find_opt counter.font_freq size |> Option.value ~default:0 in
    Hashtbl.replace counter.font_freq size (n + 1)

  let count_weight counter w =
    let n = Hashtbl.find_opt counter.weight_freq w |> Option.value ~default:0 in
    Hashtbl.replace counter.weight_freq w (n + 1)

  (** 카운터 → 레지스트리 변환 (threshold 이상만 변수화) *)
  let finalize counter threshold =
    let reg = create ~threshold () in

    Hashtbl.iter (fun hex freq ->
      if freq >= threshold then begin
        reg.color_count <- reg.color_count + 1;
        let var_name = sprintf "$c%d" reg.color_count in
        Hashtbl.add reg.colors hex var_name
      end
    ) counter.color_freq;

    Hashtbl.iter (fun size freq ->
      if freq >= threshold then begin
        reg.font_count <- reg.font_count + 1;
        let var_name = sprintf "$f%d" reg.font_count in
        Hashtbl.add reg.fonts size var_name
      end
    ) counter.font_freq;

    Hashtbl.iter (fun w freq ->
      if freq >= threshold then begin
        reg.weight_count <- reg.weight_count + 1;
        let var_name = sprintf "$w%d" reg.weight_count in
        Hashtbl.add reg.weights w var_name
      end
    ) counter.weight_freq;

    reg

  (** 변수 조회 (없으면 원래 값 반환) *)
  let lookup_color reg hex =
    Hashtbl.find_opt reg.colors hex |> Option.value ~default:hex

  let lookup_font reg size =
    match Hashtbl.find_opt reg.fonts size with
    | Some var -> var
    | None -> fmt_num size

  let lookup_weight reg w =
    match Hashtbl.find_opt reg.weights w with
    | Some var -> var
    | None -> string_of_int w

  (** 변수 정의 블록 생성 *)
  let to_defs reg =
    if Hashtbl.length reg.colors = 0 &&
       Hashtbl.length reg.fonts = 0 &&
       Hashtbl.length reg.weights = 0 then ""
    else
      let parts = ref [] in
      Hashtbl.iter (fun hex var ->
        parts := sprintf "%s=%s" var hex :: !parts
      ) reg.colors;
      Hashtbl.iter (fun size var ->
        parts := sprintf "%s=%s" var (fmt_num size) :: !parts
      ) reg.fonts;
      Hashtbl.iter (fun w var ->
        parts := sprintf "%s=%d" var w :: !parts
      ) reg.weights;
      "@vars{" ^ String.concat ";" (List.rev !parts) ^ "}\n"

  (** DSL 문자열에서 원래 값을 변수로 치환 *)
  let apply_to_dsl reg dsl =
    let result = ref dsl in
    (* 색상 치환: #XXXXXX → $cN *)
    Hashtbl.iter (fun hex var ->
      result := Str.global_replace (Str.regexp_string hex) var !result
    ) reg.colors;
    (* 폰트 크기 치환: s:XX → s:$fN *)
    Hashtbl.iter (fun size var ->
      let size_str = fmt_num size in
      result := Str.global_replace
        (Str.regexp (sprintf "s:%s\\([^0-9]\\|$\\)" size_str))
        (sprintf "s:%s\\1" var) !result
    ) reg.fonts;
    (* 폰트 웨이트 치환: w:XXX → w:$wN *)
    Hashtbl.iter (fun w var ->
      result := Str.global_replace
        (Str.regexp (sprintf "w:%d\\([^0-9]\\|$\\)" w))
        (sprintf "w:%s\\1" var) !result
    ) reg.weights;
    !result

  (** 노드 트리에서 스타일 수집 (1차 패스) *)
  let rec collect_from_node counter (node : ui_node) =
    (* 색상 수집 *)
    (match get_bg_color node.fills with
     | Some c when c <> "#FFFFFF" && c <> "#FFF" && c <> "#000000" -> count_color counter c
     | _ -> ());
    (match get_stroke_color node.strokes with
     | Some c -> count_color counter c
     | _ -> ());

    (* 타이포그래피 수집 *)
    (match node.typography with
     | Some t ->
         if t.font_size <> 14. then count_font counter t.font_size;
         if t.font_weight <> 400 then count_weight counter t.font_weight;
         (match get_bg_color node.fills with
          | Some c when c <> "#000000" && c <> "#191919" -> count_color counter c
          | _ -> ())
     | None -> ());

    (* 자식 순회 *)
    List.iter (collect_from_node counter) node.children
end

(** ============== 노드 분류 (UI 의미 추론) ============== *)

type ui_semantic =
  | SemFrame
  | SemText
  | SemButton
  | SemInput
  | SemImage
  | SemIcon
  | SemDivider

let align_to_code = function
  | Min -> "min"
  | Center -> "c"
  | Max -> "max"
  | SpaceBetween -> "sb"
  | Baseline -> "base"

let sizing_to_code = function
  | Fixed -> "fix"
  | Hug -> "hug"
  | Fill -> "fill"

let add_xy_size_attrs ?(is_root=false) ?origin attrs node =
  match node.bbox, origin with
  | Some b, Some (ox, oy) ->
      let attrs =
        if is_root then attrs
        else sprintf "xy:%s,%s" (fmt_num (b.x -. ox)) (fmt_num (b.y -. oy)) :: attrs
      in
      sprintf "sz:%s,%s" (fmt_num b.width) (fmt_num b.height) :: attrs
  | Some b, None ->
      let attrs =
        if is_root then attrs
        else sprintf "xy:%s,%s" (fmt_num b.x) (fmt_num b.y) :: attrs
      in
      sprintf "sz:%s,%s" (fmt_num b.width) (fmt_num b.height) :: attrs
  | _ -> attrs

let classify_node node =
  let name_lower = String.lowercase_ascii node.name in
  let has_prefix prefix =
    String.length name_lower >= String.length prefix &&
    String.sub name_lower 0 (String.length prefix) = prefix
  in

  match node.node_type with
  | Text -> SemText
  | Vector | Line | Ellipse | Star | RegularPolygon | Rectangle ->
      (* 얇은 사각형 = Divider *)
      (match node.bbox with
       | Some b when b.height <= 2. || b.width <= 2. -> SemDivider
       | _ -> SemIcon)
  | Instance | Component ->
      if has_prefix "button" || has_prefix "btn" then SemButton
      else if has_prefix "input" || has_prefix "field" || has_prefix "textfield" then SemInput
      else if has_prefix "icon" || has_prefix "ic_" then SemIcon
      else if has_prefix "image" || has_prefix "img" || has_prefix "photo" then SemImage
      else SemFrame
  | Frame | Group | Section ->
      if has_prefix "button" || has_prefix "btn" then SemButton
      else if has_prefix "input" || has_prefix "field" then SemInput
      else if has_prefix "image" || has_prefix "img" then SemImage
      else SemFrame
  | _ -> SemFrame

(** ============== 압축 DSL 생성 ============== *)

(**
   출력 포맷:
   F(방향,w,h)[속성]{자식}
   T"텍스트"[속성]
   B"라벨"[속성]
   N"플레이스홀더"[속성]
   I(w,h)
   V"이름"

   속성 약어:
   - bg: 배경색
   - c: 텍스트색
   - r: 반경
   - p: 패딩
   - g: 갭
   - s: 폰트크기
   - w: 폰트굵기/너비
   - h: 높이
   - op: 투명도
   - sh: 그림자
*)

let rec node_to_compact ?(indent=0) node =
  let sp = String.make (indent * 2) ' ' in
  let sem = classify_node node in

  match sem with
  | SemText ->
      let content = node.characters |? "" in
      let attrs = ref [] in
      (match node.typography with
       | Some t ->
           if t.font_size <> 14. then attrs := sprintf "s:%s" (fmt_num t.font_size) :: !attrs;
           if t.font_weight <> 400 then attrs := sprintf "w:%d" t.font_weight :: !attrs;
           (match get_bg_color node.fills with  (* 텍스트 색상은 fills에서 *)
            | Some c when c <> "#000000" && c <> "#191919" -> attrs := sprintf "c:%s" c :: !attrs
            | _ -> ());
           (match t.text_align_h with
            | Center -> attrs := "a:c" :: !attrs
            | Right -> attrs := "a:r" :: !attrs
            | _ -> ())
       | None -> ());
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sT\"%s\"%s" sp content attr_str

  | SemButton ->
      let label =
        (* 버튼 내 텍스트 찾기 *)
        let rec find_text nodes =
          match nodes with
          | [] -> node.name
          | n :: rest ->
              if n.node_type = Text then n.characters |? node.name
              else find_text (n.children @ rest)
        in
        find_text node.children
      in
      let attrs = ref [] in
      (match get_bg_color node.fills with
       | Some c -> attrs := sprintf "bg:%s" c :: !attrs
       | None -> ());
      if node.border_radius > 0. then attrs := sprintf "r:%s" (fmt_num node.border_radius) :: !attrs;
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sB\"%s\"%s" sp label attr_str

  | SemInput ->
      let placeholder = node.name in
      let attrs = ref [] in
      (match node.bbox with
       | Some b -> attrs := sprintf "w:%s" (fmt_num b.width) :: !attrs
       | None -> ());
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sN\"%s\"%s" sp placeholder attr_str

  | SemImage ->
      let size_str = match node.bbox with
        | Some b -> sprintf "(%s,%s)" (fmt_num b.width) (fmt_num b.height)
        | None -> ""
      in
      sprintf "%sI%s[id:%s]" sp size_str node.id

  | SemIcon ->
      sprintf "%sV\"%s\"" sp node.name

  | SemDivider ->
      sprintf "%s---" sp

  | SemFrame ->
      let dir = match node.layout_mode with
        | Horizontal -> "row"
        | Vertical -> "col"
        | None' -> "abs"  (* absolute positioning *)
      in
      let size_str = match node.bbox with
        | Some b -> sprintf ",%s,%s" (fmt_num b.width) (fmt_num b.height)
        | None -> ""
      in
      let attrs = ref [] in
      (match get_bg_color node.fills with
       | Some c when c <> "#FFFFFF" && c <> "#FFF" -> attrs := sprintf "bg:%s" c :: !attrs
       | _ -> ());
      if node.border_radius > 0. then attrs := sprintf "r:%s" (fmt_num node.border_radius) :: !attrs;
      (match fmt_padding node.padding with
       | Some p -> attrs := sprintf "p:%s" p :: !attrs
       | None -> ());
      if node.gap > 0. then attrs := sprintf "g:%s" (fmt_num node.gap) :: !attrs;
      if node.opacity < 1. then attrs := sprintf "op:%s" (fmt_num node.opacity) :: !attrs;
      (match get_stroke_color node.strokes with
       | Some c -> attrs := sprintf "bd:%s" c :: !attrs
       | _ -> ());

      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in

      if node.children = [] then
        sprintf "%sF(%s%s)%s" sp dir size_str attr_str
      else
        let children_str =
          node.children
          |> List.filter (fun n -> n.visible)
          |> List.map (node_to_compact ~indent:(indent+1))
          |> String.concat "\n"
        in
        sprintf "%sF(%s%s)%s{\n%s\n%s}" sp dir size_str attr_str children_str sp

(** ============== Fidelity DSL 생성 (픽셀 정확도 우선) ============== *)

let rec node_to_fidelity ?(indent=0) ?origin ?(is_root=false) node =
  let sp = String.make (indent * 2) ' ' in
  let sem = classify_node node in

  let origin =
    match origin, node.bbox with
    | Some o, _ -> Some o
    | None, Some b -> Some (b.x, b.y)
    | None, None -> None
  in

  match sem with
  | SemText ->
      let content = node.characters |? "" in
      let attrs = ref [] in
      attrs := add_xy_size_attrs ?origin ~is_root !attrs node;
      (match node.typography with
       | Some t ->
           attrs := sprintf "s:%s" (fmt_num t.font_size) :: !attrs;
           attrs := sprintf "w:%d" t.font_weight :: !attrs;
           attrs := sprintf "ff:%s" t.font_family :: !attrs;
           (match t.line_height with
            | Some lh -> attrs := sprintf "lh:%s" (fmt_num lh) :: !attrs
            | None -> ());
           (match t.letter_spacing with
            | Some ls -> attrs := sprintf "ls:%s" (fmt_num ls) :: !attrs
            | None -> ());
           attrs := sprintf "a:%s" (match t.text_align_h with
             | Left -> "l" | Center -> "c" | Right -> "r" | Justified -> "j") :: !attrs
       | None -> ());
      (match get_bg_fidelity node.fills with
       | Some c -> attrs := sprintf "c:%s" c :: !attrs
       | None -> ());
      if node.opacity < 1. then attrs := sprintf "op:%s" (fmt_num node.opacity) :: !attrs;
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sT\"%s\"%s" sp content attr_str

  | SemButton ->
      let label =
        let rec find_text nodes =
          match nodes with
          | [] -> node.name
          | n :: rest ->
              if n.node_type = Text then n.characters |? node.name
              else find_text (n.children @ rest)
        in
        find_text node.children
      in
      let attrs = ref [] in
      attrs := add_xy_size_attrs ?origin ~is_root !attrs node;
      (match get_bg_fidelity node.fills with
       | Some c -> attrs := sprintf "bg:%s" c :: !attrs
       | None -> ());
      if node.border_radius > 0. then attrs := sprintf "r:%s" (fmt_num node.border_radius) :: !attrs;
      (match get_stroke_color node.strokes with
       | Some c -> attrs := sprintf "bd:%s" c :: !attrs
       | None -> ());
      if node.stroke_weight > 0. then attrs := sprintf "bw:%s" (fmt_num node.stroke_weight) :: !attrs;
      if node.opacity < 1. then attrs := sprintf "op:%s" (fmt_num node.opacity) :: !attrs;
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sB\"%s\"%s" sp label attr_str

  | SemInput ->
      let placeholder = node.name in
      let attrs = ref [] in
      attrs := add_xy_size_attrs ?origin ~is_root !attrs node;
      (match get_bg_fidelity node.fills with
       | Some c -> attrs := sprintf "bg:%s" c :: !attrs
       | None -> ());
      (match get_stroke_color node.strokes with
       | Some c -> attrs := sprintf "bd:%s" c :: !attrs
       | None -> ());
      if node.stroke_weight > 0. then attrs := sprintf "bw:%s" (fmt_num node.stroke_weight) :: !attrs;
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sN\"%s\"%s" sp placeholder attr_str

  | SemImage ->
      let attrs = ref [] in
      attrs := add_xy_size_attrs ?origin ~is_root !attrs node;
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sI[id:%s]%s" sp node.id attr_str

  | SemIcon ->
      let attrs = ref [] in
      attrs := add_xy_size_attrs ?origin ~is_root !attrs node;
      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sV\"%s\"%s" sp node.name attr_str

  | SemDivider ->
      sprintf "%s---" sp

  | SemFrame ->
      let dir = match node.layout_mode with
        | Horizontal -> "row"
        | Vertical -> "col"
        | None' -> "abs"
      in
      let size_str = match node.bbox with
        | Some b -> sprintf ",%s,%s" (fmt_num b.width) (fmt_num b.height)
        | None -> ""
      in
      let attrs = ref [] in
      attrs := add_xy_size_attrs ?origin ~is_root !attrs node;
      (match get_bg_fidelity node.fills with
       | Some c -> attrs := sprintf "bg:%s" c :: !attrs
       | None -> ());
      (match node.border_radii with
       | Some (tl, tr, br, bl) ->
           attrs := sprintf "rr:%s,%s,%s,%s" (fmt_num tl) (fmt_num tr) (fmt_num br) (fmt_num bl) :: !attrs
       | None ->
           if node.border_radius > 0. then
             attrs := sprintf "r:%s" (fmt_num node.border_radius) :: !attrs);
      (match fmt_padding node.padding with
       | Some p -> attrs := sprintf "p:%s" p :: !attrs
       | None -> ());
      attrs := sprintf "ax:%s" (align_to_code node.primary_axis_align) :: !attrs;
      attrs := sprintf "cx:%s" (align_to_code node.counter_axis_align) :: !attrs;
      attrs := sprintf "sx:%s" (sizing_to_code node.layout_sizing_h) :: !attrs;
      attrs := sprintf "sy:%s" (sizing_to_code node.layout_sizing_v) :: !attrs;
      if node.gap > 0. then attrs := sprintf "g:%s" (fmt_num node.gap) :: !attrs;
      if node.opacity < 1. then attrs := sprintf "op:%s" (fmt_num node.opacity) :: !attrs;
      (match get_stroke_color node.strokes with
       | Some c -> attrs := sprintf "bd:%s" c :: !attrs
       | None -> ());
      if node.stroke_weight > 0. then attrs := sprintf "bw:%s" (fmt_num node.stroke_weight) :: !attrs;
      if node.rotation <> 0. then attrs := sprintf "rot:%s" (fmt_num node.rotation) :: !attrs;

      let attr_str = if !attrs = [] then "" else "[" ^ String.concat "," (List.rev !attrs) ^ "]" in

      if node.children = [] then
        sprintf "%sF(%s%s)%s" sp dir size_str attr_str
      else
        let children_str =
          node.children
          |> List.filter (fun n -> n.visible)
          |> List.map (node_to_fidelity ~indent:(indent+1) ?origin ~is_root:false)
          |> String.concat "\n"
        in
        sprintf "%sF(%s%s)%s{\n%s\n%s}" sp dir size_str attr_str children_str sp

(** ============== HTML 생성 (참고용) ============== *)

(** P0-3: Effects (shadow, blur) → CSS 변환 *)
let effects_to_css ?(precise=false) (effects : Figma_types.fx list) =
  let shadow_parts = ref [] in
  let filter_parts = ref [] in

  effects |> List.iter (fun (eff : Figma_types.fx) ->
    if eff.visible then
      match eff.fx_type with
      | DropShadow ->
          let (ox, oy) = eff.offset |? (0., 0.) in
          let blur = eff.radius in
          let spread = eff.spread |? 0. in
          let color = match eff.color with
            | Some c ->
                if precise then
                  sprintf "rgba(%.0f,%.0f,%.0f,%.2f)"
                    (c.r *. 255.) (c.g *. 255.) (c.b *. 255.) c.a
                else
                  sprintf "rgba(%.0f,%.0f,%.0f,%.1f)"
                    (c.r *. 255.) (c.g *. 255.) (c.b *. 255.) c.a
            | None -> "rgba(0,0,0,0.25)"
          in
          shadow_parts := sprintf "%.0fpx %.0fpx %.0fpx %.0fpx %s"
            ox oy blur spread color :: !shadow_parts
      | InnerShadow ->
          let (ox, oy) = eff.offset |? (0., 0.) in
          let blur = eff.radius in
          let spread = eff.spread |? 0. in
          let color = match eff.color with
            | Some c ->
                sprintf "rgba(%.0f,%.0f,%.0f,%.2f)"
                  (c.r *. 255.) (c.g *. 255.) (c.b *. 255.) c.a
            | None -> "rgba(0,0,0,0.25)"
          in
          shadow_parts := sprintf "inset %.0fpx %.0fpx %.0fpx %.0fpx %s"
            ox oy blur spread color :: !shadow_parts
      | LayerBlur ->
          filter_parts := sprintf "filter:blur(%.0fpx)" eff.radius :: !filter_parts
      | BackgroundBlur ->
          filter_parts := sprintf "backdrop-filter:blur(%.0fpx)" eff.radius :: !filter_parts
  );

  let result = ref [] in
  (match !shadow_parts with
   | [] -> ()
   | parts -> result := sprintf "box-shadow:%s" (String.concat "," (List.rev parts)) :: !result);
  (match !filter_parts with
   | [] -> ()
   | parts -> result := (String.concat ";" (List.rev parts)) :: !result);

  String.concat ";" (List.rev !result)

(** 정밀 모드: rgb() 포맷 + 반올림 색상 *)
let style_to_css ?(precise=false) node =
  let parts = ref [] in
  let get_color = if precise then get_bg_color_precise else get_bg_color in

  (* 크기 *)
  (match node.bbox with
   | Some b ->
       parts := sprintf "width:%.0fpx" b.width :: !parts;
       parts := sprintf "height:%.0fpx" b.height :: !parts
   | None -> ());

  (* 배경 - precise 모드에서는 rgb() 사용 *)
  (match get_color node.fills with
   | Some c -> parts := sprintf "background:%s" c :: !parts
   | None -> ());

  (* 테두리 *)
  if node.border_radius > 0. then
    parts := sprintf "border-radius:%.0fpx" node.border_radius :: !parts;

  (match get_stroke_color node.strokes with
   | Some c when node.stroke_weight > 0. ->
       parts := sprintf "border:%.0fpx solid %s" node.stroke_weight c :: !parts
   | _ -> ());

  (* 패딩 *)
  let (pt, pr, pb, pl) = node.padding in
  if pt > 0. || pr > 0. || pb > 0. || pl > 0. then
    parts := sprintf "padding:%.0fpx %.0fpx %.0fpx %.0fpx" pt pr pb pl :: !parts;

  (* 투명도 *)
  if node.opacity < 1. then
    parts := sprintf "opacity:%.2f" node.opacity :: !parts;

  (* P0-3: Effects (shadow, blur) *)
  let effects_css = effects_to_css ~precise node.effects in
  if effects_css <> "" then
    parts := effects_css :: !parts;

  String.concat ";" (List.rev !parts)

let typography_to_css typo =
  let parts = ref [] in
  parts := sprintf "font-size:%.0fpx" typo.font_size :: !parts;
  if typo.font_weight <> 400 then
    parts := sprintf "font-weight:%d" typo.font_weight :: !parts;
  parts := sprintf "font-family:'%s',sans-serif" typo.font_family :: !parts;
  (* line-height: 텍스트 높이 정확도에 필수 *)
  (match typo.line_height with
   | Some lh -> parts := sprintf "line-height:%.0fpx" lh :: !parts
   | None -> ());
  (* letter-spacing: 텍스트 폭 정확도에 필수 *)
  (match typo.letter_spacing with
   | Some ls when ls <> 0. -> parts := sprintf "letter-spacing:%.2fpx" ls :: !parts
   | _ -> ());
  (match typo.text_align_h with
   | Center -> parts := "text-align:center" :: !parts
   | Right -> parts := "text-align:right" :: !parts
   | Justified -> parts := "text-align:justify" :: !parts
   | Left -> ());
  String.concat ";" (List.rev !parts)

(** HTML 생성 - precise=true 시 rgb() 포맷 + 정확한 typography 사용 *)
let rec node_to_html ?(indent=0) ?(precise=true) node =
  let sp = String.make (indent * 2) ' ' in
  let sem = classify_node node in

  match sem with
  | SemText ->
      let content = node.characters |? "" in
      let css = match node.typography with
        | Some t ->
            let base = typography_to_css t in
            (* 텍스트 색상도 precise 모드 사용 *)
            let color = (if precise then get_bg_color_precise else get_bg_color) node.fills
              |> Option.map (sprintf "color:%s") |? "" in
            if color = "" then base else base ^ ";" ^ color
        | None -> ""
      in
      sprintf "%s<span style=\"%s\">%s</span>" sp css content

  | SemButton ->
      let label =
        let rec find_text nodes =
          match nodes with
          | [] -> node.name
          | n :: rest ->
              if n.node_type = Text then n.characters |? node.name
              else find_text (n.children @ rest)
        in
        find_text node.children
      in
      let css = style_to_css ~precise node in
      sprintf "%s<button style=\"%s\">%s</button>" sp css label

  | SemInput ->
      let css = style_to_css ~precise node in
      sprintf "%s<input placeholder=\"%s\" style=\"%s\"/>" sp node.name css

  | SemImage ->
      let css = style_to_css ~precise node in
      sprintf "%s<img src=\"\" alt=\"%s\" style=\"%s\"/>" sp node.name css

  | SemIcon | SemDivider ->
      let css = style_to_css ~precise node in
      sprintf "%s<div style=\"%s\"><!-- %s --></div>" sp css node.name

  | SemFrame ->
      let flex_dir = match node.layout_mode with
        | Horizontal -> ""
        | Vertical -> "flex-direction:column;"
        | None' -> "position:relative;"
      in
      (* P0-1: justify-content from primaryAxisAlignItems *)
      let justify = match node.primary_axis_align with
        | Min -> ""  (* flex-start is default *)
        | Center -> "justify-content:center;"
        | Max -> "justify-content:flex-end;"
        | SpaceBetween -> "justify-content:space-between;"
        | Baseline -> ""  (* not applicable for main axis *)
      in
      (* P0-2: align-items from counterAxisAlignItems *)
      let align = match node.counter_axis_align with
        | Min -> ""  (* flex-start is default for stretch behavior *)
        | Center -> "align-items:center;"
        | Max -> "align-items:flex-end;"
        | SpaceBetween -> ""  (* not applicable for cross axis *)
        | Baseline -> "align-items:baseline;"
      in
      let base_css = style_to_css ~precise node in
      let css = sprintf "display:flex;%s%s%s%s" flex_dir justify align base_css in

      if node.gap > 0. then
        let css = css ^ sprintf ";gap:%.0fpx" node.gap in
        if node.children = [] then
          sprintf "%s<div style=\"%s\"></div>" sp css
        else
          let children_html =
            node.children
            |> List.filter (fun n -> n.visible)
            |> List.map (node_to_html ~indent:(indent+1) ~precise)
            |> String.concat "\n"
          in
          sprintf "%s<div style=\"%s\">\n%s\n%s</div>" sp css children_html sp
      else
        if node.children = [] then
          sprintf "%s<div style=\"%s\"></div>" sp css
        else
          let children_html =
            node.children
            |> List.filter (fun n -> n.visible)
            |> List.map (node_to_html ~indent:(indent+1) ~precise)
            |> String.concat "\n"
          in
          sprintf "%s<div style=\"%s\">\n%s\n%s</div>" sp css children_html sp

(** ============== 메인 출력 함수 ============== *)

(** 압축 통계 분석 *)
type compression_stats = {
  original_chars: int;
  compact_chars: int;
  verbose_chars: int;
  compression_ratio: float;
}

(** Verbose 모드 (모든 속성 출력) *)
let rec node_to_verbose ?(indent=0) node =
  let sp = String.make (indent * 2) ' ' in
  let sem = classify_node node in

  match sem with
  | SemText ->
      let content = node.characters |? "" in
      let attrs = ref [] in
      (match node.typography with
       | Some t ->
           attrs := sprintf "s:%s" (fmt_num t.font_size) :: !attrs;
           attrs := sprintf "w:%d" t.font_weight :: !attrs;
           attrs := sprintf "f:%s" t.font_family :: !attrs;
           (match get_bg_color node.fills with
            | Some c -> attrs := sprintf "c:%s" c :: !attrs
            | None -> attrs := "c:#000" :: !attrs);
           attrs := sprintf "a:%s" (match t.text_align_h with
             | Left -> "l" | Center -> "c" | Right -> "r" | Justified -> "j") :: !attrs
       | None -> ());
      attrs := sprintf "op:%s" (fmt_num node.opacity) :: !attrs;
      attrs := sprintf "vis:%b" node.visible :: !attrs;
      let attr_str = "[" ^ String.concat "," (List.rev !attrs) ^ "]" in
      sprintf "%sT\"%s\"%s" sp content attr_str

  | SemFrame ->
      let dir = match node.layout_mode with
        | Horizontal -> "row"
        | Vertical -> "col"
        | None' -> "abs"
      in
      let size_str = match node.bbox with
        | Some b -> sprintf ",%s,%s" (fmt_num b.width) (fmt_num b.height)
        | None -> ",0,0"
      in
      let attrs = ref [] in
      (match get_bg_color node.fills with
       | Some c -> attrs := sprintf "bg:%s" c :: !attrs
       | None -> attrs := "bg:none" :: !attrs);
      attrs := sprintf "r:%s" (fmt_num node.border_radius) :: !attrs;
      let (pt, pr, pb, pl) = node.padding in
      attrs := sprintf "p:%s,%s,%s,%s" (fmt_num pt) (fmt_num pr) (fmt_num pb) (fmt_num pl) :: !attrs;
      attrs := sprintf "g:%s" (fmt_num node.gap) :: !attrs;
      attrs := sprintf "op:%s" (fmt_num node.opacity) :: !attrs;
      (match get_stroke_color node.strokes with
       | Some c -> attrs := sprintf "bd:%s" c :: !attrs
       | None -> ());
      let attr_str = "[" ^ String.concat "," (List.rev !attrs) ^ "]" in

      if node.children = [] then
        sprintf "%sF(%s%s)%s" sp dir size_str attr_str
      else
        let children_str =
          node.children
          |> List.filter (fun n -> n.visible)
          |> List.map (node_to_verbose ~indent:(indent+1))
          |> String.concat "\n"
        in
        sprintf "%sF(%s%s)%s{\n%s\n%s}" sp dir size_str attr_str children_str sp

  | _ -> node_to_compact ~indent node  (* 다른 타입은 compact와 동일 *)

(** 압축 효율 분석 *)
let analyze_compression node original_json =
  let compact = node_to_compact node in
  let verbose = node_to_verbose node in
  let original_len = String.length original_json in
  let compact_len = String.length compact in
  let verbose_len = String.length verbose in
  let ratio = 1.0 -. (float_of_int compact_len /. float_of_int original_len) in
  {
    original_chars = original_len;
    compact_chars = compact_len;
    verbose_chars = verbose_len;
    compression_ratio = ratio;
  }

let format_stats stats =
  sprintf "=== Compression Analysis ===\nOriginal JSON: %d chars\nVerbose DSL:   %d chars (%.1f%% of original)\nCompact DSL:   %d chars (%.1f%% of original)\nToken savings: %.1f%%\n"
    stats.original_chars
    stats.verbose_chars
    (float_of_int stats.verbose_chars /. float_of_int stats.original_chars *. 100.)
    stats.compact_chars
    (float_of_int stats.compact_chars /. float_of_int stats.original_chars *. 100.)
    (stats.compression_ratio *. 100.)

(** 기본 Compact DSL (memoization 없음) *)
let generate_compact node =
  node_to_compact node

(** Memoized Compact DSL - 반복 스타일을 변수로 추출 *)
let generate_compact_memoized ?(threshold=2) node =
  (* 1단계: 스타일 수집 *)
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.collect_from_node counter node;

  (* 2단계: threshold 이상 반복되는 스타일만 변수화 *)
  let reg = StyleRegistry.finalize counter threshold in

  (* 3단계: DSL 생성 + 변수 치환 *)
  let dsl = node_to_compact node in
  let dsl_with_vars = StyleRegistry.apply_to_dsl reg dsl in

  (* 4단계: 변수 정의 헤더 + 치환된 DSL *)
  let defs = StyleRegistry.to_defs reg in
  defs ^ dsl_with_vars

(** 스마트 Compact - 큰 트리면 자동으로 memoization 적용 *)
let generate_compact_smart node =
  (* 노드 수 카운트 *)
  let rec count_nodes n acc =
    1 + List.fold_left (fun a child -> count_nodes child a) acc n.children
  in
  let total = count_nodes node 0 in

  (* 20개 이상 노드면 memoization 적용 *)
  if total >= 20 then generate_compact_memoized ~threshold:2 node
  else generate_compact node

let generate_verbose node =
  node_to_verbose node

let json_member key json =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let json_has_key key json =
  match json with
  | `Assoc fields -> List.exists (fun (k, _) -> k = key) fields
  | _ -> false

let json_pick_keys keys json =
  List.filter_map (fun key ->
    match json_member key json with
    | Some v -> Some (key, v)
    | None -> None
  ) keys

let json_missing_keys keys json =
  List.filter (fun key -> not (json_has_key key json)) keys

let json_list_of_strings xs =
  `List (List.map (fun s -> `String s) xs)

let json_has_any_key keys json =
  List.exists (fun key -> json_has_key key json) keys

let rec json_collect_image_refs json =
  match json with
  | `Assoc items ->
      List.fold_left (fun acc (key, value) ->
        let acc =
          if key = "imageRef" then
            match value with
            | `String s -> s :: acc
            | _ -> acc
          else acc
        in
        List.rev_append (json_collect_image_refs value) acc
      ) [] items
  | `List items ->
      List.fold_left (fun acc item ->
        List.rev_append (json_collect_image_refs item) acc
      ) [] items
  | _ -> []

let extract_root_node json =
  match json_member "document" json with
  | Some doc -> doc
  | None ->
      (match json_member "nodes" json with
       | Some (`Assoc nodes) -> (
           match nodes with
           | [(_, node)] -> (match json_member "document" node with Some doc -> doc | None -> json)
           | _ -> json
         )
       | _ -> json)

let rec fidelity_node json =
  let node_type =
    match json_member "type" json with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let layout_mode =
    match json_member "layoutMode" json with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let meta_keys = [
    "id"; "name"; "type"; "visible"; "locked";
    "prototypeStartNodeID"; "flowStartingPoints"; "prototypeDevice";
    "interactions"; "transitionNodeID"; "measurements";
    "sectionContentsHidden"; "devStatus";
    "styles"; "exportSettings";
  ] in
  let instance_keys = [
    "componentId"; "componentProperties"; "componentPropertyReferences";
    "componentPropertyDefinitions"; "componentSetId"; "variantProperties"; "isExposedInstance";
  ] in
  let variables_keys = [
    "boundVariables";
  ] in
  let geometry_keys = [
    "absoluteBoundingBox"; "absoluteRenderBounds";
    "relativeTransform"; "absoluteTransform"; "size"; "rotation";
  ] in
  let vector_keys = [
    "strokeGeometry"; "fillGeometry"; "vectorPaths"; "vectorNetwork";
  ] in
  let layout_keys = [
    "constraints"; "layoutMode"; "layoutWrap";
    "primaryAxisSizingMode"; "counterAxisSizingMode";
    "primaryAxisAlignItems"; "counterAxisAlignItems";
    "layoutAlign"; "layoutGrow"; "layoutPositioning";
    "layoutSizingHorizontal"; "layoutSizingVertical";
    "itemSpacing";
    "paddingTop"; "paddingRight"; "paddingBottom"; "paddingLeft";
    "minWidth"; "maxWidth"; "minHeight"; "maxHeight";
    "clipsContent"; "layoutGrids";
    "itemReverseZIndex"; "strokesIncludedInLayout"; "scrollBehavior";
  ] in
  let paint_keys = [
    "fills"; "strokes"; "strokeWeight"; "strokeWeights";
    "strokeTopWeight"; "strokeRightWeight"; "strokeBottomWeight"; "strokeLeftWeight";
    "strokeAlign"; "strokeDashes"; "strokeCap"; "strokeJoin";
    "cornerRadius"; "rectangleCornerRadii"; "cornerSmoothing";
    "isMask"; "maskType";
    "background"; "backgroundColor";
  ] in
  let effects_keys = [
    "opacity"; "blendMode"; "effects";
  ] in
  let text_keys = [
    "characters"; "style";
    "textAutoResize"; "textTruncation"; "maxLines"; "hyperlink";
  ] in
  let text_segments_keys = [
    "styleOverrideTable"; "characterStyleOverrides"; "styledTextSegments";
  ] in
  let children_present = json_has_key "children" json in
  let children =
    match json_member "children" json with
    | Some (`List items) ->
        items
        |> List.filter_map (function
          | `Assoc _ as child -> Some (fidelity_node child)
          | _ -> None
        )
    | _ -> []
  in
  let children_count =
    match json_member "children" json with
    | Some (`List items) -> List.length items
    | _ -> 0
  in
  let is_text = node_type = Some "TEXT" in
  let layout_applicable =
    json_has_any_key layout_keys json
    || match layout_mode with
       | Some mode -> mode <> "NONE"
       | None -> false
  in
  let paint_applicable = json_has_any_key paint_keys json in
  let effects_applicable = json_has_any_key effects_keys json in
  let vector_applicable =
    json_has_any_key vector_keys json
    || match node_type with
       | Some ("VECTOR" | "BOOLEAN_OPERATION" | "STAR" | "LINE" | "ELLIPSE" | "POLYGON") -> true
       | _ -> false
  in
  let instance_applicable =
    match node_type with
    | Some ("COMPONENT" | "COMPONENT_SET" | "INSTANCE") -> true
    | _ -> false
  in
  let variables_applicable = json_has_key "boundVariables" json in
  let structure_applicable =
    match node_type with
    | Some ("DOCUMENT" | "CANVAS") -> true
    | _ -> false
  in
  let image_refs =
    json_collect_image_refs json
    |> List.sort_uniq String.compare
  in
  let meta = json_pick_keys meta_keys json in
  let instance = if instance_applicable then json_pick_keys instance_keys json else [] in
  let variables = if variables_applicable then json_pick_keys variables_keys json else [] in
  let geometry = json_pick_keys geometry_keys json in
  let vector = if vector_applicable then json_pick_keys vector_keys json else [] in
  let layout = if layout_applicable then json_pick_keys layout_keys json else [] in
  let paint = if paint_applicable then json_pick_keys paint_keys json else [] in
  let effects = if effects_applicable then json_pick_keys effects_keys json else [] in
  let text = if is_text then json_pick_keys text_keys json else [] in
  let text_segments = if is_text then json_pick_keys text_segments_keys json else [] in
  let assets = `Assoc [
    ("image_refs", json_list_of_strings image_refs);
  ] in
  let structure = `Assoc [
    ("children_present", `Bool children_present);
    ("children_count", `Int children_count);
  ] in
  let meta_missing = json_missing_keys meta_keys json in
  let instance_missing = if instance_applicable then json_missing_keys instance_keys json else [] in
  let variables_missing = if variables_applicable then json_missing_keys variables_keys json else [] in
  let geometry_missing = json_missing_keys geometry_keys json in
  let vector_missing = if vector_applicable then json_missing_keys vector_keys json else [] in
  let layout_missing = if layout_applicable then json_missing_keys layout_keys json else [] in
  let paint_missing = if paint_applicable then json_missing_keys paint_keys json else [] in
  let effects_missing = if effects_applicable then json_missing_keys effects_keys json else [] in
  let text_missing = if is_text then json_missing_keys text_keys json else [] in
  let text_segments_missing = if is_text then json_missing_keys text_segments_keys json else [] in
  let assets_missing = if image_refs = [] then [] else ["image_fills"] in
  let structure_missing = if structure_applicable && not children_present then ["children"] else [] in
  `Assoc [
    ("meta", `Assoc meta);
    ("meta_missing", json_list_of_strings meta_missing);
    ("instance", `Assoc instance);
    ("instance_missing", json_list_of_strings instance_missing);
    ("variables", `Assoc variables);
    ("variables_missing", json_list_of_strings variables_missing);
    ("geometry", `Assoc geometry);
    ("geometry_missing", json_list_of_strings geometry_missing);
    ("vector", `Assoc vector);
    ("vector_missing", json_list_of_strings vector_missing);
    ("layout", `Assoc layout);
    ("layout_missing", json_list_of_strings layout_missing);
    ("paint", `Assoc paint);
    ("paint_missing", json_list_of_strings paint_missing);
    ("effects", `Assoc effects);
    ("effects_missing", json_list_of_strings effects_missing);
    ("text", `Assoc text);
    ("text_missing", json_list_of_strings text_missing);
    ("text_segments", `Assoc text_segments);
    ("text_segments_missing", json_list_of_strings text_segments_missing);
    ("assets", assets);
    ("assets_missing", json_list_of_strings assets_missing);
    ("structure", structure);
    ("structure_missing", json_list_of_strings structure_missing);
    ("children_present", `Bool children_present);
    ("children", `List children);
  ]

let generate_fidelity json =
  let root = extract_root_node json in
  Yojson.Safe.to_string (fidelity_node root)

let generate_html node =
  "<!DOCTYPE html>\n<html><head><meta charset=\"UTF-8\"></head><body>\n"
  ^ node_to_html node
  ^ "\n</body></html>"

(** 플랫 HTML 생성 (Visual Verification용)
    Figma 계층 구조를 무시하고 시각적으로 동일한 단순 HTML 생성
    - 텍스트 + 배경 + 크기만으로 렌더링
    - 99% SSIM 달성을 위한 최적화 *)
let generate_flat_html node =
  let open Printf in
  (* 첫 번째 텍스트 노드 찾기 *)
  let rec find_first_text n =
    if n.node_type = Text then Some n
    else List.find_map find_first_text n.children
  in
  (* 가장 안쪽의 배경색이 있는 프레임 찾기 *)
  let rec find_inner_frame n =
    let has_bg = match n.fills with
      | [] -> false
      | fills -> List.exists (fun (p:paint) -> p.paint_type = Solid && p.visible) fills
    in
    if has_bg then
      (* 자식 중 배경색 있는 프레임이 있으면 더 깊이 탐색 *)
      match List.find_opt (fun c ->
        List.exists (fun (p:paint) -> p.paint_type = Solid && p.visible) c.fills
      ) n.children with
      | Some inner -> find_inner_frame inner
      | None -> n
    else
      match n.children with
      | [] -> n
      | _ -> match List.find_opt (fun c -> c.visible) n.children with
             | Some c -> find_inner_frame c
             | None -> n
  in

  let text_node = find_first_text node in
  let inner = find_inner_frame node in
  let outer_bbox = node.bbox in
  let inner_bbox = inner.bbox in

  (* 스타일 생성 *)
  let bg = get_bg_color_precise inner.fills in
  let (outer_w, outer_h) = match outer_bbox with
    | Some b -> (b.width, b.height)
    | None -> (343., 48.)
  in
  let (inner_w, inner_h) = match inner_bbox with
    | Some b -> (b.width, b.height)
    | None -> (outer_w, outer_h)
  in

  (* 텍스트 스타일 *)
  let text_style, text_content = match text_node with
    | Some tn ->
        let typo_css = match tn.typography with
          | Some t -> typography_to_css t
          | None -> "font-size:16px;font-family:'Noto Sans KR',sans-serif"
        in
        let color = get_bg_color_precise tn.fills
          |> Option.map (fun c -> sprintf ";color:%s" c)
          |> Option.value ~default:";color:#FFFFFF"
        in
        (typo_css ^ color, tn.characters |? "")
    | None -> ("font-size:16px;color:#FFFFFF", node.name)
  in

  (* HTML 생성 - 외부 컨테이너 + 내부 버튼 구조 *)
  if Float.abs (outer_w -. inner_w) > 1. || Float.abs (outer_h -. inner_h) > 1. then
    (* 외부/내부 크기가 다름 → 2단계 구조 *)
    sprintf {|<!DOCTYPE html><html><head><meta charset="UTF-8"></head><body style="margin:0;padding:0;">
<div style="display:flex;align-items:center;justify-content:center;width:%.0fpx;height:%.0fpx;box-sizing:border-box;">
  <div style="display:flex;align-items:center;justify-content:center;width:%.0fpx;height:%.0fpx;%s%s">
    <span style="%s">%s</span>
  </div>
</div>
</body></html>|}
      outer_w outer_h
      inner_w inner_h
      (Option.map (sprintf "background:%s;") bg |> Option.value ~default:"")
      (if inner.border_radius > 0. then sprintf "border-radius:%.0fpx;" inner.border_radius else "")
      text_style text_content
  else
    (* 단일 구조 *)
    sprintf {|<!DOCTYPE html><html><head><meta charset="UTF-8"></head><body style="margin:0;padding:0;">
<div style="display:flex;align-items:center;justify-content:center;width:%.0fpx;height:%.0fpx;%s%s">
  <span style="%s">%s</span>
</div>
</body></html>|}
      outer_w outer_h
      (Option.map (sprintf "background:%s;") bg |> Option.value ~default:"")
      (if inner.border_radius > 0. then sprintf "border-radius:%.0fpx;" inner.border_radius else "")
      text_style text_content

(** 화면 목록 추출 (최상위 Frame들) *)
let extract_screens node =
  match node.node_type with
  | Document | Canvas ->
      node.children
      |> List.filter (fun n -> n.node_type = Frame && n.visible)
      |> List.map (fun n -> (n.name, n.id))
  | _ -> [(node.name, node.id)]

(** 컴포넌트 분리 모드 *)
let split_to_components node =
  let screens = match node.node_type with
    | Document | Canvas -> node.children |> List.filter (fun n -> n.visible)
    | Frame -> node.children |> List.filter (fun n -> n.visible)
    | _ -> [node]
  in
  screens
  |> List.mapi (fun i n ->
      sprintf "## %d. %s\n%s" (i+1) n.name (node_to_compact n)
    )
  |> String.concat "\n\n"
