(** Figma 노드 비교 엔진 - Web/Mobile 일관성 검사 *)

open Figma_types

(** ============== 비교 결과 타입 ============== *)

type difference_severity =
  | Critical   (** 심각: 완전히 다른 구조 *)
  | Major      (** 주요: 크기/색상 차이가 큼 *)
  | Minor      (** 경미: 작은 차이 *)
  | Info       (** 정보: 참고 사항 *)

type difference = {
  category: string;       (** size, color, typography, layout, structure *)
  property: string;       (** 비교한 속성 이름 *)
  value_a: string;        (** 첫 번째 노드 값 *)
  value_b: string;        (** 두 번째 노드 값 *)
  severity: difference_severity;
  message: string;        (** 사람이 읽을 수 있는 설명 *)
}

type comparison_result = {
  node_a_name: string;
  node_b_name: string;
  node_a_id: string;
  node_b_id: string;
  differences: difference list;
  similarity_score: float;  (** 0.0 ~ 1.0 *)
}

(** ============== 유틸리티 함수 ============== *)

let severity_to_string = function
  | Critical -> "CRITICAL"
  | Major -> "MAJOR"
  | Minor -> "MINOR"
  | Info -> "INFO"

let severity_weight = function
  | Critical -> 1.0
  | Major -> 0.5
  | Minor -> 0.2
  | Info -> 0.0

(** 부동소수점 비교 (허용 오차) *)
let float_eq ?(tolerance=0.5) a b =
  Float.abs (a -. b) <= tolerance

(** 퍼센트 차이 계산 *)
let percent_diff a b =
  if a = 0.0 && b = 0.0 then 0.0
  else if a = 0.0 then 100.0
  else Float.abs ((b -. a) /. a *. 100.0)

(** ============== 크기 비교 ============== *)

let compare_size node_a node_b =
  match node_a.bbox, node_b.bbox with
  | Some ba, Some bb ->
      let diffs = ref [] in

      (* 너비 비교 *)
      let width_diff = percent_diff ba.width bb.width in
      if not (float_eq ba.width bb.width) then begin
        let severity =
          if width_diff > 20.0 then Major
          else if width_diff > 5.0 then Minor
          else Info
        in
        diffs := {
          category = "size";
          property = "width";
          value_a = Printf.sprintf "%.0f" ba.width;
          value_b = Printf.sprintf "%.0f" bb.width;
          severity;
          message = Printf.sprintf "너비 차이: %.1f%% (%.0f → %.0f)"
            width_diff ba.width bb.width;
        } :: !diffs
      end;

      (* 높이 비교 *)
      let height_diff = percent_diff ba.height bb.height in
      if not (float_eq ba.height bb.height) then begin
        let severity =
          if height_diff > 20.0 then Major
          else if height_diff > 5.0 then Minor
          else Info
        in
        diffs := {
          category = "size";
          property = "height";
          value_a = Printf.sprintf "%.0f" ba.height;
          value_b = Printf.sprintf "%.0f" bb.height;
          severity;
          message = Printf.sprintf "높이 차이: %.1f%% (%.0f → %.0f)"
            height_diff ba.height bb.height;
        } :: !diffs
      end;

      (* 가로세로 비율 비교 *)
      let ratio_a = ba.width /. ba.height in
      let ratio_b = bb.width /. bb.height in
      if not (float_eq ~tolerance:0.1 ratio_a ratio_b) then begin
        diffs := {
          category = "size";
          property = "aspect_ratio";
          value_a = Printf.sprintf "%.2f" ratio_a;
          value_b = Printf.sprintf "%.2f" ratio_b;
          severity = Minor;
          message = Printf.sprintf "가로세로 비율 차이: %.2f → %.2f" ratio_a ratio_b;
        } :: !diffs
      end;

      !diffs
  | _ -> []

(** ============== 색상 비교 ============== *)

let rgba_to_hex (c: rgba) =
  let r = int_of_float (c.r *. 255.) in
  let g = int_of_float (c.g *. 255.) in
  let b = int_of_float (c.b *. 255.) in
  Printf.sprintf "#%02X%02X%02X" r g b

let color_distance (c1: rgba) (c2: rgba) =
  (* 단순 RGB 거리 - 더 정확한 Delta E가 필요하면 확장 *)
  let dr = (c1.r -. c2.r) *. 255. in
  let dg = (c1.g -. c2.g) *. 255. in
  let db = (c1.b -. c2.b) *. 255. in
  Float.sqrt (dr *. dr +. dg *. dg +. db *. db)

let compare_colors node_a node_b =
  let get_first_solid_color fills =
    List.find_map (fun p ->
      match p.paint_type, p.color with
      | Solid, Some c -> Some c
      | _ -> None
    ) fills
  in

  match get_first_solid_color node_a.fills, get_first_solid_color node_b.fills with
  | Some ca, Some cb ->
      let dist = color_distance ca cb in
      if dist > 10.0 then
        let severity = if dist > 50.0 then Major else Minor in
        [{
          category = "color";
          property = "fill";
          value_a = rgba_to_hex ca;
          value_b = rgba_to_hex cb;
          severity;
          message = Printf.sprintf "배경색 차이: %s → %s (거리: %.1f)"
            (rgba_to_hex ca) (rgba_to_hex cb) dist;
        }]
      else []
  | None, Some cb ->
      [{
        category = "color";
        property = "fill";
        value_a = "없음";
        value_b = rgba_to_hex cb;
        severity = Minor;
        message = Printf.sprintf "A는 배경색 없음, B는 %s" (rgba_to_hex cb);
      }]
  | Some ca, None ->
      [{
        category = "color";
        property = "fill";
        value_a = rgba_to_hex ca;
        value_b = "없음";
        severity = Minor;
        message = Printf.sprintf "A는 %s, B는 배경색 없음" (rgba_to_hex ca);
      }]
  | None, None -> []

(** ============== 타이포그래피 비교 ============== *)

let compare_typography node_a node_b =
  match node_a.typography, node_b.typography with
  | Some ta, Some tb ->
      let diffs = ref [] in

      (* 폰트 크기 *)
      let size_diff = percent_diff ta.font_size tb.font_size in
      if size_diff > 5.0 then begin
        let severity = if size_diff > 20.0 then Major else Minor in
        diffs := {
          category = "typography";
          property = "font_size";
          value_a = Printf.sprintf "%.0f" ta.font_size;
          value_b = Printf.sprintf "%.0f" tb.font_size;
          severity;
          message = Printf.sprintf "폰트 크기 차이: %.0f → %.0f (%.1f%%)"
            ta.font_size tb.font_size size_diff;
        } :: !diffs
      end;

      (* 폰트 패밀리 *)
      if ta.font_family <> tb.font_family then begin
        diffs := {
          category = "typography";
          property = "font_family";
          value_a = ta.font_family;
          value_b = tb.font_family;
          severity = Minor;
          message = Printf.sprintf "폰트 패밀리: %s → %s" ta.font_family tb.font_family;
        } :: !diffs
      end;

      (* 폰트 무게 *)
      if ta.font_weight <> tb.font_weight then begin
        diffs := {
          category = "typography";
          property = "font_weight";
          value_a = string_of_int ta.font_weight;
          value_b = string_of_int tb.font_weight;
          severity = Info;
          message = Printf.sprintf "폰트 무게: %d → %d" ta.font_weight tb.font_weight;
        } :: !diffs
      end;

      !diffs
  | Some _, None ->
      [{
        category = "typography";
        property = "presence";
        value_a = "있음";
        value_b = "없음";
        severity = Major;
        message = "A는 텍스트 스타일 있음, B는 없음";
      }]
  | None, Some _ ->
      [{
        category = "typography";
        property = "presence";
        value_a = "없음";
        value_b = "있음";
        severity = Major;
        message = "A는 텍스트 스타일 없음, B는 있음";
      }]
  | None, None -> []

(** ============== 레이아웃 비교 ============== *)

let layout_mode_to_string = function
  | Horizontal -> "Horizontal"
  | Vertical -> "Vertical"
  | None' -> "None"

let compare_layout node_a node_b =
  let diffs = ref [] in

  (* 레이아웃 모드 *)
  if node_a.layout_mode <> node_b.layout_mode then begin
    diffs := {
      category = "layout";
      property = "layout_mode";
      value_a = layout_mode_to_string node_a.layout_mode;
      value_b = layout_mode_to_string node_b.layout_mode;
      severity = Major;
      message = Printf.sprintf "레이아웃 모드: %s → %s"
        (layout_mode_to_string node_a.layout_mode)
        (layout_mode_to_string node_b.layout_mode);
    } :: !diffs
  end;

  (* 간격 (gap) *)
  if not (float_eq node_a.gap node_b.gap) then begin
    diffs := {
      category = "layout";
      property = "gap";
      value_a = Printf.sprintf "%.0f" node_a.gap;
      value_b = Printf.sprintf "%.0f" node_b.gap;
      severity = Minor;
      message = Printf.sprintf "간격: %.0f → %.0f" node_a.gap node_b.gap;
    } :: !diffs
  end;

  (* 패딩 - 튜플 (top, right, bottom, left) *)
  let (pa_t, pa_r, pa_b, pa_l) = node_a.padding in
  let (pb_t, pb_r, pb_b, pb_l) = node_b.padding in
  if pa_t <> pb_t || pa_r <> pb_r || pa_b <> pb_b || pa_l <> pb_l then begin
    diffs := {
      category = "layout";
      property = "padding";
      value_a = Printf.sprintf "%.0f,%.0f,%.0f,%.0f" pa_t pa_r pa_b pa_l;
      value_b = Printf.sprintf "%.0f,%.0f,%.0f,%.0f" pb_t pb_r pb_b pb_l;
      severity = Minor;
      message = Printf.sprintf "패딩: [%.0f,%.0f,%.0f,%.0f] → [%.0f,%.0f,%.0f,%.0f]"
        pa_t pa_r pa_b pa_l pb_t pb_r pb_b pb_l;
    } :: !diffs
  end;

  !diffs

(** ============== 구조 비교 ============== *)

let compare_structure node_a node_b =
  let diffs = ref [] in
  let count_a = List.length node_a.children in
  let count_b = List.length node_b.children in

  if count_a <> count_b then begin
    let severity =
      if abs (count_a - count_b) > 3 then Major
      else Minor
    in
    diffs := {
      category = "structure";
      property = "children_count";
      value_a = string_of_int count_a;
      value_b = string_of_int count_b;
      severity;
      message = Printf.sprintf "자식 노드 수: %d → %d" count_a count_b;
    } :: !diffs
  end;

  (* 노드 타입 *)
  if node_a.node_type <> node_b.node_type then begin
    diffs := {
      category = "structure";
      property = "node_type";
      value_a = Figma_query.node_type_to_string node_a.node_type;
      value_b = Figma_query.node_type_to_string node_b.node_type;
      severity = Critical;
      message = Printf.sprintf "노드 타입: %s → %s"
        (Figma_query.node_type_to_string node_a.node_type)
        (Figma_query.node_type_to_string node_b.node_type);
    } :: !diffs
  end;

  !diffs

(** ============== 전체 비교 ============== *)

let compare_nodes node_a node_b =
  let all_diffs =
    compare_structure node_a node_b @
    compare_size node_a node_b @
    compare_colors node_a node_b @
    compare_typography node_a node_b @
    compare_layout node_a node_b
  in

  (* 유사도 점수 계산: 차이점 가중치 기반 *)
  let total_weight = List.fold_left (fun acc d ->
    acc +. severity_weight d.severity
  ) 0.0 all_diffs in

  (* 최대 10점 기준으로 유사도 계산 *)
  let similarity = Float.max 0.0 (1.0 -. total_weight /. 10.0) in

  {
    node_a_name = node_a.name;
    node_b_name = node_b.name;
    node_a_id = node_a.id;
    node_b_id = node_b.id;
    differences = all_diffs;
    similarity_score = similarity;
  }

(** ============== 결과 포맷팅 ============== *)

let result_to_string result =
  (* 유사도 점수는 실험적 휴리스틱 기반이므로 차이점 유무만 표시 *)
  let header = Printf.sprintf "비교: \"%s\" vs \"%s\"\n"
    result.node_a_name result.node_b_name
  in

  if List.length result.differences = 0 then
    header ^ "유사도: 100%\n차이점 없음 ✓"
  else begin
    let grouped =
      ["structure"; "size"; "color"; "typography"; "layout"]
      |> List.filter_map (fun cat ->
        let cat_diffs = List.filter (fun d -> d.category = cat) result.differences in
        if List.length cat_diffs > 0 then
          Some (cat, cat_diffs)
        else None
      )
    in

    (* 차이점 요약: 심각도별 개수 *)
    let critical_count = List.length (List.filter (fun d -> d.severity = Critical) result.differences) in
    let major_count = List.length (List.filter (fun d -> d.severity = Major) result.differences) in
    let minor_count = List.length (List.filter (fun d -> d.severity = Minor) result.differences) in
    let total_count = List.length result.differences in

    let summary = Printf.sprintf "차이점: %d개 (Critical: %d, Major: %d, Minor: %d)\n"
      total_count critical_count major_count minor_count
    in

    let sections = List.map (fun (cat, diffs) ->
      let items = List.map (fun d ->
        Printf.sprintf "  [%s] %s" (severity_to_string d.severity) d.message
      ) diffs in
      Printf.sprintf "\n[%s]\n%s" (String.uppercase_ascii cat) (String.concat "\n" items)
    ) grouped in

    header ^ summary ^ String.concat "" sections
  end

(** ============== 이름 기반 매칭 ============== *)

(** 공통 접두사/접미사 제거하여 핵심 이름 추출 *)
let normalize_name name =
  (* "Web/Button/Primary" -> "Button/Primary" *)
  (* "Mobile_Button_Primary" -> "Button_Primary" *)
  let name = String.lowercase_ascii name in
  let prefixes = ["web/"; "web_"; "mobile/"; "mobile_"; "desktop/"; "desktop_";
                  "ios/"; "ios_"; "android/"; "android_"] in
  List.fold_left (fun n prefix ->
    if String.length n > String.length prefix &&
       String.sub n 0 (String.length prefix) = prefix then
      String.sub n (String.length prefix) (String.length n - String.length prefix)
    else n
  ) name prefixes

(** 두 노드 리스트에서 이름이 매칭되는 쌍 찾기 *)
let find_matching_pairs nodes_a nodes_b =
  List.filter_map (fun na ->
    let norm_a = normalize_name na.name in
    let match_b = List.find_opt (fun nb ->
      normalize_name nb.name = norm_a
    ) nodes_b in
    match match_b with
    | Some nb -> Some (na, nb)
    | None -> None
  ) nodes_a

(** Web/Mobile 컴포넌트 일괄 비교 *)
let compare_web_mobile ~web_nodes ~mobile_nodes =
  let pairs = find_matching_pairs web_nodes mobile_nodes in
  let results = List.map (fun (web, mobile) ->
    compare_nodes web mobile
  ) pairs in

  (* 요약 통계 *)
  let total = List.length results in
  let avg_similarity =
    if total = 0 then 0.0
    else (List.fold_left (fun acc r -> acc +. r.similarity_score) 0.0 results) /. float_of_int total
  in

  let critical_count = List.fold_left (fun acc r ->
    acc + List.length (List.filter (fun d -> d.severity = Critical) r.differences)
  ) 0 results in

  let major_count = List.fold_left (fun acc r ->
    acc + List.length (List.filter (fun d -> d.severity = Major) r.differences)
  ) 0 results in

  (results, total, avg_similarity, critical_count, major_count)
