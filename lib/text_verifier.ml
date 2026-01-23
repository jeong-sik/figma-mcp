(** Text Verifier - DSL과 HTML 간 텍스트 정확도 검증

    SSIM은 픽셀 구조만 측정하므로, 같은 폰트/크기면 다른 텍스트도 높은 점수를 받음.
    이 모듈은 실제 텍스트 내용이 일치하는지 검증함. *)

open Figma_types

(** 텍스트 비교 결과 *)
type text_match = {
  dsl_text: string;      (** DSL에서 추출한 텍스트 *)
  html_text: string option; (** HTML에서 찾은 텍스트 (없으면 None) *)
  matched: bool;         (** 일치 여부 *)
}

type text_verification_result = {
  total_texts: int;      (** DSL의 총 TEXT 노드 수 *)
  matched_count: int;    (** 일치하는 텍스트 수 *)
  accuracy: float;       (** 정확도 (0.0 ~ 1.0) *)
  matches: text_match list; (** 상세 매칭 결과 *)
  passed: bool;          (** 모든 텍스트 일치 여부 *)
}

(** ui_node 트리에서 모든 TEXT 노드의 characters 추출 *)
let rec extract_texts_from_node (node: ui_node) : string list =
  let current_text =
    match node.node_type, node.characters with
    | Text, Some chars when String.length chars > 0 -> [chars]
    | _ -> []
  in
  let children_texts = List.concat_map extract_texts_from_node node.children in
  current_text @ children_texts

(** HTML에서 텍스트 추출 (간단한 태그 제거) *)
let extract_texts_from_html (html: string) : string list =
  (* 1. script, style 태그 내용 제거 *)
  let html = Str.global_replace (Str.regexp "<script[^>]*>.*?</script>") "" html in
  let html = Str.global_replace (Str.regexp "<style[^>]*>.*?</style>") "" html in

  (* 2. HTML 태그 제거 *)
  let html = Str.global_replace (Str.regexp "<[^>]*>") "\n" html in

  (* 3. HTML 엔티티 디코딩 *)
  let html = Str.global_replace (Str.regexp_string "&nbsp;") " " html in
  let html = Str.global_replace (Str.regexp_string "&lt;") "<" html in
  let html = Str.global_replace (Str.regexp_string "&gt;") ">" html in
  let html = Str.global_replace (Str.regexp_string "&amp;") "&" html in
  let html = Str.global_replace (Str.regexp_string "&quot;") "\"" html in

  (* 4. 줄바꿈/공백으로 분리하고 빈 문자열 제거 *)
  let parts = Str.split (Str.regexp "[\n\r\t]+") html in
  List.filter_map (fun s ->
    let trimmed = String.trim s in
    if String.length trimmed > 0 then Some trimmed else None
  ) parts

(** 텍스트 정규화 (비교용) *)
let normalize_text (s: string) : string =
  (* 공백 정규화: 연속 공백 → 단일 공백 *)
  let s = Str.global_replace (Str.regexp "[ \t\n\r]+") " " s in
  String.trim s

(** DSL 텍스트가 HTML 텍스트 목록에 존재하는지 확인 *)
let find_text_in_html (dsl_text: string) (html_texts: string list) : string option =
  let normalized_dsl = normalize_text dsl_text in
  List.find_opt (fun html_text ->
    let normalized_html = normalize_text html_text in
    (* 완전 일치 또는 포함 관계 *)
    normalized_dsl = normalized_html ||
    String.length normalized_dsl > 2 && (
      (* DSL 텍스트가 HTML에 포함되어 있는지 *)
      try Str.search_forward (Str.regexp_string normalized_dsl) normalized_html 0 >= 0
      with Not_found -> false
    )
  ) html_texts

(** 텍스트 정확도 검증 *)
let verify_texts ~(dsl_node: ui_node) ~(html: string) : text_verification_result =
  let dsl_texts = extract_texts_from_node dsl_node in
  let html_texts = extract_texts_from_html html in

  let matches = List.map (fun dsl_text ->
    let html_match = find_text_in_html dsl_text html_texts in
    {
      dsl_text;
      html_text = html_match;
      matched = Option.is_some html_match;
    }
  ) dsl_texts in

  let total = List.length matches in
  let matched_count = List.length (List.filter (fun m -> m.matched) matches) in
  let accuracy = if total > 0 then float_of_int matched_count /. float_of_int total else 1.0 in

  {
    total_texts = total;
    matched_count;
    accuracy;
    matches;
    passed = matched_count = total;
  }

(** 결과를 JSON으로 변환 *)
let match_to_json (m: text_match) : Yojson.Safe.t =
  `Assoc [
    ("dsl_text", `String m.dsl_text);
    ("html_text", match m.html_text with Some t -> `String t | None -> `Null);
    ("matched", `Bool m.matched);
  ]

let result_to_json (result: text_verification_result) : Yojson.Safe.t =
  `Assoc [
    ("total_texts", `Int result.total_texts);
    ("matched_count", `Int result.matched_count);
    ("accuracy", `Float result.accuracy);
    ("passed", `Bool result.passed);
    ("matches", `List (List.map match_to_json result.matches));
  ]
