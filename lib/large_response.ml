(** Large Response Handler

    큰 MCP 응답(>500KB)을 파일로 저장하고 경로만 반환하는 모듈.
    Claude Code가 7MB JSON을 컨텍스트에 넣으려다 멈추는 문제 해결.
*)

open Printf

(** 설정 - Figma_config에서 가져옴 *)
let max_inline_size = Figma_config.Response.max_inline
let storage_dir = Figma_config.Response.large_dir
let response_ttl = Figma_config.Response.ttl_seconds

(** 디렉토리 생성 *)
let ensure_dir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

(** 사람이 읽기 쉬운 크기 문자열 *)
let human_size bytes =
  if bytes < 1024 then sprintf "%d B" bytes
  else if bytes < 1024 * 1024 then sprintf "%.1f KB" (float_of_int bytes /. 1024.0)
  else sprintf "%.1f MB" (float_of_int bytes /. 1024.0 /. 1024.0)

(** MCP content wrapper *)
let text_content text =
  `Assoc [
    ("content", `List [
      `Assoc [("type", `String "text"); ("text", `String text)]
    ])
  ]

let add_meta base meta =
  match base with
  | `Assoc fields -> `Assoc (fields @ meta)
  | _ -> base

(** 고유 파일명 생성 *)
let sanitize_prefix prefix =
  let is_safe_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
    | _ -> false
  in
  let mapped =
    String.map (fun c -> if is_safe_char c then c else '_') prefix
  in
  let mapped = if mapped = "" then "response" else mapped in
  let max_len = 64 in
  if String.length mapped > max_len then String.sub mapped 0 max_len else mapped

let generate_filename ~prefix =
  let prefix = sanitize_prefix prefix in
  let timestamp = Unix.gettimeofday () in
  let random = Random.int 10000 in
  sprintf "%s_%d_%04d.json" prefix (int_of_float timestamp) random

(** 파일로 저장 *)
let save_to_file ~prefix content =
  ensure_dir storage_dir;
  let filename = generate_filename ~prefix in
  let filepath = Filename.concat storage_dir filename in
  Out_channel.with_open_bin filepath (fun oc ->
    output_string oc content
  );
  filepath

(** 오래된 파일 정리 *)
let cleanup_old_files () =
  if Sys.file_exists storage_dir then begin
    let now = Unix.time () in
    let files = Sys.readdir storage_dir in
    Array.iter (fun filename ->
      let filepath = Filename.concat storage_dir filename in
      try
        let stats = Unix.stat filepath in
        if now -. stats.Unix.st_mtime > float_of_int response_ttl then
          Unix.unlink filepath
      with Unix.Unix_error _ -> ()
    ) files
  end

(** 응답 크기에 따라 분기 처리

    @param prefix 파일명 접두사 (예: "node_2089_10737")
    @param format 출력 포맷 (fidelity, raw, html)
    @param content JSON 문자열
    @return 작은 응답이면 그대로, 큰 응답이면 파일 경로 메타데이터
*)
let handle_response ~prefix ~format content : Yojson.Safe.t =
  let size = String.length content in

  (* 주기적으로 오래된 파일 정리 (10% 확률) *)
  if Random.int 10 = 0 then cleanup_old_files ();

  if size <= max_inline_size then
    (* 작은 응답: 그대로 반환 *)
    (try Yojson.Safe.from_string content
     with _ -> `String content)
  else begin
    (* 큰 응답: 파일로 저장하고 메타데이터 반환 *)
    let filepath = save_to_file ~prefix content in
    (* Include first 2KB as preview for immediate context *)
    let preview_size = min 2048 size in
    let preview = String.sub content 0 preview_size in
    let preview_truncated = if size > preview_size then preview ^ "\n... [truncated]" else preview in
    `Assoc [
      ("status", `String "large_result");
      ("file_path", `String filepath);
      ("size_bytes", `Int size);
      ("size_human", `String (human_size size));
      ("format", `String format);
      ("ttl_seconds", `Int response_ttl);
      ("preview", `String preview_truncated);
      ("hint", `String "Use figma_read_large_result with offset/limit, or figma_get_node_summary for lightweight structure");
      ("next_chunk", `Assoc [
        ("tool", `String "figma_read_large_result");
        ("args", `Assoc [
          ("file_path", `String filepath);
          ("offset", `Int preview_size);
          ("limit", `Int 10000);
        ]);
      ]);
      ("examples", `Assoc [
        ("read_first_1000_chars", `String (sprintf "head -c 1000 %s" filepath));
        ("read_with_jq", `String (sprintf "jq '.children[:5]' %s" filepath));
        ("progressive_load", `String "figma_get_node_summary → figma_get_node_chunk(depth_start=0, depth_end=2)");
      ]);
    ]
  end

(** JSON 결과를 크기 체크하여 처리 *)
let wrap_json_result ~prefix ~format (result : Yojson.Safe.t) : Yojson.Safe.t =
  let content = Yojson.Safe.to_string result in
  handle_response ~prefix ~format content

(** 문자열 결과를 크기 체크하여 처리 (fidelity DSL 등) *)
let wrap_string_result ~prefix ~format (content : string) : Yojson.Safe.t =
  let size = String.length content in

  if size <= max_inline_size then
    text_content content
  else begin
    let filepath = save_to_file ~prefix content in
    (* Include first 2KB as preview *)
    let preview_size = min 2048 size in
    let preview = String.sub content 0 preview_size in
    let preview_truncated = if size > preview_size then preview ^ "\n... [truncated]" else preview in
    let message = sprintf
      "Large result saved to %s (%s). Preview below, use figma_read_large_result for more.\n\n%s"
      filepath (human_size size) preview_truncated
    in
    add_meta (text_content message) [
      ("status", `String "large_result");
      ("file_path", `String filepath);
      ("size_bytes", `Int size);
      ("size_human", `String (human_size size));
      ("format", `String format);
      ("preview_bytes", `Int preview_size);
      ("next_chunk", `Assoc [
        ("tool", `String "figma_read_large_result");
        ("args", `Assoc [
          ("file_path", `String filepath);
          ("offset", `Int preview_size);
          ("limit", `Int 10000);
        ]);
      ]);
      ("hint", `String "Content saved to file. Use figma_read_large_result or figma_get_node_chunk for progressive loading.");
    ]
  end

(** ============== 노드 수 기반 사전 경고 시스템 ============== *)

(** 노드 수 임계값 *)
let node_thresholds = {|
  - 0~50: Small - 인라인 DSL 권장
  - 51~200: Medium - 기본 DSL 사용 가능
  - 201~500: Large - memoization 권장
  - 501~1000: Very Large - progressive loading 필수
  - 1001+: Huge - depth 제한 또는 chunk 필수
|}

(** 예상 토큰 수 계산 (노드당 평균 ~50 토큰) *)
let estimate_tokens node_count =
  (* 실험 기반: 노드당 평균 30-70 토큰, 중앙값 50 *)
  node_count * 50

(** 노드 수 기반 경고 레벨 *)
type warning_level =
  | Info     (** 문제 없음 *)
  | Caution  (** 최적화 권장 *)
  | Warning  (** progressive loading 권장 *)
  | Critical (** 반드시 조치 필요 *)

let warning_level_of_count count =
  if count <= 50 then Info
  else if count <= 200 then Caution
  else if count <= 500 then Warning
  else Critical

let warning_emoji = function
  | Info -> "✅"
  | Caution -> "💡"
  | Warning -> "⚠️"
  | Critical -> "🚨"

let warning_message level count tokens =
  let emoji = warning_emoji level in
  match level with
  | Info ->
    sprintf "%s Small tree (%d nodes, ~%d tokens) - inline DSL OK" emoji count tokens
  | Caution ->
    sprintf "%s Medium tree (%d nodes, ~%d tokens) - consider memoization" emoji count tokens
  | Warning ->
    sprintf "%s Large tree (%d nodes, ~%d tokens) - progressive loading recommended" emoji count tokens
  | Critical ->
    sprintf "%s Very large tree (%d nodes, ~%d tokens) - MUST use depth limiting or chunking" emoji count tokens

(** 노드 수 분석 결과 *)
type node_analysis = {
  total_nodes: int;
  estimated_tokens: int;
  level: warning_level;
  message: string;
  recommendations: string list;
}

(** 노드 수 기반 사전 분석 *)
let analyze_node_count count : node_analysis =
  let tokens = estimate_tokens count in
  let level = warning_level_of_count count in
  let message = warning_message level count tokens in
  let recommendations = match level with
    | Info -> []
    | Caution -> [
        "Use generate_compact_memoized for repeated styles";
      ]
    | Warning -> [
        "Use figma_get_node_summary first for structure overview";
        "Apply depth_limit to limit tree depth";
        "Consider figma_fidelity_loop for progressive refinement";
      ]
    | Critical -> [
        "REQUIRED: Use figma_get_node_chunk with depth ranges";
        "Example: figma_get_node_chunk(depth_start=0, depth_end=2) → then (depth_start=3, depth_end=5)";
        "Or use figma_get_nodes to fetch specific node IDs only";
      ]
  in
  { total_nodes = count; estimated_tokens = tokens; level; message; recommendations }

(** 분석 결과를 JSON으로 변환 *)
let analysis_to_json (analysis : node_analysis) : Yojson.Safe.t =
  `Assoc [
    ("total_nodes", `Int analysis.total_nodes);
    ("estimated_tokens", `Int analysis.estimated_tokens);
    ("warning_level", `String (match analysis.level with
      | Info -> "info" | Caution -> "caution"
      | Warning -> "warning" | Critical -> "critical"));
    ("message", `String analysis.message);
    ("recommendations", `List (List.map (fun s -> `String s) analysis.recommendations));
  ]

(** 응답에 경고 정보 추가 *)
let add_warning_to_response ~node_count (response : Yojson.Safe.t) : Yojson.Safe.t =
  let analysis = analyze_node_count node_count in
  if analysis.level = Info then response
  else
    match response with
    | `Assoc fields ->
      `Assoc (("_warning", analysis_to_json analysis) :: fields)
    | other -> other

(** JSON에서 노드 수 세기 (재귀) *)
let rec count_nodes_json (json : Yojson.Safe.t) : int =
  match json with
  | `Assoc fields ->
    let children_count =
      match List.assoc_opt "children" fields with
      | Some (`List children) ->
        List.fold_left (fun acc child -> acc + count_nodes_json child) 0 children
      | _ -> 0
    in
    1 + children_count  (* 현재 노드 + 자식들 *)
  | `List items ->
    List.fold_left (fun acc item -> acc + count_nodes_json item) 0 items
  | _ -> 0

(** JSON 응답을 분석하고 필요시 경고 추가 *)
let analyze_and_wrap_json ~prefix ~format (json : Yojson.Safe.t) : Yojson.Safe.t =
  (* 1. 노드 수 세기 *)
  let node_count = count_nodes_json json in
  let analysis = analyze_node_count node_count in

  (* 2. 크기 기반 처리 *)
  let content = Yojson.Safe.to_string json in
  let size = String.length content in

  if size <= max_inline_size then begin
    (* 인라인 가능 - 경고만 추가 *)
    if analysis.level = Info then json
    else
      match json with
      | `Assoc fields ->
        `Assoc (("_warning", analysis_to_json analysis) :: fields)
      | _ -> json
  end else begin
    (* 파일로 저장 - 경고 포함 *)
    let filepath = save_to_file ~prefix content in
    `Assoc [
      ("_warning", analysis_to_json analysis);
      ("status", `String "large_result");
      ("file_path", `String filepath);
      ("size_bytes", `Int size);
      ("size_human", `String (human_size size));
      ("format", `String format);
      ("ttl_seconds", `Int response_ttl);
      ("hint", `String "Use progressive loading with figma_get_node_chunk");
    ]
  end

(** DSL 문자열을 분석하고 경고 포함하여 반환 *)
let wrap_dsl_with_warning ~prefix ~format ~node_count (dsl : string) : Yojson.Safe.t =
  let analysis = analyze_node_count node_count in
  let size = String.length dsl in

  if size <= max_inline_size then begin
    (* 인라인 가능 *)
    if analysis.level = Info then
      text_content dsl
    else
      add_meta (text_content dsl) [
        ("_warning", analysis_to_json analysis);
      ]
  end else begin
    (* 파일로 저장 *)
    let filepath = save_to_file ~prefix dsl in
    let message = sprintf
      "Large DSL saved to %s (%s). Use figma_read_large_result if needed."
      filepath (human_size size)
    in
    add_meta (text_content message) [
      ("_warning", analysis_to_json analysis);
      ("status", `String "large_result");
      ("file_path", `String filepath);
      ("size_bytes", `Int size);
      ("size_human", `String (human_size size));
      ("format", `String format);
      ("hint", `String "Content saved to file. Use figma_read_large_result if too large for context.");
    ]
  end

(** 텍스트 결과를 분석하고 경고 포함하여 반환 *)
let wrap_text_with_warning ~prefix ~format ~node_count (text : string) : Yojson.Safe.t =
  let analysis = analyze_node_count node_count in
  let size = String.length text in

  if size <= max_inline_size then begin
    if analysis.level = Info then
      text_content text
    else
      add_meta (text_content text) [
        ("_warning", analysis_to_json analysis);
      ]
  end else begin
    let filepath = save_to_file ~prefix text in
    let message = sprintf
      "Large result saved to %s (%s). Use figma_read_large_result."
      filepath (human_size size)
    in
    add_meta (text_content message) [
      ("_warning", analysis_to_json analysis);
      ("status", `String "large_result");
      ("file_path", `String filepath);
      ("size_bytes", `Int size);
      ("size_human", `String (human_size size));
      ("format", `String format);
      ("hint", `String "Use figma_read_large_result to read chunks.");
    ]
  end
