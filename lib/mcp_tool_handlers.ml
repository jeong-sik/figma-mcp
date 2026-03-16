open Figma_mcp_protocol
open Mcp_helpers
open Mcp_tool_defs
open Printf
let command_output cmd argv =
  match Safe_exec.run_stdout ~timeout_ms:5000 ~output_limit:(64 * 1024) cmd argv with
  | Ok out -> String.trim out
  | Error _ -> ""

let has_command name =
  Safe_exec.command_exists name

let has_node_module name =
  match Safe_exec.run ~timeout_ms:5000 ~output_limit:(32 * 1024) "node" [| "node"; "-e"; Printf.sprintf "require('%s')" name |] with
  | Ok out ->
      (match out.status with
       | Unix.WEXITED 0 -> true
       | _ -> false)
  | Error _ -> false

(** mkdir_p: moved to mcp_helpers.ml *)

let normalize_path path =
  try Some (Unix.realpath path) with Unix.Unix_error _ -> None

let is_under_dir ~dir path =
  (* #164: non-existent file causes normalize_path to return None on macOS
     where /tmp → /private/tmp symlink. Normalize parent dir as fallback. *)
  let normalize_or_parent p =
    match normalize_path p with
    | Some _ as ok -> ok
    | None ->
        let parent = Filename.dirname p in
        match normalize_path parent with
        | Some parent_norm -> Some (Filename.concat parent_norm (Filename.basename p))
        | None -> None
  in
  match (normalize_path dir, normalize_or_parent path) with
  | (Some dir_norm, Some path_norm) ->
      let prefix = if String.ends_with ~suffix:"/" dir_norm then dir_norm else dir_norm ^ "/" in
      path_norm = dir_norm || String.starts_with ~prefix path_norm
  | _ -> false


(** Core Figma API handlers (handle_get_file .. handle_get_style): moved to mcp_api_handlers.ml *)

(** Plugin handlers (handle_plugin_*, handle_figma_plugin, known_plugin_actions):
    moved to mcp_plugin_handlers.ml *)

(** ============== LLM Bridge 핸들러 ============== *)

let has_field key fields =
  List.exists (fun (k, _) -> k = key) fields

let set_field key value fields =
  let filtered = List.filter (fun (k, _) -> k <> key) fields in
  (key, value) :: filtered

let add_if_missing key value fields =
  if has_field key fields then fields else (key, value) :: fields

let get_string_any keys json =
  let rec loop = function
    | [] -> None
    | key :: rest ->
        (match get_string key json with
         | Some v -> Some v
         | None -> loop rest)
  in
  loop keys

let truncate_string ~max_len value =
  if max_len <= 0 then value
  else if String.length value > max_len then
    String.sub value 0 max_len ^ "...(truncated)"
  else
    value

let is_utf8_continuation byte =
  byte land 0b1100_0000 = 0b1000_0000

let utf8_safe_boundary ~start ~max_bytes value =
  let len = String.length value in
  let pos = min (start + max_bytes) len in
  let rec back i =
    if i <= start then start
    else
      let byte = Char.code value.[i - 1] in
      if is_utf8_continuation byte then back (i - 1) else i
  in
  back pos

let truncate_utf8 ~max_bytes value =
  if max_bytes <= 0 then (value, false)
  else
    let len = String.length value in
    if len <= max_bytes then (value, false)
    else
      let cut = utf8_safe_boundary ~start:0 ~max_bytes value in
      let cut = if cut = 0 then min max_bytes len else cut in
      (String.sub value 0 cut, true)

let take_n n items =
  let rec loop acc remaining = function
    | [] -> List.rev acc
    | _ when remaining <= 0 -> List.rev acc
    | x :: xs -> loop (x :: acc) (remaining - 1) xs
  in
  loop [] n items

let chunk_list chunk_size items =
  let size = if chunk_size <= 0 then 1 else chunk_size in
  let rec loop acc current = function
    | [] ->
        let acc =
          if current = [] then acc
          else List.rev current :: acc
        in
        List.rev acc
    | x :: xs ->
        let current = x :: current in
        if List.length current >= size then
          loop (List.rev current :: acc) [] xs
        else
          loop acc current xs
  in
  loop [] [] items

let rec compact_json
    ~depth
    ~max_depth
    ~max_children
    ~max_list_items
    ~max_string
    json =
  match json with
  | `Assoc fields ->
      let fields =
        List.filter (fun (k, _) -> not (String.ends_with ~suffix:"_missing" k)) fields
      in
      let fields =
        if depth >= max_depth then
          List.filter (fun (k, _) -> k <> "children") fields
          |> fun filtered -> ("_depth_truncated", `Bool true) :: filtered
        else
          fields
      in
      let fields =
        List.map (fun (k, v) ->
          if k = "children" then
            match v with
            | `List items ->
                let total = List.length items in
                let items = take_n max_children items in
                let items =
                  List.map (compact_json
                              ~depth:(depth + 1)
                              ~max_depth
                              ~max_children
                              ~max_list_items
                              ~max_string) items
                in
                if total > List.length items then
                  (k, `List (items @ [`Assoc [("_truncated", `Bool true); ("total", `Int total)]]))
                else
                  (k, `List items)
            | _ ->
                (k, compact_json
                      ~depth:(depth + 1)
                      ~max_depth
                      ~max_children
                      ~max_list_items
                      ~max_string
                      v)
          else
            (k, compact_json
                  ~depth:(depth + 1)
                  ~max_depth
                  ~max_children
                  ~max_list_items
                  ~max_string
                  v)
        ) fields
      in
      `Assoc fields
  | `List items ->
      let total = List.length items in
      let items = take_n max_list_items items in
      let items =
        List.map (compact_json
                    ~depth:(depth + 1)
                    ~max_depth
                    ~max_children
                    ~max_list_items
                    ~max_string) items
      in
      if total > List.length items then
        `List (items @ [`Assoc [("_truncated", `Bool true); ("total", `Int total)]])
      else
        `List items
  | `String s -> `String (truncate_string ~max_len:max_string s)
  | other -> other

let chunkify_children ~chunk_size json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "children" fields with
      | Some (`List children) ->
          let chunks = chunk_list chunk_size children in
          let total = List.length chunks in
          let chunks =
            List.mapi (fun idx chunk ->
              `Assoc [
                ("chunk_index", `Int (idx + 1));
                ("chunk_total", `Int total);
                ("children", `List chunk);
              ]) chunks
          in
          let fields = List.filter (fun (k, _) -> k <> "children") fields in
          `Assoc (("chunks", `List chunks) :: ("chunk_total", `Int total) :: fields)
      | _ -> json)
  | _ -> json

let chunkify_text ~chunk_size text =
  let size = if chunk_size <= 0 then 1 else chunk_size in
  let len = String.length text in
  let rec loop idx acc =
    if idx >= len then List.rev acc
    else
      let next = utf8_safe_boundary ~start:idx ~max_bytes:size text in
      let next = if next <= idx then min (idx + size) len else next in
      let chunk = String.sub text idx (next - idx) in
      loop next (chunk :: acc)
  in
  let chunks = loop 0 [] in
  let total = List.length chunks in
  let chunks =
    List.mapi (fun idx chunk ->
      `Assoc [
        ("chunk_index", `Int (idx + 1));
        ("chunk_total", `Int total);
        ("content", `String chunk);
      ]) chunks
  in
  `Assoc [
    ("chunked_text", `Bool true);
    ("chunk_total", `Int total);
    ("chunks", `List chunks);
  ]

let select_chunked_json ~selected json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "chunks" fields with
      | Some (`List chunks) ->
          let selected_set =
            selected
            |> List.map (fun v -> (v, ()))
            |> List.to_seq
            |> Hashtbl.of_seq
          in
          let keep chunk =
            match chunk with
            | `Assoc chunk_fields ->
                (match List.assoc_opt "chunk_index" chunk_fields with
                 | Some (`Int idx) -> Hashtbl.mem selected_set idx
                 | Some (`Float f) -> Hashtbl.mem selected_set (int_of_float f)
                 | _ -> false)
            | _ -> false
          in
          let chunks = List.filter keep chunks in
          let fields = List.filter (fun (k, _) -> k <> "chunks") fields in
          `Assoc (("chunks", `List chunks) :: ("chunk_selected", `List (List.map (fun v -> `Int v) selected)) :: fields)
      | _ -> json)
  | _ -> json

let bump_count counts key =
  let current = match Hashtbl.find_opt counts key with
    | Some v -> v
    | None -> 0
  in
  Hashtbl.replace counts key (current + 1)

let type_counts_to_json counts =
  let items =
    Hashtbl.to_seq counts
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  `Assoc (List.map (fun (k, v) -> (k, `Int v)) items)

type plugin_stats = {
  mutable node_count: int;
  mutable text_nodes: int;
  mutable segment_count: int;
  mutable segment_bounds_count: int;
  type_counts: (string, int) Hashtbl.t;
  mutable name_samples: string list;
  mutable text_samples: string list;
  mutable selection_count: int option;
}

let create_plugin_stats () =
  {
    node_count = 0;
    text_nodes = 0;
    segment_count = 0;
    segment_bounds_count = 0;
    type_counts = Hashtbl.create 32;
    name_samples = [];
    text_samples = [];
    selection_count = None;
  }

let append_sample ~max items value =
  if value = "" then items
  else if List.length items >= max then items
  else value :: items

let count_segment_bounds segments =
  List.fold_left (fun acc seg ->
    match seg with
    | `Assoc fields ->
        (match List.assoc_opt "bounds" fields with
         | Some (`Null) | None -> acc
         | _ -> acc + 1)
    | _ -> acc
  ) 0 segments

let rec collect_plugin_stats ~sample_size stats json =
  match json with
  | `Assoc fields ->
      stats.node_count <- stats.node_count + 1;
      (match List.assoc_opt "type" fields with
       | Some (`String t) -> bump_count stats.type_counts t
       | _ -> ());
      (match List.assoc_opt "name" fields with
       | Some (`String name) ->
           stats.name_samples <- append_sample ~max:sample_size stats.name_samples name
       | _ -> ());
      (match List.assoc_opt "text" fields with
       | Some (`Assoc text_fields) ->
           stats.text_nodes <- stats.text_nodes + 1;
           (match List.assoc_opt "characters" text_fields with
            | Some (`String chars) ->
                let snippet = truncate_string ~max_len:80 chars in
                stats.text_samples <- append_sample ~max:sample_size stats.text_samples snippet
            | _ -> ());
           (match List.assoc_opt "segments" text_fields with
            | Some (`List segments) ->
                stats.segment_count <- stats.segment_count + List.length segments;
                stats.segment_bounds_count <-
                  stats.segment_bounds_count + count_segment_bounds segments
            | _ -> ())
       | _ -> ());
      (match List.assoc_opt "children" fields with
       | Some (`List kids) ->
           List.iter (collect_plugin_stats ~sample_size stats) kids
       | _ -> ())
  | `List items ->
      List.iter (collect_plugin_stats ~sample_size stats) items
  | _ -> ()

let summarize_plugin_payload ~sample_size payload =
  match payload with
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`String err) -> `Assoc [("error", `String err)]
      | Some _ -> `Assoc [("error", `String "Plugin payload error")]
      | None ->
          let stats = create_plugin_stats () in
          (match List.assoc_opt "selectionCount" fields with
           | Some (`Int v) -> stats.selection_count <- Some v
           | Some (`Float f) -> stats.selection_count <- Some (int_of_float f)
           | _ -> ());
          let nodes =
            match List.assoc_opt "nodes" fields with
            | Some (`List nodes) -> `List nodes
            | _ -> payload
          in
          collect_plugin_stats ~sample_size stats nodes;
          let summary = [
            ("node_count", `Int stats.node_count);
            ("text_nodes", `Int stats.text_nodes);
            ("segment_count", `Int stats.segment_count);
            ("segment_bounds_count", `Int stats.segment_bounds_count);
            ("type_counts", type_counts_to_json stats.type_counts);
            ("name_samples", `List (List.rev_map (fun s -> `String s) stats.name_samples |> List.rev));
            ("text_samples", `List (List.rev_map (fun s -> `String s) stats.text_samples |> List.rev));
          ] in
          let summary =
            match stats.selection_count with
            | Some v -> ("selection_count", `Int v) :: summary
            | None -> summary
          in
          `Assoc summary
    )
  | _ -> `Assoc [("error", `String "Invalid plugin payload")]





let handle_parse_url args : (Yojson.Safe.t, string) result =
  match get_string "url" args with
  | None -> Error "Missing required parameter: url"
  | Some url ->
      let info = Figma_api.parse_figma_url url in
      let result = sprintf "Parsed URL:\n- team_id: %s\n- project_id: %s\n- file_key: %s\n- node_id: %s"
        (Option.value ~default:"(none)" info.team_id)
        (Option.value ~default:"(none)" info.project_id)
        (Option.value ~default:"(none)" info.file_key)
        (Option.value ~default:"(none)" info.node_id)
      in
      Ok (make_text_content result)

(** figma_get_me 핸들러 - 현재 사용자 정보 *)
let handle_get_me args : (Yojson.Safe.t, string) result =
  match resolve_token args with
  | None -> Error "Missing required parameter: token (set FIGMA_TOKEN env var or pass explicitly)"
  | Some token ->
      (match Figma_effects.Perform.get_me ~token with
       | Ok json ->
           let id = get_string "id" json in
           let email = get_string "email" json in
           let handle = get_string "handle" json in
           let result = sprintf "User Info:\n- id: %s\n- email: %s\n- handle: %s"
             (Option.value ~default:"(unknown)" id)
             (Option.value ~default:"(unknown)" email)
             (Option.value ~default:"(unknown)" handle)
           in
           Ok (make_text_content result)
       | Error err -> Error err)

(** figma_list_projects 핸들러 - 팀의 프로젝트 목록 *)
let handle_list_projects args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_projects ~token ~team_id with
       | Ok json ->
           let projects = match member "projects" json with
             | Some (`List lst) -> lst
             | _ -> []
           in
           let project_list = List.filter_map (fun p ->
             let id = get_string "id" p in
             let name = get_string "name" p in
             match (id, name) with
             | (Some id, Some name) -> Some (sprintf "- %s (id: %s)" name id)
             | _ -> None
           ) projects in
           let result = sprintf "Found %d projects:\n%s"
             (List.length project_list)
             (String.concat "\n" project_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_list_files 핸들러 - 프로젝트의 파일 목록 *)
let handle_list_files args : (Yojson.Safe.t, string) result =
  let project_id = get_string "project_id" args in
  let token = resolve_token args in

  match (project_id, token) with
  | (Some project_id, Some token) ->
      (match Figma_effects.Perform.get_project_files ~token ~project_id with
       | Ok json ->
           let files = match member "files" json with
             | Some (`List lst) -> lst
             | _ -> []
           in
           let file_list = List.filter_map (fun f ->
             let key = get_string "key" f in
             let name = get_string "name" f in
             match (key, name) with
             | (Some key, Some name) -> Some (sprintf "- %s (key: %s)" name key)
             | _ -> None
           ) files in
           let result = sprintf "Found %d files:\n%s"
             (List.length file_list)
             (String.concat "\n" file_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: project_id, token"

(** figma_crawl_team 핸들러 - 팀 전체 크롤링 + Neo4j 저장 (Effects 기반) *)
let handle_crawl_team args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let team_name = get_string_or "team_name" "Unknown Team" args in
  let token = resolve_token args in
  let max_depth = get_string "max_depth" args |> Option.map int_of_string |> Option.value ~default:10 in
  let rate_limit_ms = get_string "rate_limit_ms" args |> Option.map int_of_string |> Option.value ~default:100 in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (* Neo4j 설정 (환경변수에서 또는 파라미터에서) *)
      let neo4j_uri = get_string "neo4j_uri" args
        |> Option.value ~default:(Sys.getenv_opt "NEO4J_URI" |> Option.value ~default:"http://localhost:7474") in
      let neo4j_user = get_string "neo4j_user" args
        |> Option.value ~default:(Sys.getenv_opt "NEO4J_USER" |> Option.value ~default:"neo4j") in
      let neo4j_password = get_string "neo4j_password" args
        |> Option.value ~default:(Sys.getenv_opt "NEO4J_PASSWORD" |> Option.value ~default:"") in
      let neo4j_database = Sys.getenv_opt "NEO4J_DATABASE" |> Option.value ~default:"neo4j" in

      let neo4j_cfg = Figma_crawl.create_neo4j_config
        ~uri:neo4j_uri ~database:neo4j_database ~user:neo4j_user ~password:neo4j_password () in

      let options = {
        Figma_crawl.max_depth;
        include_hidden = false;
        batch_size = 100;
        rate_limit_ms;
        skip_files = [];
      } in

      (* 진행 상황 버퍼 *)
      let (on_progress, get_log) = Figma_crawl.buffer_progress () in

      (* 크롤링 실행 (Effects 기반) *)
      (match Figma_crawl.crawl_team ~token ~team_id ~neo4j_cfg ~options ~team_name ~on_progress () with
       | Ok progress ->
           let result_json = `Assoc [
             ("status", `String "success");
             ("summary", Figma_crawl.progress_to_json progress);
             ("log", `String (get_log ()));
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result_json))
       | Error err ->
           let result_json = `Assoc [
             ("status", `String "error");
             ("error", `String err);
             ("log", `String (get_log ()));
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result_json)))
  | (None, _) -> Error "Missing required parameter: team_id"
  | (_, None) -> Error "Missing required parameter: token (set FIGMA_TOKEN or pass token parameter)"

(** figma_team_tree 핸들러 - 팀 구조 트리 출력 (Neo4j 불필요) *)
let handle_team_tree args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let team_name = get_string_or "team_name" "Unknown Team" args in
  let token = resolve_token args in
  let include_nodes = get_bool_or "include_nodes" false args in
  let node_depth = get_string "node_depth" args |> Option.map int_of_string |> Option.value ~default:2 in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_crawl.team_tree ~token ~team_id ~team_name ~include_nodes ~node_depth () with
       | Ok (tree_str, progress) ->
           Ok (make_text_content (sprintf "```\n%s```\n\n%s"
             tree_str (Yojson.Safe.pretty_to_string (`Assoc [("summary", Figma_crawl.progress_to_json progress)]))))
       | Error err -> Error err)
  | (None, _) -> Error "Missing required parameter: team_id"
  | (_, None) -> Error "Missing required parameter: token (set FIGMA_TOKEN or pass token parameter)"

(** figma_export_team 핸들러 - 파일 시스템으로 내보내기 *)
let handle_export_team args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let team_name = get_string_or "team_name" "Unknown Team" args in
  let token = resolve_token args in
  let output_dir = get_string "output_dir" args in
  let max_depth = get_string "max_depth" args |> Option.map int_of_string |> Option.value ~default:2 in

  match (team_id, token, output_dir) with
  | (Some team_id, Some token, Some output_dir) ->
      let (on_progress, get_log) = Figma_crawl.buffer_progress () in
      (match Figma_crawl.export_team_to_fs ~token ~team_id ~output_dir ~max_depth ~team_name ~on_progress () with
       | Ok progress ->
           Ok (make_text_content (sprintf "```\n%s```\n\n%s"
             (get_log ()) (Yojson.Safe.pretty_to_string (`Assoc [
               ("status", `String "success");
               ("output_dir", `String output_dir);
               ("summary", Figma_crawl.progress_to_json progress);
             ]))))
       | Error err -> Error err)
  | (None, _, _) -> Error "Missing required parameter: team_id"
  | (_, None, _) -> Error "Missing required parameter: token"
  | (_, _, None) -> Error "Missing required parameter: output_dir"

(** figma_get_variables 핸들러 - 디자인 토큰/변수 *)
let handle_get_variables args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let format = get_string_or "format" "summary" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      let json_result =
        match fetch_variables_cached ~file_key ~token with
        | Ok (json, _) -> Ok json
        | Error err -> Error err
      in
      (match json_result with
       | Ok json ->
           (match format with
            | "raw" ->
                Ok (make_text_content (Yojson.Safe.pretty_to_string json))
            | "resolved" ->
                let resolved = resolve_variables json in
                Ok (make_text_content (Yojson.Safe.pretty_to_string resolved))
            | _ ->
                (* 변수 컬렉션과 변수 목록 추출 *)
                let collections = match member "meta" json with
                  | Some meta -> (match member "variableCollections" meta with
                      | Some (`Assoc lst) -> List.length lst
                      | _ -> 0)
                  | _ -> 0
                in
                let variables = match member "meta" json with
                  | Some meta -> (match member "variables" meta with
                      | Some (`Assoc lst) -> List.length lst
                      | _ -> 0)
                  | _ -> 0
                in
                let result = sprintf "Design Tokens Summary:\n- Collections: %d\n- Variables: %d"
                  collections variables
                in
                Ok (make_text_content result))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** ============== Phase 2: 고급 쿼리 핸들러 ============== *)

(** figma_query 핸들러 - 노드 필터링 *)
let handle_query args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let node_id = get_string "node_id" args in
  let type_filter = get_string "type" args in
  let width_min = get_float "width_min" args in
  let width_max = get_float "width_max" args in
  let height_min = get_float "height_min" args in
  let height_max = get_float "height_max" args in
  let color = get_string "color" args in
  let name = get_string "name" args in
  let depth = get_float "depth" args |> Option.map int_of_float in
  let limit = get_float "limit" args |> Option.map int_of_float in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (* 파일 또는 특정 노드 가져오기 - Effect 사용 *)
      let json_result = match node_id with
        | Some nid -> Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[nid] ()
        | None -> Figma_effects.Perform.get_file ~token ~file_key ()
      in
      (match json_result with
       | Ok json ->
           (* JSON에서 document 추출 *)
           let doc_json = match node_id with
             | Some nid ->
                 (match member "nodes" json with
                  | Some (`Assoc nodes) ->
                      (match List.assoc_opt nid nodes with
                       | Some node -> member "document" node
                       | None -> None)
                  | _ -> None)
             | None -> Figma_api.extract_document json
           in
           (match doc_json with
            | Some doc_json ->
                let doc_str = Yojson.Safe.to_string doc_json in
                (match Figma_parser.parse_json_string doc_str with
                 | Some root ->
                     (* 쿼리 빌드 *)
                     let q = Figma_query.empty_query in
                     let q = match type_filter with
                       | Some t -> Figma_query.with_type (String.split_on_char ',' t |> List.map String.trim) q
                       | None -> q
                     in
                     let q = match width_min with Some w -> Figma_query.with_width_min w q | None -> q in
                     let q = match width_max with Some w -> Figma_query.with_width_max w q | None -> q in
                     let q = match height_min with Some h -> Figma_query.with_height_min h q | None -> q in
                     let q = match height_max with Some h -> Figma_query.with_height_max h q | None -> q in
                     let q = match color with Some c -> Figma_query.with_color c q | None -> q in
                     let q = match name with Some n -> Figma_query.with_name n q | None -> q in
                     let q = match depth with Some d -> Figma_query.with_depth d q | None -> q in
                     let q = match limit with Some l -> Figma_query.with_limit l q | None -> q in

                     (* 쿼리 실행 *)
                     let results = Figma_query.execute_query q root in
                     let result_str = Figma_query.results_to_string results in
                     Ok (make_text_content result_str)
                 | None -> Error "Failed to parse document")
            | None -> Error "Document not found")
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_search 핸들러 - 텍스트/이름 검색 *)
let handle_search args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let query = get_string "query" args in
  let search_in = get_string_or "search_in" "both" args in
  let limit = get_float "limit" args |> Option.map int_of_float |> Option.value ~default:20 in

  match (file_key, token, query) with
  | (Some file_key, Some token, Some query) ->
      (match fetch_file_for_search_cached ~file_key ~token with
       | Ok (json, _source) ->
           (match Figma_api.extract_document json with
            | Some doc_json ->
                let doc_str = Yojson.Safe.to_string doc_json in
                (match Figma_parser.parse_json_string doc_str with
                 | Some root ->
                     (* 모든 노드 수집 *)
                     let all_nodes = Figma_query.collect_nodes_with_ancestors ~max_depth:None ~ancestors:[] root in
                     let query_lower = String.lowercase_ascii query |> String.trim in
                     if query_lower = "" then
                       Ok (make_text_content "[]")
                     else
                       let tokens =
                         query_lower
                         |> Str.split (Str.regexp "[ \t\r\n]+")
                         |> List.filter (fun s -> s <> "")
                       in
                       let tokens = if tokens = [] then [query_lower] else tokens in

                       let contains hay needle =
                         try
                           let _ = Str.search_forward (Str.regexp_string needle) hay 0 in
                           true
                         with Not_found -> false
                       in

                       let score_node (ancestors, node) =
                         let name_lower = String.lowercase_ascii node.Figma_types.name in
                         let chars_lower =
                           match node.Figma_types.characters with
                           | Some chars -> Some (String.lowercase_ascii chars)
                           | None -> None
                         in

                         let token_in_name tok = contains name_lower tok in
                         let token_in_text tok =
                           match chars_lower with
                           | Some t -> contains t tok
                           | None -> false
                         in

                         let matched_name = List.exists token_in_name tokens in
                         let matched_text = List.exists token_in_text tokens in
                         let matches =
                           match search_in with
                           | "name" -> matched_name
                           | "text" -> matched_text
                           | _ -> matched_name || matched_text
                         in
                         if not matches then
                           None
                         else
                           let base_score =
                             List.fold_left
                               (fun acc tok ->
                                 acc
                                 +. (if token_in_name tok then 3.0 else 0.0)
                                 +. (if token_in_text tok then 1.0 else 0.0))
                               0.0
                               tokens
                           in
                           let exact_bonus =
                             if query_lower <> "" && name_lower = query_lower then 100.0 else 0.0
                           in
                           let prefix_bonus =
                             if query_lower <> ""
                                && String.length name_lower >= String.length query_lower
                                && String.sub name_lower 0 (String.length query_lower) = query_lower
                             then 10.0
                             else 0.0
                           in
                           let matched_in =
                             match (matched_name, matched_text) with
                             | (true, true) -> "both"
                             | (true, false) -> "name"
                             | (false, true) -> "text"
                             | _ -> "both"
                           in
                           Some (base_score +. exact_bonus +. prefix_bonus, matched_in, ancestors, node)
                       in

                       let scored =
                         all_nodes
                         |> List.filter_map score_node
                         |> List.sort (fun (sa, _, _, a) (sb, _, _, b) ->
                              let c = compare sb sa in
                              if c <> 0 then c
                              else
                                let an = String.lowercase_ascii a.Figma_types.name in
                                let bn = String.lowercase_ascii b.Figma_types.name in
                                let cn = String.compare an bn in
                                if cn <> 0 then cn
                                else String.compare a.Figma_types.id b.Figma_types.id)
                         |> List.filteri (fun i _ -> i < limit)
                       in

                       (* Return JSON array in text content for robust client parsing.
                          Each item: {id,name,type,characters,score,matched_in}. *)
                       let items_json =
                         scored
                         |> List.map (fun (score, matched_in, ancestors, node) ->
                              let type_str =
                                Figma_query.node_type_to_string node.Figma_types.node_type
                              in
                              let chars =
                                match node.Figma_types.characters with
                                | Some c -> truncate_string ~max_len:200 c
                                | None -> ""
                              in
                              let parent_name = match List.rev ancestors with
                                | x :: _ -> x
                                | [] -> ""
                              in
                              `Assoc [
                                ("id", `String node.Figma_types.id);
                                ("name", `String node.Figma_types.name);
                                ("type", `String type_str);
                                ("characters", `String chars);
                                ("score", `Float score);
                                ("matched_in", `String matched_in);
                                ("parent_name", `String parent_name);
                                ("ancestors", `List (List.map (fun s -> `String s) ancestors));
                              ])
                       in
                       Ok (make_text_content (Yojson.Safe.to_string (`List items_json)))
                 | None -> Error "Failed to parse document")
            | None -> Error "Document not found")
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token, query"

(** figma_tree 핸들러 *)
let handle_tree args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let node_id = get_string "node_id" args in
  let style_str = get_string_or "style" "ascii" args in
  let max_depth = get_float "max_depth" args |> Option.map int_of_float in
  let show_size = get_string_or "show_size" "true" args = "true" in
  let show_stats = get_string_or "show_stats" "false" args = "true" in

  let style = match style_str with
    | "indent" -> Figma_tree.Indent
    | "compact" -> Figma_tree.Compact
    | _ -> Figma_tree.Ascii
  in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Yojson.Safe.Util.member "document" json with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let start_node = match node_id with
                       | Some id ->
                           let all = Figma_query.collect_nodes ~max_depth:None root in
                           (match List.find_opt (fun n -> n.Figma_types.id = id) all with
                            | Some n -> n
                            | None -> root)
                       | None -> root
                     in
                     let result = Figma_tree.render ~style ~max_depth ~show_size ~show_stats start_node in
                     Ok (make_text_content result)
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_stats 핸들러 *)
let handle_stats args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let node_id = get_string "node_id" args in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Yojson.Safe.Util.member "document" json with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let start_node = match node_id with
                       | Some id ->
                           let all = Figma_query.collect_nodes ~max_depth:None root in
                           (match List.find_opt (fun n -> n.Figma_types.id = id) all with
                            | Some n -> n
                            | None -> root)
                       | None -> root
                     in
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None start_node in
                     let stats = Figma_stats.generate_report all_nodes in
                     Ok (make_text_content (Figma_stats.report_to_string stats))
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_export_tokens 핸들러 *)
let handle_export_tokens args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let format = get_string_or "format" "css" args in
  let node_id = get_string "node_id" args in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Yojson.Safe.Util.member "document" json with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let start_node = match node_id with
                       | Some id ->
                           let all = Figma_query.collect_nodes ~max_depth:None root in
                           (match List.find_opt (fun n -> n.Figma_types.id = id) all with
                            | Some n -> n
                            | None -> root)
                       | None -> root
                     in
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None start_node in
                     let result = match format with
                       | "semantic" ->
                         (* UIFormer-inspired Semantic DSL output *)
                         all_nodes
                         |> List.map (fun n ->
                           let dsl = Semantic_mapper.node_to_semantic n in
                           let prefix = match n.Figma_types.node_type with
                             | Figma_types.Frame | Figma_types.Component | Figma_types.Instance -> "F"
                             | Figma_types.Text -> "T"
                             | Figma_types.Rectangle | Figma_types.Ellipse | Figma_types.Vector -> "V"
                             | _ -> "N"
                           in
                           Printf.sprintf "%s(%s) ; %s" prefix dsl n.Figma_types.name)
                         |> String.concat "\n"
                       | _ ->
                         (* Design token extraction (CSS/Tailwind/JSON) *)
                         let tokens = Figma_tokens.extract_all all_nodes in
                         Figma_tokens.export_tokens tokens (Figma_tokens.format_of_string format)
                     in
                     Ok (make_text_content result)
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** 환경/의존성 점검 핸들러 *)
let handle_doctor _args : (Yojson.Safe.t, string) result =
  let mk_check name ok detail =
    `Assoc [
      ("name", `String name);
      ("ok", `Bool ok);
      ("detail", `String detail);
    ]
  in
  let node_ok = has_command "node" in
  let node_version =
    if node_ok then command_output "node" [| "node"; "-v" |] else "missing"
  in
  let playwright_ok = node_ok && has_node_module "playwright" in
  let pngjs_ok = node_ok && has_node_module "pngjs" in
  let pixelmatch_ok = node_ok && has_node_module "pixelmatch" in
  let magick_ok = has_command "magick" || has_command "convert" in
  let magick_detail =
    if has_command "magick" then "magick"
    else if has_command "convert" then "convert"
    else "missing"
  in
  let sips_ok = has_command "sips" in
  let render_script = Visual_verifier.render_script_path in
  let ssim_script = Visual_verifier.ssim_script_path in
  let render_script_ok = Sys.file_exists render_script in
  let ssim_script_ok = Sys.file_exists ssim_script in

  let required_ok =
    node_ok
    && playwright_ok
    && pngjs_ok
    && pixelmatch_ok
    && magick_ok
    && render_script_ok
    && ssim_script_ok
  in

  let checks = `List [
    mk_check "node" node_ok node_version;
    mk_check "playwright" playwright_ok (if playwright_ok then "ok (fallback renderer)" else "missing");
    mk_check "pngjs" pngjs_ok (if pngjs_ok then "ok" else "missing");
    mk_check "pixelmatch" pixelmatch_ok (if pixelmatch_ok then "ok" else "missing");
    mk_check "imagemagick" magick_ok magick_detail;
    mk_check "sips" sips_ok (if sips_ok then "ok" else "missing");
    mk_check "render_script" render_script_ok render_script;
    mk_check "ssim_script" ssim_script_ok ssim_script;
    mk_check "claude-in-chrome" true "preferred renderer (Claude Code built-in, runtime detection)";
  ] in

  let hints =
    List.filter_map Fun.id [
      if not node_ok then Some "Install Node.js (node required for render/compare scripts)." else None;
      if node_ok && not playwright_ok then Some "Playwright missing - will use claude-in-chrome if available, otherwise install: npm i -D playwright && npx playwright install chromium." else None;
      if node_ok && (not pngjs_ok || not pixelmatch_ok) then Some "Install image deps: npm i -D pngjs pixelmatch." else None;
      if not magick_ok then Some "Install ImageMagick (magick/convert) for PPM conversion." else None;
      if not render_script_ok then Some "Ensure render-html.js path is valid (FIGMA_RENDER_SCRIPT or scripts/render-html.js)." else None;
      if not ssim_script_ok then Some "Ensure ssim-compare.js path is valid (scripts/ssim-compare.js)." else None;
      Some "Chrome-First: When claude-in-chrome is available, use it for HTML rendering before falling back to Playwright.";
    ]
  in

  let result = `Assoc [
    ("status", `String (if required_ok then "ok" else "needs_attention"));
    ("checks", checks);
    ("hints", `List (List.map (fun h -> `String h) hints));
  ] in
  Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** large_result 파일 읽기 핸들러 *)
let handle_read_large_result args : (Yojson.Safe.t, string) result =
  let file_path = get_string "file_path" args in
  let offset = get_int "offset" args |> Option.value ~default:0 in
  let limit = get_int "limit" args |> Option.value ~default:20000 in

  match file_path with
  | None -> Error "Missing required parameter: file_path"
  | Some path ->
      let storage_dir = Large_response.storage_dir in
      if not (is_under_dir ~dir:storage_dir path) then
        Error (Printf.sprintf "file_path must be under %s" storage_dir)
      else if not (Sys.file_exists path) then
        Error (Printf.sprintf "File not found: %s" path)
      else
        let safe_offset = max 0 offset in
        let safe_limit = if limit <= 0 then 20000 else limit in
        In_channel.with_open_bin path (fun ic ->
          let total = in_channel_length ic in
          if safe_offset >= total then
            Error "offset is beyond EOF"
          else begin
            seek_in ic safe_offset;
            let to_read = min safe_limit (total - safe_offset) in
            let chunk = really_input_string ic to_read in
            let result = `Assoc [
              ("file_path", `String path);
              ("offset", `Int safe_offset);
              ("limit", `Int safe_limit);
              ("read_bytes", `Int to_read);
              ("total_bytes", `Int total);
              ("eof", `Bool (safe_offset + to_read >= total));
              ("chunk", `String chunk);
            ] in
            Ok (make_text_content (Yojson.Safe.pretty_to_string result))
          end)

(** 캐시 통계 핸들러 *)
let handle_cache_stats _args : (Yojson.Safe.t, string) result =
  let stats = Figma_cache.stats () in
  Ok (make_text_content (Yojson.Safe.pretty_to_string stats))

(** 캐시 무효화 핸들러 *)
let handle_cache_invalidate args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  Figma_cache.invalidate ?file_key ?node_id ();
  let message = match file_key, node_id with
    | None, _ -> "All cache invalidated"
    | Some fk, None -> sprintf "Cache invalidated for file: %s" fk
    | Some fk, Some nid -> sprintf "Cache invalidated for node: %s/%s" fk nid
  in
  let result = `Assoc [("status", `String "ok"); ("message", `String message)] in
  Ok (make_text_content (Yojson.Safe.to_string result))

(** Code Connect index cache (in-memory).
    This is intentionally small and bounded to avoid unbounded memory growth. *)
let code_connect_index_cache : (string, Figma_code_connect.mapping) Hashtbl.t =
  Hashtbl.create 32

let code_connect_cache_put key mapping =
  if Hashtbl.length code_connect_index_cache > 64 then Hashtbl.reset code_connect_index_cache;
  Hashtbl.replace code_connect_index_cache key mapping

let handle_code_connect args : (Yojson.Safe.t, string) result =
  let mode = get_string_or "mode" "" args |> String.lowercase_ascii |> String.trim in
  if mode = "" then Error "Missing required parameter: mode"
  else
    let is_safe_relative_path path =
      let path = String.trim path in
      if path = "" then false
      else if path.[0] = '/' || path.[0] = '~' then false
      else
        let segs = String.split_on_char '/' path in
        not (List.exists (fun seg -> seg = "..") segs)
    in
    let read_file path =
      try
        In_channel.with_open_bin path (fun ic ->
          let len = in_channel_length ic in
          Ok (really_input_string ic len))
      with Sys_error msg -> Error msg
    in
    let default_paths = [ "figma-code-connect.json"; ".figma/code-connect.json" ] in
    let resolve_source () =
      match get_string "json" args with
      | Some s when String.trim s <> "" -> Ok (s, None)
      | _ ->
          let path_opt =
            match get_string "path" args with
            | Some p when String.trim p <> "" -> Some p
            | _ -> List.find_opt Sys.file_exists default_paths
          in
          (match path_opt with
           | None ->
               Error
                 "Mapping not found. Provide 'json' or 'path' (default search: ./figma-code-connect.json, ./.figma/code-connect.json)."
           | Some path ->
               if not (is_safe_relative_path path) then
                 Error "Unsafe mapping path (must be relative, no '..', no '~', no absolute path)."
               else if not (Sys.file_exists path) then
                 Error (Printf.sprintf "Mapping file not found: %s" path)
               else
                 (match read_file path with
                  | Ok content -> Ok (content, Some path)
                  | Error msg -> Error (Printf.sprintf "Failed to read mapping file: %s" msg)))
    in
    let diag_to_json (d : Figma_code_connect.diagnostic) =
      `Assoc [ ("message", `String d.message); ("path", `String d.path) ]
    in
    let json_of_kv kv =
      `Assoc (List.map (fun (k, v) -> (k, `String v)) kv)
    in
    let json_of_figma (f : Figma_code_connect.figma) =
      `Assoc
        (List.filter_map
           (fun (k, v) -> v |> Option.map (fun v -> (k, `String v)))
           [ ("node_id", f.node_id); ("component_key", f.component_key) ]
         @ [
           ("name", `String f.name);
           ("variant", json_of_kv f.variant);
         ])
    in
    let json_of_code (c : Figma_code_connect.code) =
      `Assoc
        (List.filter_map
           (fun (k, v) -> v |> Option.map (fun v -> (k, `String v)))
           [ ("package", c.package); ("file", c.file) ]
         @ [
           ("export", `String c.export);
           ("props", json_of_kv c.props);
         ])
    in
    let json_of_component (c : Figma_code_connect.component) =
      `Assoc
        [
          ("id", `String c.id);
          ("figma", json_of_figma c.figma);
          ("code", json_of_code c.code);
          ("aliases", `List (List.map (fun s -> `String s) c.aliases));
          ("tags", `List (List.map (fun s -> `String s) c.tags));
        ]
    in
    let parse_mapping_content content =
      try
        let json = Yojson.Safe.from_string content in
        let mapping, parse_errors = Figma_code_connect.parse_json json in
        Ok (mapping, parse_errors)
      with Yojson.Json_error msg ->
        Error (Printf.sprintf "Failed to parse JSON: %s" msg)
    in

    let find_mapping_for_match () =
      match get_string "index_id" args with
      | Some index_id when String.trim index_id <> "" -> (
          match Hashtbl.find_opt code_connect_index_cache index_id with
          | Some mapping -> Ok (mapping, Some index_id, None)
          | None -> Error (Printf.sprintf "Unknown index_id: %s (run mode=index first)" index_id))
      | _ ->
          (match resolve_source () with
           | Ok (content, src_path) ->
               (match parse_mapping_content content with
                | Ok (mapping, parse_errors) ->
                    let semantic_errors = Figma_code_connect.validate mapping in
                    let errors = parse_errors @ semantic_errors in
                    if errors <> [] then
                      let result =
                        `Assoc
                          [
                            ("ok", `Bool false);
                            ("errors", `List (List.map diag_to_json errors));
                            ("warnings", `List []);
                          ]
                      in
                      Error (Yojson.Safe.pretty_to_string result)
                    else Ok (mapping, None, src_path)
                | Error msg -> Error msg)
           | Error msg -> Error msg)
    in

    match mode with
    | "validate" -> (
        match resolve_source () with
        | Ok (content, src_path) -> (
            match parse_mapping_content content with
            | Ok (mapping, parse_errors) ->
                let errors = parse_errors @ Figma_code_connect.validate mapping in
                let result =
                  `Assoc
                    [
                      ("ok", `Bool (errors = []));
                      ("errors", `List (List.map diag_to_json errors));
                      ("warnings", `List []);
                      ("source", (match src_path with Some p -> `String p | None -> `String "inline"));
                    ]
                in
                Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | Error msg -> Error msg)
        | Error msg -> Error msg)
    | "index" -> (
        match resolve_source () with
        | Ok (content, src_path) -> (
            match parse_mapping_content content with
            | Ok (mapping, parse_errors) ->
                let errors = parse_errors @ Figma_code_connect.validate mapping in
                if errors <> [] then
                  let result =
                    `Assoc
                      [
                        ("ok", `Bool false);
                        ("errors", `List (List.map diag_to_json errors));
                        ("warnings", `List []);
                      ]
                  in
                  Ok (make_text_content (Yojson.Safe.pretty_to_string result))
                else
                  let index_id =
                    match get_string "cache_key" args with
                    | Some k when String.trim k <> "" -> k
                    | _ -> Digest.(to_hex (string content))
                  in
                  code_connect_cache_put index_id mapping;
                  let result =
                    `Assoc
                      [
                        ("ok", `Bool true);
                        ("index_id", `String index_id);
                        ("component_count", `Int (List.length mapping.components));
                        ("warnings", `List []);
                        ("source", (match src_path with Some p -> `String p | None -> `String "inline"));
                      ]
                  in
                  Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | Error msg -> Error msg)
        | Error msg -> Error msg)
    | "list" -> (
        match find_mapping_for_match () with
        | Ok (mapping, index_id_opt, src_path) ->
            let result =
              `Assoc
                [
                  ("ok", `Bool true);
                  ("index_id", (match index_id_opt with Some id -> `String id | None -> `Null));
                  ("source", (match src_path with Some p -> `String p | None -> `Null));
                  ("component_count", `Int (List.length mapping.components));
                  ("components", `List (List.map json_of_component mapping.components));
                ]
            in
            Ok (make_text_content (Yojson.Safe.pretty_to_string result))
        | Error msg -> Error msg)
    | "match" -> (
        match find_mapping_for_match () with
        | Ok (mapping, index_id_opt, src_path) ->
            let query_node_id = get_string "node_id" args in
            let query_component_key = get_string "component_key" args in
            let query_name = get_string_or "name" "" args in
            let query_variant =
              match get_json "variant" args with
              | Some (`Assoc kv) ->
                  List.filter_map
                    (fun (k, v) ->
                      match v with
                      | `String s -> Some (k, s)
                      | `Int i -> Some (k, string_of_int i)
                      | `Float f -> Some (k, string_of_float f)
                      | _ -> None)
                    kv
              | _ -> []
            in
            let limit = get_int_positive ~min:0 "limit" 3 args in
            let matches =
              Figma_code_connect.choose ~limit ~query_name ~query_variant ~query_node_id
                ~query_component_key mapping.components
            in
            let matches_json =
              `List
                (List.map
                   (fun (score, reason, (c : Figma_code_connect.component)) ->
                     `Assoc
                       [
                         ("mapping_id", `String c.id);
                         ("score", `Float score);
                         ("reason", `String reason);
                         ("code", json_of_code c.code);
                       ])
                   matches)
            in
            let result =
              `Assoc
                [
                  ("ok", `Bool true);
                  ("index_id", (match index_id_opt with Some id -> `String id | None -> `Null));
                  ("source", (match src_path with Some p -> `String p | None -> `Null));
                  ("matches", matches_json);
                ]
            in
            Ok (make_text_content (Yojson.Safe.pretty_to_string result))
        | Error msg -> Error msg)
    | _ -> Error (Printf.sprintf "Unknown mode: %s (use validate|index|match|list)" mode)

(** ============== 핸들러 맵 (Pure Eio) ============== *)

(** 동기 래퍼 - Pure Eio Effect 핸들러로 감싸서 실행 *)
let wrap_sync_pure (f : Yojson.Safe.t -> (Yojson.Safe.t, string) result) : tool_handler_sync =
  fun args ->
    match get_eio_context () with
    | Some ctx ->
        let (Net net) = ctx.net in
        let (Clock clock) = ctx.clock in
        let same_domain = ctx.domain = Domain.self () in
        let run_with sw =
          Figma_effects.run_with_pure_eio_api ~sw ~net ~clock ~client:ctx.client (fun () -> f args)
        in
        if same_domain then
          run_with ctx.sw
        else
          Eio.Switch.run run_with
    | None ->
        Error "Eio context not set - server not properly initialized"

(** 순수 함수 핸들러들 *)
let handle_codegen_sync args : (Yojson.Safe.t, string) result =
  let json_str = get_string "json" args in
  let format = get_string_or "format" "fidelity" args in
  match json_str with
  | None -> Error "Missing required parameter: json"
  | Some json_str ->
      (match process_json_string ~format json_str with
       | Ok result -> Ok (make_text_content result)
       | Error msg -> Error msg)

(** 카테고리 도구 핸들러 - tool 파라미터로 하위 도구 실행 또는 목록 반환 *)
let handle_category category_name args =
  let mode_param = get_string "mode" args |> Option.map String.lowercase_ascii in
  let tool_param = get_string "tool" args in
  let args_param = member "args" args in

  (* #161: mode validation을 Eio 컨텍스트 진입 전에 즉시 수행 *)
  match mode_param with
  | Some m when m <> "list" && m <> "describe" && m <> "call" ->
      Error (sprintf "Invalid mode: %s (use list|describe|call)" m)
  | _ ->

  let find_tool_def (full_name : string) : tool_def option =
    List.find_opt (fun (t : tool_def) -> t.name = full_name) all_detailed_tools
  in

  try
    let effective_mode : [ `List | `Describe | `Call ] =
      match mode_param with
      | Some "list" -> `List
      | Some "describe" -> `Describe
      | Some "call" -> `Call
      | Some _ -> assert false (* validated above *)
      | None ->
          match tool_param, args_param with
          | None, _ -> `List
          | Some _, None -> `Describe
          | Some _, Some _ -> `Call
    in

    match effective_mode with
    | `List ->
        (match List.find_opt (fun c -> c.name = category_name) tool_categories with
         | Some cat ->
             let tools_info =
               List.map (fun tool_name ->
                 let full_name = "figma_" ^ tool_name in
                 let desc =
                   match find_tool_def full_name with
                   | Some t -> `String t.description
                   | None -> `Null
                 in
                 `Assoc [
                   ("name", `String tool_name);
                   ("full_name", `String full_name);
                   ("description", desc);
                 ]
               ) cat.tools
             in
             let info =
               `Assoc [
                 ("category", `String category_name);
                 ("description", `String cat.description);
                 ("tool_count", `Int (List.length cat.tools));
                 ("tools", `List tools_info);
                 ("usage", `String (sprintf "figma_%s mode=call tool=<tool_name> args={...}" category_name));
                 ("usage_describe", `String (sprintf "figma_%s mode=describe tool=<tool_name>" category_name));
               ]
             in
             Ok (make_text_content (Yojson.Safe.pretty_to_string info))
         | None ->
             Error (sprintf "Unknown category: %s" category_name))

    | `Describe ->
        (match tool_param with
         | None ->
             Error (sprintf "Missing required parameter: tool (category=%s)" category_name)
         | Some tool_name ->
             if not (find_tool_in_category category_name tool_name) then
               Error (sprintf "Tool '%s' not found in category '%s'. Use figma_%s mode=list to see available tools." tool_name category_name category_name)
             else
               let full_name = "figma_" ^ tool_name in
               (match find_tool_def full_name with
                | Some t ->
                    let info =
                      `Assoc [
                        ("category", `String category_name);
                        ("name", `String tool_name);
                        ("full_name", `String full_name);
                        ("description", `String t.description);
                        ("input_schema", t.input_schema);
                        ("usage", `String (sprintf "figma_%s mode=call tool=%s args={...}" category_name tool_name));
                      ]
                    in
                    Ok (make_text_content (Yojson.Safe.pretty_to_string info))
                | None ->
                    Error (sprintf "Tool '%s' exists but tool definition not found. Try 'tools/list'." full_name)))

    | `Call ->
        (match tool_param with
         | None ->
             Error (sprintf "Missing required parameter: tool (category=%s)" category_name)
         | Some tool_name ->
             if not (find_tool_in_category category_name tool_name) then
               Error (sprintf "Tool '%s' not found in category '%s'. Use figma_%s mode=list to see available tools." tool_name category_name category_name)
             else
               let full_name = "figma_" ^ tool_name in
               match Hashtbl.find_opt handler_registry full_name with
               | Some handler ->
                   let actual_args = match args_param with
                    | Some a -> Some a
                    | None ->
                        (* Flat params fallback: mode/tool을 제외한 나머지를 args로 취급 *)
                        match args with
                        | `Assoc pairs ->
                            let filtered = List.filter (fun (k, _) ->
                              k <> "mode" && k <> "tool") pairs in
                            if filtered = [] then None
                            else Some (`Assoc filtered)
                        | _ -> None
                   in
                   (match actual_args with
                    | None ->
                        Error (sprintf "Missing required parameter: args (mode=call). \
                          Usage: figma_%s mode=call tool=%s args={...}" category_name tool_name)
                    | Some a ->
                        (* #160: handler 결과가 make_text_content 형태가 아니면 래핑 *)
                        (match handler a with
                         | Ok json ->
                             let has_content =
                               match json with
                               | `Assoc fields -> List.mem_assoc "content" fields
                               | _ -> false
                             in
                             if has_content then Ok json
                             else Ok (make_text_content (Yojson.Safe.pretty_to_string json))
                         | Error msg -> Error msg))
               | None ->
                   Error (sprintf "Tool '%s' exists but handler not found. Try 'figma_%s' directly." tool_name tool_name))
  with
  | Invalid_argument msg -> Error msg
