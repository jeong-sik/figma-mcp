(** Figma Team Crawler (Effects-based)

    팀 전체를 재귀적으로 크롤링하여 Neo4j에 저장합니다.
    OCaml 5.x Algebraic Effects 사용.

    구조:
      Team -> Projects -> Files -> Nodes (recursive)

    Usage:
      let neo4j_cfg = Figma_crawl.create_neo4j_config
        ~uri:"http://localhost:7474" ~user:"neo4j" ~password:"pass" () in
      let result = Figma_crawl.crawl_team ~token ~team_id ~neo4j_cfg
        ~on_progress:(fun msg -> print_endline msg) () in
      (* result is an effectful computation - run with Figma_effects.run_with_pure_eio_api *)
*)

open Printf

(** 크롤링 진행 상황 *)
type progress = {
  mutable teams: int;
  mutable projects: int;
  mutable files: int;
  mutable nodes: int;
  mutable errors: string list;
}

let create_progress () = {
  teams = 0;
  projects = 0;
  files = 0;
  nodes = 0;
  errors = [];
}

(** 크롤링 옵션 *)
type crawl_options = {
  max_depth: int;           (** 노드 탐색 최대 깊이 (기본: 10) *)
  include_hidden: bool;     (** 숨김 노드 포함 여부 *)
  batch_size: int;          (** Neo4j 배치 크기 (기본: 100) *)
  rate_limit_ms: int;       (** API 호출 간 대기 시간 (ms) *)
  skip_files: string list;  (** 스킵할 파일 키 목록 *)
}

let default_options = {
  max_depth = 10;
  include_hidden = false;
  batch_size = 100;
  rate_limit_ms = 100;
  skip_files = [];
}

(** Neo4j 설정 (Effects 호출용) *)
type neo4j_config = {
  uri: string;
  database: string;
  auth_header: string;
}

let create_neo4j_config ~uri ?(database="neo4j") ~user ~password () =
  let credentials = sprintf "%s:%s" user password in
  let auth_header = "Basic " ^ Base64.encode_string credentials in
  { uri; database; auth_header }

let create_neo4j_config_from_env () =
  let uri = Sys.getenv_opt "NEO4J_URI" |> Option.value ~default:"http://localhost:7474" in
  let database = Sys.getenv_opt "NEO4J_DATABASE" |> Option.value ~default:"neo4j" in
  let user = Sys.getenv_opt "NEO4J_USER" |> Option.value ~default:"neo4j" in
  let password = Sys.getenv_opt "NEO4J_PASSWORD" |> Option.value ~default:"" in
  create_neo4j_config ~uri ~database ~user ~password ()

(** JSON 헬퍼 *)
let get_string key json =
  match Yojson.Safe.Util.member key json with
  | `String s -> Some s
  | _ -> None

let get_string_or key default json =
  get_string key json |> Option.value ~default

let get_list key json =
  match Yojson.Safe.Util.member key json with
  | `List l -> l
  | _ -> []

(** Rate limiting *)
let rate_limit ms =
  if ms > 0 then
    Unix.sleepf (float_of_int ms /. 1000.0)

(** Neo4j Cypher 실행 (Effects 사용) *)
let run_cypher ~neo4j_cfg query params =
  Figma_effects.Perform.neo4j_run_cypher
    ~uri:neo4j_cfg.uri
    ~database:neo4j_cfg.database
    ~auth_header:neo4j_cfg.auth_header
    ~query
    ~params

let run_batch ~neo4j_cfg queries =
  Figma_effects.Perform.neo4j_run_batch
    ~uri:neo4j_cfg.uri
    ~database:neo4j_cfg.database
    ~auth_header:neo4j_cfg.auth_header
    ~queries

(** 팀의 프로젝트 목록 가져오기 (Effect) *)
let fetch_projects ~token ~team_id =
  match Figma_effects.Perform.get_team_projects ~token ~team_id with
  | Ok json ->
      let projects = get_list "projects" json in
      let result = List.filter_map (fun p ->
        match (get_string "id" p, get_string "name" p) with
        | (Some id, Some name) -> Some (id, name)
        | _ -> None
      ) projects in
      Ok result
  | Error err -> Error err

(** 프로젝트의 파일 목록 가져오기 (Effect) *)
let fetch_files ~token ~project_id =
  match Figma_effects.Perform.get_project_files ~token ~project_id with
  | Ok json ->
      let files = get_list "files" json in
      let result = List.filter_map (fun f ->
        let key = get_string "key" f in
        let name = get_string "name" f in
        let last_modified = get_string_or "last_modified" "" f in
        match (key, name) with
        | (Some key, Some name) -> Some (key, name, last_modified)
        | _ -> None
      ) files in
      Ok result
  | Error err -> Error err

(** 파일의 노드 트리 가져오기 (Effect) *)
let fetch_file_nodes ~token ~file_key =
  match Figma_effects.Perform.get_file ~token ~file_key () with
  | Ok json ->
      let document = Yojson.Safe.Util.member "document" json in
      Ok document
  | Error err -> Error err

(** 노드 트리를 평탄화 (재귀) *)
let rec flatten_nodes ~file_key ~parent_id ~depth ~max_depth node acc =
  if depth > max_depth then acc
  else
    let node_id = get_string_or "id" "" node in
    let name = get_string_or "name" "" node in
    let node_type = get_string_or "type" "" node in

    (* 현재 노드 추가 *)
    let acc = (node_id, name, node_type, file_key, parent_id) :: acc in

    (* 자식 노드 재귀 처리 *)
    let children = get_list "children" node in
    List.fold_left (fun acc child ->
      flatten_nodes ~file_key ~parent_id:(Some node_id) ~depth:(depth + 1) ~max_depth child acc
    ) acc children

(** 노드들을 Neo4j에 배치 저장 (Effect) *)
let save_nodes_batch ~neo4j_cfg ~progress nodes =
  let batch_queries = List.map (fun (node_id, name, node_type, file_key, _parent_id) ->
    let query =
      "MERGE (n:FigmaNode {id: $id}) \
       SET n.name = $name, n.type = $type, n.file_key = $file_key"
    in
    let params = [
      ("id", `String node_id);
      ("name", `String name);
      ("type", `String node_type);
      ("file_key", `String file_key);
    ] in
    (query, params)
  ) nodes in

  match run_batch ~neo4j_cfg batch_queries with
  | Ok _ ->
      progress.nodes <- progress.nodes + List.length nodes;
      Ok ()
  | Error msg ->
      progress.errors <- msg :: progress.errors;
      Error msg

(** 노드 관계 저장 (parent-child) (Effect) *)
let save_node_relationships ~neo4j_cfg nodes =
  let rel_queries = List.filter_map (fun (node_id, _, _, file_key, parent_id) ->
    match parent_id with
    | Some pid ->
        let query =
          "MATCH (p:FigmaNode {id: $parent_id}), (c:FigmaNode {id: $child_id}) \
           MERGE (p)-[:HAS_CHILD]->(c)"
        in
        Some (query, [("parent_id", `String pid); ("child_id", `String node_id)])
    | None ->
        (* 루트 노드는 파일에 연결 *)
        let query =
          "MATCH (f:FigmaFile {key: $file_key}), (n:FigmaNode {id: $node_id}) \
           MERGE (f)-[:HAS_NODE]->(n)"
        in
        Some (query, [("file_key", `String file_key); ("node_id", `String node_id)])
  ) nodes in

  match run_batch ~neo4j_cfg rel_queries with
  | Ok _ -> Ok ()
  | Error msg -> Error msg

(** Figma 팀 노드 생성 (Effect) *)
let create_figma_team ~neo4j_cfg ~team_id ~name =
  let query =
    "MERGE (t:FigmaTeam {id: $id}) \
     SET t.name = $name, t.synced_at = $synced_at \
     RETURN t"
  in
  let params = [
    ("id", `String team_id);
    ("name", `String name);
    ("synced_at", `String (Unix.gettimeofday () |> string_of_float));
  ] in
  run_cypher ~neo4j_cfg query params

(** Figma 프로젝트 노드 생성 (Effect) *)
let create_figma_project ~neo4j_cfg ~project_id ~name ~team_id =
  let query =
    "MERGE (p:FigmaProject {id: $id}) \
     SET p.name = $name \
     WITH p \
     MATCH (t:FigmaTeam {id: $team_id}) \
     MERGE (t)-[:HAS_PROJECT]->(p) \
     RETURN p"
  in
  let params = [
    ("id", `String project_id);
    ("name", `String name);
    ("team_id", `String team_id);
  ] in
  run_cypher ~neo4j_cfg query params

(** Figma 파일 노드 생성 (Effect) *)
let create_figma_file ~neo4j_cfg ~file_key ~name ~project_id ~last_modified =
  let query =
    "MERGE (f:FigmaFile {key: $key}) \
     SET f.name = $name, f.last_modified = $last_modified \
     WITH f \
     MATCH (p:FigmaProject {id: $project_id}) \
     MERGE (p)-[:HAS_FILE]->(f) \
     RETURN f"
  in
  let params = [
    ("key", `String file_key);
    ("name", `String name);
    ("last_modified", `String last_modified);
    ("project_id", `String project_id);
  ] in
  run_cypher ~neo4j_cfg query params

(** Neo4j 연결 테스트 (Effect) *)
let test_connection ~neo4j_cfg =
  match run_cypher ~neo4j_cfg "RETURN 1 as test" [] with
  | Ok _ -> Ok ()
  | Error msg -> Error msg

(** 단일 파일 크롤링 (Effect) *)
let crawl_file ~token ~neo4j_cfg ~progress ~options ~project_id ~file_key ~file_name ~last_modified ~on_progress =
  on_progress (sprintf "  📄 File: %s (%s)" file_name file_key);

  (* 파일 노드 생성 *)
  let _ = create_figma_file ~neo4j_cfg
    ~file_key ~name:file_name ~project_id ~last_modified
  in
  progress.files <- progress.files + 1;

  (* 파일 노드 트리 가져오기 *)
  rate_limit options.rate_limit_ms;
  match fetch_file_nodes ~token ~file_key with
  | Ok document ->
      (* 노드 평탄화 *)
      let flat_nodes = flatten_nodes
        ~file_key
        ~parent_id:None
        ~depth:0
        ~max_depth:options.max_depth
        document []
      in
      on_progress (sprintf "    Found %d nodes" (List.length flat_nodes));

      (* 배치로 저장 *)
      let rec save_in_batches nodes =
        match nodes with
        | [] -> Ok ()
        | _ ->
            let batch, rest =
              let rec take n acc lst =
                if n = 0 then (List.rev acc, lst)
                else match lst with
                  | [] -> (List.rev acc, [])
                  | x :: xs -> take (n - 1) (x :: acc) xs
              in
              take options.batch_size [] nodes
            in
            match save_nodes_batch ~neo4j_cfg ~progress batch with
            | Ok () ->
                let _ = save_node_relationships ~neo4j_cfg batch in
                save_in_batches rest
            | Error _ as e -> e
      in
      save_in_batches flat_nodes

  | Error err ->
      progress.errors <- err :: progress.errors;
      on_progress (sprintf "    ⚠️ Error: %s" err);
      Error err

(** 단일 프로젝트 크롤링 (Effect) *)
let crawl_project ~token ~neo4j_cfg ~progress ~options ~team_id ~project_id ~project_name ~on_progress =
  on_progress (sprintf "📁 Project: %s (%s)" project_name project_id);

  (* 프로젝트 노드 생성 *)
  let _ = create_figma_project ~neo4j_cfg
    ~project_id ~name:project_name ~team_id
  in
  progress.projects <- progress.projects + 1;

  (* 파일 목록 가져오기 *)
  rate_limit options.rate_limit_ms;
  match fetch_files ~token ~project_id with
  | Ok files ->
      on_progress (sprintf "  Found %d files" (List.length files));
      let results = List.map (fun (file_key, file_name, last_modified) ->
        if List.mem file_key options.skip_files then begin
          on_progress (sprintf "  ⏭️ Skipping: %s" file_name);
          Ok ()
        end else
          crawl_file ~token ~neo4j_cfg ~progress ~options
            ~project_id ~file_key ~file_name ~last_modified ~on_progress
      ) files in
      (* 에러가 있어도 계속 진행 *)
      let _ = results in
      Ok ()

  | Error err ->
      progress.errors <- err :: progress.errors;
      on_progress (sprintf "  ⚠️ Error fetching files: %s" err);
      Error err

(** 팀 전체 크롤링 (Effect-based computation)

    이 함수는 Effects를 사용하는 computation을 반환합니다.
    실행하려면 Figma_effects.run_with_pure_eio_api로 래핑해야 합니다.
*)
let crawl_team ~token ~team_id ~neo4j_cfg
    ?(options=default_options)
    ?(team_name="Unknown Team")
    ~on_progress
    () =

  let progress = create_progress () in

  on_progress (sprintf "🏢 Starting crawl for team: %s (%s)" team_name team_id);
  on_progress "─────────────────────────────────────────";

  (* Neo4j 연결 테스트 *)
  (match test_connection ~neo4j_cfg with
   | Ok () -> on_progress "✅ Neo4j connection OK"
   | Error err ->
       on_progress (sprintf "❌ Neo4j connection failed: %s" err);
       failwith err);

  (* 팀 노드 생성 *)
  let _ = create_figma_team ~neo4j_cfg ~team_id ~name:team_name in
  progress.teams <- 1;

  (* 프로젝트 목록 가져오기 *)
  on_progress "📋 Fetching projects...";
  match fetch_projects ~token ~team_id with
  | Ok projects ->
      on_progress (sprintf "Found %d projects" (List.length projects));
      on_progress "─────────────────────────────────────────";

      (* 각 프로젝트 크롤링 *)
      List.iter (fun (project_id, project_name) ->
        let _ = crawl_project ~token ~neo4j_cfg ~progress ~options
          ~team_id ~project_id ~project_name ~on_progress
        in
        ()
      ) projects;

      (* 결과 요약 *)
      on_progress "─────────────────────────────────────────";
      on_progress "📊 Crawl Summary:";
      on_progress (sprintf "  Teams: %d" progress.teams);
      on_progress (sprintf "  Projects: %d" progress.projects);
      on_progress (sprintf "  Files: %d" progress.files);
      on_progress (sprintf "  Nodes: %d" progress.nodes);
      if progress.errors <> [] then begin
        on_progress (sprintf "  Errors: %d" (List.length progress.errors));
        List.iter (fun e -> on_progress (sprintf "    - %s" e)) progress.errors
      end;

      Ok progress

  | Error err ->
      on_progress (sprintf "❌ Failed to fetch projects: %s" err);
      Error err

(** 크롤링 결과를 JSON으로 변환 *)
let progress_to_json progress =
  `Assoc [
    ("teams", `Int progress.teams);
    ("projects", `Int progress.projects);
    ("files", `Int progress.files);
    ("nodes", `Int progress.nodes);
    ("errors", `List (List.map (fun e -> `String e) progress.errors));
  ]

(** 간단한 진행 상황 출력 콜백 *)
let stdout_progress msg =
  print_endline msg;
  flush stdout

(** 진행 상황을 버퍼에 저장하는 콜백 생성 *)
let buffer_progress () =
  let buf = Buffer.create 1024 in
  let callback msg =
    Buffer.add_string buf msg;
    Buffer.add_char buf '\n'
  in
  (callback, fun () -> Buffer.contents buf)
