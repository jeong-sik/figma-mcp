(** Neo4j HTTP Transactional API Client

    Neo4j HTTP API를 사용하여 Cypher 쿼리를 실행합니다.
    Bolt 드라이버 없이 HTTP API만으로 동작합니다.

    Usage:
      let client = Neo4j_client.create ~uri:"http://localhost:7474" ~user:"neo4j" ~password:"password" () in
      Neo4j_client.run_cypher client "CREATE (n:Test {name: $name})" [("name", `String "test")]
*)

open Printf

(** Neo4j 클라이언트 설정 *)
type t = {
  uri: string;           (** Neo4j HTTP endpoint (e.g., http://localhost:7474) *)
  database: string;      (** Database name (default: neo4j) *)
  auth_header: string;   (** Basic auth header value *)
}

(** Cypher 쿼리 결과 *)
type query_result = {
  columns: string list;
  data: Yojson.Safe.t list list;
  stats: Yojson.Safe.t option;
}

(** 에러 타입 *)
type neo4j_error = {
  code: string;
  message: string;
}

(** 클라이언트 생성 *)
let create ~uri ?(database="neo4j") ~user ~password () =
  let credentials = sprintf "%s:%s" user password in
  let auth_header = "Basic " ^ Base64.encode_string credentials in
  { uri; database; auth_header }

(** 환경변수에서 클라이언트 생성 *)
let create_from_env () =
  let uri = Sys.getenv_opt "NEO4J_URI" |> Option.value ~default:"http://localhost:7474" in
  let database = Sys.getenv_opt "NEO4J_DATABASE" |> Option.value ~default:"neo4j" in
  let user = Sys.getenv_opt "NEO4J_USER" |> Option.value ~default:"neo4j" in
  let password = Sys.getenv_opt "NEO4J_PASSWORD" |> Option.value ~default:"" in
  create ~uri ~database ~user ~password ()

(** JSON 파라미터를 Neo4j 형식으로 변환 *)
let params_to_json (params : (string * Yojson.Safe.t) list) : Yojson.Safe.t =
  `Assoc params

(** Cypher 문 생성 *)
let make_statement query params =
  let params_json = params_to_json params in
  `Assoc [
    ("statement", `String query);
    ("parameters", params_json);
  ]

(** HTTP 요청 바디 생성 *)
let make_request_body statements =
  `Assoc [("statements", `List statements)]
  |> Yojson.Safe.to_string

(** 응답 파싱 *)
let parse_response body : (query_result list, neo4j_error list) result =
  try
    let json = Yojson.Safe.from_string body in
    let errors = match Yojson.Safe.Util.member "errors" json with
      | `List errs ->
          List.filter_map (fun e ->
            let code = Yojson.Safe.Util.(member "code" e |> to_string_option) |> Option.value ~default:"" in
            let message = Yojson.Safe.Util.(member "message" e |> to_string_option) |> Option.value ~default:"" in
            if code = "" && message = "" then None
            else Some { code; message }
          ) errs
      | _ -> []
    in
    if errors <> [] then Error errors
    else
      let results = match Yojson.Safe.Util.member "results" json with
        | `List res ->
            List.map (fun r ->
              let columns = match Yojson.Safe.Util.member "columns" r with
                | `List cols -> List.filter_map Yojson.Safe.Util.to_string_option cols
                | _ -> []
              in
              let data = match Yojson.Safe.Util.member "data" r with
                | `List rows ->
                    List.map (fun row ->
                      match Yojson.Safe.Util.member "row" row with
                      | `List vals -> vals
                      | _ -> []
                    ) rows
                | _ -> []
              in
              let stats = match Yojson.Safe.Util.member "stats" r with
                | `Null -> None
                | s -> Some s
              in
              { columns; data; stats }
            ) res
        | _ -> []
      in
      Ok results
  with
  | Yojson.Json_error msg -> Error [{ code = "JSON_PARSE_ERROR"; message = msg }]
  | _ -> Error [{ code = "UNKNOWN_ERROR"; message = "Failed to parse response" }]

(** Base64 인코딩 - 외부 라이브러리 사용 *)
let base64_encode s = Base64.encode_string s

(** Eio 기반 HTTP POST 요청 *)
let http_post ~sw ~env client endpoint body =
  let uri = Uri.of_string (client.uri ^ endpoint) in
  let headers = Cohttp.Header.of_list [
    ("Content-Type", "application/json");
    ("Accept", "application/json");
    ("Authorization", client.auth_header);
  ] in
  let body_str = body in
  let _net = Eio.Stdenv.net env in

  (* cohttp-eio를 사용한 HTTP POST *)
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  let resp, resp_body =
    Cohttp_eio.Client.post
      ~sw
      client
      ~headers
      ~body:(Cohttp_eio.Body.of_string body_str)
      uri
  in
  let status = Cohttp.Response.status resp in
  let body_content = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int in
  (status, body_content)

(** Cypher 쿼리 실행 (단일) *)
let run_cypher ~sw ~env client query params =
  let statement = make_statement query params in
  let body = make_request_body [statement] in
  let endpoint = sprintf "/db/%s/tx/commit" client.database in
  let status, response = http_post ~sw ~env client endpoint body in
  match Cohttp.Code.code_of_status status with
  | 200 | 201 ->
      (match parse_response response with
       | Ok results -> Ok (List.nth_opt results 0)
       | Error errs -> Error (String.concat "; " (List.map (fun e -> e.message) errs)))
  | code ->
      Error (sprintf "HTTP %d: %s" code response)

(** 여러 Cypher 쿼리 배치 실행 *)
let run_batch ~sw ~env client queries =
  let statements = List.map (fun (query, params) -> make_statement query params) queries in
  let body = make_request_body statements in
  let endpoint = sprintf "/db/%s/tx/commit" client.database in
  let status, response = http_post ~sw ~env client endpoint body in
  match Cohttp.Code.code_of_status status with
  | 200 | 201 -> parse_response response
  | code -> Error [{ code = sprintf "HTTP_%d" code; message = response }]

(** 노드 생성 헬퍼 *)
let create_node ~sw ~env client label props =
  let props_list = List.map (fun (k, v) -> sprintf "%s: $%s" k k) props in
  let props_str = String.concat ", " props_list in
  let query = sprintf "CREATE (n:%s {%s}) RETURN n" label props_str in
  run_cypher ~sw ~env client query props

(** 관계 생성 헬퍼 *)
let create_relationship ~sw ~env client
    ~from_label ~from_key ~from_value
    ~to_label ~to_key ~to_value
    ~rel_type =
  let query = sprintf
    "MATCH (a:%s {%s: $from_val}), (b:%s {%s: $to_val}) \
     MERGE (a)-[r:%s]->(b) RETURN r"
    from_label from_key to_label to_key rel_type
  in
  let params = [
    ("from_val", from_value);
    ("to_val", to_value);
  ] in
  run_cypher ~sw ~env client query params

(** MERGE (upsert) 노드 *)
let merge_node ~sw ~env client label key_prop props =
  let key_name, key_value = key_prop in
  let set_props = List.filter (fun (k, _) -> k <> key_name) props in
  let set_strs = List.map (fun (k, _) -> sprintf "n.%s = $%s" k k) set_props in
  let set_clause = if set_strs = [] then "" else " SET " ^ String.concat ", " set_strs in
  let query = sprintf "MERGE (n:%s {%s: $%s})%s RETURN n" label key_name key_name set_clause in
  run_cypher ~sw ~env client query props

(** 연결 테스트 *)
let test_connection ~sw ~env client =
  match run_cypher ~sw ~env client "RETURN 1 as test" [] with
  | Ok _ -> Ok ()
  | Error msg -> Error msg

(** Figma 팀 노드 생성 *)
let create_figma_team ~sw ~env client ~team_id ~name =
  merge_node ~sw ~env client "FigmaTeam" ("id", `String team_id) [
    ("id", `String team_id);
    ("name", `String name);
    ("synced_at", `String (Unix.gettimeofday () |> string_of_float));
  ]

(** Figma 프로젝트 노드 생성 *)
let create_figma_project ~sw ~env client ~project_id ~name ~team_id =
  let _ = merge_node ~sw ~env client "FigmaProject" ("id", `String project_id) [
    ("id", `String project_id);
    ("name", `String name);
  ] in
  create_relationship ~sw ~env client
    ~from_label:"FigmaTeam" ~from_key:"id" ~from_value:(`String team_id)
    ~to_label:"FigmaProject" ~to_key:"id" ~to_value:(`String project_id)
    ~rel_type:"HAS_PROJECT"

(** Figma 파일 노드 생성 *)
let create_figma_file ~sw ~env client ~file_key ~name ~project_id ~last_modified =
  let _ = merge_node ~sw ~env client "FigmaFile" ("key", `String file_key) [
    ("key", `String file_key);
    ("name", `String name);
    ("last_modified", `String last_modified);
  ] in
  create_relationship ~sw ~env client
    ~from_label:"FigmaProject" ~from_key:"id" ~from_value:(`String project_id)
    ~to_label:"FigmaFile" ~to_key:"key" ~to_value:(`String file_key)
    ~rel_type:"HAS_FILE"

(** Figma 노드 생성 *)
let create_figma_node ~sw ~env client ~node_id ~name ~node_type ~file_key ~parent_id =
  let _ = merge_node ~sw ~env client "FigmaNode" ("id", `String node_id) [
    ("id", `String node_id);
    ("name", `String name);
    ("type", `String node_type);
    ("file_key", `String file_key);
  ] in
  match parent_id with
  | Some pid ->
      create_relationship ~sw ~env client
        ~from_label:"FigmaNode" ~from_key:"id" ~from_value:(`String pid)
        ~to_label:"FigmaNode" ~to_key:"id" ~to_value:(`String node_id)
        ~rel_type:"HAS_CHILD"
  | None ->
      create_relationship ~sw ~env client
        ~from_label:"FigmaFile" ~from_key:"key" ~from_value:(`String file_key)
        ~to_label:"FigmaNode" ~to_key:"id" ~to_value:(`String node_id)
        ~rel_type:"HAS_NODE"

(** 배치로 Figma 노드들 생성 (성능 최적화) *)
let batch_create_figma_nodes ~sw ~env client nodes =
  let queries = List.map (fun (node_id, name, node_type, file_key) ->
    let query =
      "MERGE (n:FigmaNode {id: $id}) \
       SET n.name = $name, n.type = $type, n.file_key = $file_key \
       RETURN n"
    in
    let params = [
      ("id", `String node_id);
      ("name", `String name);
      ("type", `String node_type);
      ("file_key", `String file_key);
    ] in
    (query, params)
  ) nodes in
  run_batch ~sw ~env client queries
