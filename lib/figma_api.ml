(** Figma REST API 클라이언트 (Lwt + Cohttp) *)

open Lwt.Syntax
open Printf

(** ============== API 설정 ============== *)

let api_base = "https://api.figma.com/v1"

(** ============== Node ID 변환 ============== *)

(** URL 형식 (2089-11127) → API 형식 (2089:11127)
    Figma URL에서 추출한 node-id를 API 호출용으로 변환 *)
let url_to_api_node_id id =
  Str.global_replace (Str.regexp "-") ":" id

(** API 형식 (2089:11127) → URL 형식 (2089-11127)
    API 응답의 node id를 URL용으로 변환 *)
let api_to_url_node_id id =
  Str.global_replace (Str.regexp ":") "-" id

let add_param key value params =
  match value with
  | None -> params
  | Some v -> (key, [v]) :: params

let with_query base params =
  let uri = Uri.of_string base in
  let uri =
    if params = [] then uri
    else Uri.with_query uri params
  in
  Uri.to_string uri

(** ============== 에러 타입 ============== *)

type api_error =
  | NetworkError of string
  | AuthError of string
  | NotFound of string
  | RateLimited
  | ParseError of string
  | UnknownError of int * string

let error_to_string = function
  | NetworkError msg -> sprintf "Network error: %s" msg
  | AuthError msg -> sprintf "Auth error: %s" msg
  | NotFound msg -> sprintf "Not found: %s" msg
  | RateLimited -> "Rate limited - please wait"
  | ParseError msg -> sprintf "Parse error: %s" msg
  | UnknownError (code, msg) -> sprintf "Error %d: %s" code msg

(** ============== HTTP 클라이언트 ============== *)

let make_headers token =
  Cohttp.Header.of_list [
    ("X-Figma-Token", token);
    ("Accept", "application/json");
  ]

let get_json ~token url : (Yojson.Safe.t, api_error) result Lwt.t =
  let uri = Uri.of_string url in
  let headers = make_headers token in

  Lwt.catch
    (fun () ->
       let* (resp, body) = Cohttp_lwt_unix.Client.get ~headers uri in
       let status = Cohttp.Response.status resp in
       let code = Cohttp.Code.code_of_status status in
       let* body_str = Cohttp_lwt.Body.to_string body in

       match code with
       | 200 ->
           (try
              Lwt.return_ok (Yojson.Safe.from_string body_str)
            with Yojson.Json_error msg ->
              Lwt.return_error (ParseError msg))
       | 401 | 403 ->
           Lwt.return_error (AuthError "Invalid or expired token")
       | 404 ->
           Lwt.return_error (NotFound url)
       | 429 ->
           Lwt.return_error RateLimited
       | _ ->
           Lwt.return_error (UnknownError (code, body_str)))
    (fun exn ->
       Lwt.return_error (NetworkError (Printexc.to_string exn)))

let post_json ~token url body_json : (Yojson.Safe.t, api_error) result Lwt.t =
  let uri = Uri.of_string url in
  let headers =
    make_headers token
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  in
  let body_str = Yojson.Safe.to_string body_json in
  let body = Cohttp_lwt.Body.of_string body_str in

  Lwt.catch
    (fun () ->
       let* (resp, body) = Cohttp_lwt_unix.Client.post ~headers ~body uri in
       let status = Cohttp.Response.status resp in
       let code = Cohttp.Code.code_of_status status in
       let* body_str = Cohttp_lwt.Body.to_string body in

       match code with
       | 200 | 201 ->
           (try
              Lwt.return_ok (Yojson.Safe.from_string body_str)
            with Yojson.Json_error msg ->
              Lwt.return_error (ParseError msg))
       | 401 | 403 ->
           Lwt.return_error (AuthError "Invalid or expired token")
       | 404 ->
           Lwt.return_error (NotFound url)
       | 429 ->
           Lwt.return_error RateLimited
       | _ ->
           Lwt.return_error (UnknownError (code, body_str)))
    (fun exn ->
       Lwt.return_error (NetworkError (Printexc.to_string exn)))

(** ============== File Download ============== *)

let rec mkdir_p dir =
  if dir = "" || dir = "/" then ()
  else if Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    Unix.mkdir dir 0o755
  end

let download_url ~url ~path : (string, string) result Lwt.t =
  let uri = Uri.of_string url in
  Lwt.catch
    (fun () ->
       let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
       let status = Cohttp.Response.status resp in
       let code = Cohttp.Code.code_of_status status in
       let* body_str = Cohttp_lwt.Body.to_string body in
       if code <> 200 then
         Lwt.return_error (Printf.sprintf "HTTP %d" code)
       else begin
         (try mkdir_p (Filename.dirname path) with _ -> ());
         let oc = open_out_bin path in
         output_string oc body_str;
         close_out oc;
         Lwt.return_ok path
       end)
    (fun exn -> Lwt.return_error (Printexc.to_string exn))

(** ============== Figma API 엔드포인트 ============== *)

(** 파일 전체 가져오기 *)
let get_file ?depth ?geometry ?plugin_data ?version ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result Lwt.t =
  let params =
    []
    |> add_param "version" version
    |> add_param "depth" (Option.map string_of_int depth)
    |> add_param "geometry" geometry
    |> add_param "plugin_data" plugin_data
  in
  let url = with_query (sprintf "%s/files/%s" api_base file_key) params in
  get_json ~token url

(** 파일 노드들만 가져오기 (특정 노드 ID들) *)
let get_file_nodes ?depth ?geometry ?plugin_data ?version ~token ~file_key ~node_ids ()
  : (Yojson.Safe.t, api_error) result Lwt.t =
  let ids_param = String.concat "," node_ids in
  let params =
    [("ids", [ids_param])]
    |> add_param "version" version
    |> add_param "depth" (Option.map string_of_int depth)
    |> add_param "geometry" geometry
    |> add_param "plugin_data" plugin_data
  in
  let url = with_query (sprintf "%s/files/%s/nodes" api_base file_key) params in
  get_json ~token url

(** 이미지 내보내기 URL 가져오기 *)
let get_images ?use_absolute_bounds ?version ~token ~file_key ~node_ids ~format ~scale ()
  : (Yojson.Safe.t, api_error) result Lwt.t =
  let ids_param = String.concat "," node_ids in
  let params =
    [("ids", [ids_param]);
     ("format", [format]);
     ("scale", [string_of_int scale])]
    |> add_param "use_absolute_bounds" (Option.map string_of_bool use_absolute_bounds)
    |> add_param "version" version
  in
  let url = with_query (sprintf "%s/images/%s" api_base file_key) params in
  get_json ~token url

(** 이미지 채움(image fills) 원본 URL 가져오기 *)
let get_file_images ?version ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result Lwt.t =
  let params =
    []
    |> add_param "version" version
  in
  let url = with_query (sprintf "%s/files/%s/images" api_base file_key) params in
  get_json ~token url

(** 파일 메타데이터(components/styles/componentSets) 가져오기 *)
let get_file_meta ?version ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result Lwt.t =
  let params =
    []
    |> add_param "version" version
  in
  let url = with_query (sprintf "%s/files/%s/meta" api_base file_key) params in
  get_json ~token url

(** 파일 컴포넌트 목록 *)
let get_file_components ~token ~file_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/components" api_base file_key in
  get_json ~token url

(** 팀 컴포넌트 목록 *)
let get_team_components ~token ~team_id : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/teams/%s/components" api_base team_id in
  get_json ~token url

(** 파일 컴포넌트 셋 목록 *)
let get_file_component_sets ~token ~file_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/component_sets" api_base file_key in
  get_json ~token url

(** 팀 컴포넌트 셋 목록 *)
let get_team_component_sets ~token ~team_id : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/teams/%s/component_sets" api_base team_id in
  get_json ~token url

(** 파일 스타일 목록 *)
let get_file_styles ~token ~file_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/styles" api_base file_key in
  get_json ~token url

(** 팀 스타일 목록 *)
let get_team_styles ~token ~team_id : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/teams/%s/styles" api_base team_id in
  get_json ~token url

(** 개별 컴포넌트 조회 *)
let get_component ~token ~component_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/components/%s" api_base component_key in
  get_json ~token url

(** 개별 컴포넌트 셋 조회 *)
let get_component_set ~token ~component_set_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/component_sets/%s" api_base component_set_key in
  get_json ~token url

(** 개별 스타일 조회 *)
let get_style ~token ~style_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/styles/%s" api_base style_key in
  get_json ~token url

(** 파일 버전 목록 *)
let get_file_versions ~token ~file_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/versions" api_base file_key in
  get_json ~token url

(** 파일 코멘트 목록 *)
let get_file_comments ~token ~file_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/comments" api_base file_key in
  get_json ~token url

(** 파일 코멘트 생성 *)
let post_file_comment ~token ~file_key ~message ~client_meta
  : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/comments" api_base file_key in
  let body =
    `Assoc [
      ("message", `String message);
      ("client_meta", client_meta);
    ]
  in
  post_json ~token url body

(** 기존 이름 호환 유지 *)
let get_components ~token ~file_key = get_file_components ~token ~file_key

(** ============== 팀/프로젝트 탐색 API (Phase 1) ============== *)

(** 현재 사용자 정보 *)
let get_me ~token : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/me" api_base in
  get_json ~token url

(** 팀의 프로젝트 목록 *)
let get_team_projects ~token ~team_id : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/teams/%s/projects" api_base team_id in
  get_json ~token url

(** 프로젝트의 파일 목록 *)
let get_project_files ~token ~project_id : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/projects/%s/files" api_base project_id in
  get_json ~token url

(** 파일의 로컬 변수 (Design Tokens) *)
let get_local_variables ~token ~file_key : (Yojson.Safe.t, api_error) result Lwt.t =
  let url = sprintf "%s/files/%s/variables/local" api_base file_key in
  get_json ~token url

(** ============== URL 파싱 ============== *)

type figma_url_info = {
  team_id: string option;
  project_id: string option;
  file_key: string option;
  node_id: string option;
}

(** Figma URL 파싱
    지원 패턴:
    - https://www.figma.com/files/team/TEAM_ID/project/PROJECT_ID
    - https://www.figma.com/file/FILE_KEY/...
    - https://www.figma.com/design/FILE_KEY/...?node-id=NODE_ID
*)
let parse_figma_url url =
  let empty = { team_id = None; project_id = None; file_key = None; node_id = None } in
  try
    let uri = Uri.of_string url in
    let path = Uri.path uri in
    let segments = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in

    (* node-id 쿼리 파라미터 추출 *)
    let node_id = Uri.get_query_param uri "node-id" in

    (* 경로 패턴 매칭 *)
    match segments with
    (* /files/team/TEAM_ID/project/PROJECT_ID *)
    | "files" :: "team" :: team_id :: "project" :: project_id :: _ ->
        { empty with team_id = Some team_id; project_id = Some project_id }
    (* /files/team/TEAM_ID *)
    | "files" :: "team" :: team_id :: _ ->
        { empty with team_id = Some team_id }
    (* /file/FILE_KEY/... 또는 /design/FILE_KEY/... *)
    | ("file" | "design") :: file_key :: _ ->
        { empty with file_key = Some file_key; node_id }
    (* /proto/FILE_KEY/... *)
    | "proto" :: file_key :: _ ->
        { empty with file_key = Some file_key; node_id }
    | _ -> empty
  with _ -> empty

(** ============== 응답 파싱 헬퍼 ============== *)

let member key json =
  match json with
  | `Assoc lst -> List.assoc_opt key lst
  | _ -> None

(** document 노드 추출 *)
let extract_document json =
  member "document" json

(** 페이지(Canvas) 목록 추출 *)
let extract_pages json =
  match extract_document json with
  | Some doc ->
      (match member "children" doc with
       | Some (`List pages) -> pages
       | _ -> [])
  | None -> []

(** 페이지 이름 목록 *)
let get_page_names json =
  let pages = extract_pages json in
  List.filter_map (fun page ->
    match member "name" page with
    | Some (`String name) -> Some name
    | _ -> None
  ) pages

(** 특정 페이지의 프레임들 추출 *)
let get_frames_from_page page =
  let get_str key json =
    match member key json with Some (`String s) -> Some s | _ -> None
  in
  match member "children" page with
  | Some (`List frames) ->
      List.filter_map (fun frame ->
        let id = get_str "id" frame in
        let name = get_str "name" frame in
        let type_ = get_str "type" frame in
        match (id, name, type_) with
        | (Some id, Some name, Some "FRAME") -> Some (id, name)
        | (Some id, Some name, Some "COMPONENT") -> Some (id, name)
        | _ -> None
      ) frames
  | _ -> []

(** 모든 화면(Frame) 목록 *)
let get_all_screens json =
  let pages = extract_pages json in
  List.concat_map get_frames_from_page pages

(** ============== 동기 래퍼 (MCP 핸들러용) ============== *)

let get_file_sync ~token ~file_key =
  Lwt_main.run (get_file ~token ~file_key ())

let get_file_nodes_sync ~token ~file_key ~node_ids =
  Lwt_main.run (get_file_nodes ~token ~file_key ~node_ids ())

let get_images_sync ~token ~file_key ~node_ids ~format ~scale =
  Lwt_main.run (get_images ~token ~file_key ~node_ids ~format ~scale ())

let get_me_sync ~token =
  Lwt_main.run (get_me ~token)

let get_team_projects_sync ~token ~team_id =
  Lwt_main.run (get_team_projects ~token ~team_id)

let get_project_files_sync ~token ~project_id =
  Lwt_main.run (get_project_files ~token ~project_id)

let get_local_variables_sync ~token ~file_key =
  Lwt_main.run (get_local_variables ~token ~file_key)
