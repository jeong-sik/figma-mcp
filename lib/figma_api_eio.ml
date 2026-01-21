(** Figma API 클라이언트 - Eio 네이티브 버전

    Lwt 의존성 없이 순수 Eio 기반 HTTP 클라이언트.
    cohttp-eio + tls-eio 사용.
*)

open Printf

(** ============== Types ============== *)

(** 타임아웃 예외 - 내부에서만 사용 *)
exception Request_timeout

type api_error =
  | Http_error of int * string   (** HTTP 상태 코드 + 메시지 *)
  | Json_error of string         (** JSON 파싱 에러 *)
  | Network_error of string      (** 네트워크 에러 *)
  | Timeout_error                (** 타임아웃 *)

let api_error_to_string = function
  | Http_error (code, msg) -> sprintf "HTTP %d: %s" code msg
  | Json_error msg -> sprintf "JSON error: %s" msg
  | Network_error msg -> sprintf "Network error: %s" msg
  | Timeout_error -> "Request timeout"

(** ============== Configuration ============== *)

let api_base = "https://api.figma.com/v1"
let default_timeout = 30.0  (* seconds *)
let max_body_size = 10 * 1024 * 1024  (* 10MB *)

(** ============== Logging ============== *)

(** 에러 로깅 - 침묵하지 않고 stderr로 출력 *)
let log_error context msg =
  Printf.eprintf "[figma_api_eio] %s: %s\n%!" context msg

let log_warning context msg =
  Printf.eprintf "[figma_api_eio] WARN %s: %s\n%!" context msg

(** ============== URL Utilities ============== *)

(** 노드 ID 정규화: "1-2" -> "1:2" *)
let normalize_node_id id =
  String.map (fun c -> if c = '-' then ':' else c) id

(** 복수 노드 ID 정규화 *)
let normalize_node_ids ids =
  List.map normalize_node_id ids

(** 쿼리 파라미터 추가 헬퍼 *)
let add_param name value params =
  match value with
  | Some v -> (name, [v]) :: params
  | None -> params

(** URL에 쿼리 파라미터 추가 *)
let with_query base_url params =
  match params with
  | [] -> base_url
  | _ ->
    let uri = Uri.of_string base_url in
    let uri = Uri.add_query_params uri params in
    Uri.to_string uri

(** ============== Eio HTTP Client ============== *)

(** HTTP 클라이언트 타입 *)
type client = Cohttp_eio.Client.t

(** TLS 설정 생성 - cohttp-eio가 TCP 연결 관리, 우리는 TLS 래핑만 *)
let make_https_handler () =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok auth -> auth
    | Error (`Msg msg) ->
      failwith (sprintf "Failed to load CA certificates: %s" msg)
  in
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Ok config -> config
    | Error (`Msg msg) -> failwith (sprintf "TLS config error: %s" msg)
  in
  (* 시그니처: Uri.t -> flow -> tls_wrapped_flow *)
  Some (fun _uri flow ->
    Tls_eio.client_of_flow tls_config flow
  )

(** HTTP 클라이언트 생성 *)
let make_client (net : _ Eio.Net.t) : client =
  let https = make_https_handler () in
  Cohttp_eio.Client.make ~https net

(** GET 요청 실행 (내부용) *)
let http_get_raw ~sw ~client ~headers url =
  let uri = Uri.of_string url in
  let headers = Http.Header.of_list headers in
  let resp, body = Cohttp_eio.Client.get client ~sw ~headers uri in
  let status = Http.Response.status resp in
  let body_str = Eio.Buf_read.(of_flow ~max_size:max_body_size body |> take_all) in
  (Http.Status.to_int status, body_str)

(** POST 요청 실행 (내부용) *)
let http_post_raw ~sw ~client ~headers url body_str =
  let uri = Uri.of_string url in
  let headers = Http.Header.of_list headers in
  let body = Cohttp_eio.Body.of_string body_str in
  let resp, body = Cohttp_eio.Client.post client ~sw ~headers ~body uri in
  let status = Http.Response.status resp in
  let body_str = Eio.Buf_read.(of_flow ~max_size:max_body_size body |> take_all) in
  (Http.Status.to_int status, body_str)

(** GET 요청 with timeout *)
let http_get ~sw ~clock ~client ~headers ?(timeout=default_timeout) url =
  match Eio.Time.with_timeout clock timeout (fun () ->
    Ok (http_get_raw ~sw ~client ~headers url)
  ) with
  | Ok result -> result
  | Error `Timeout ->
    log_error "http_get" (sprintf "Timeout after %.1fs: %s" timeout url);
    raise Request_timeout

(** POST 요청 with timeout *)
let http_post ~sw ~clock ~client ~headers ?(timeout=default_timeout) url body_str =
  match Eio.Time.with_timeout clock timeout (fun () ->
    Ok (http_post_raw ~sw ~client ~headers url body_str)
  ) with
  | Ok result -> result
  | Error `Timeout ->
    log_error "http_post" (sprintf "Timeout after %.1fs: %s" timeout url);
    raise Request_timeout

(** ============== Figma API Core ============== *)

(** Figma API GET 요청 - 에러 로깅 포함 *)
let get_json ~sw ~clock ~client ~token url : (Yojson.Safe.t, api_error) result =
  let headers = [
    ("X-Figma-Token", token);
    ("Accept", "application/json");
  ] in
  try
    let status, body = http_get ~sw ~clock ~client ~headers url in
    if status >= 200 && status < 300 then
      try Ok (Yojson.Safe.from_string body)
      with Yojson.Json_error msg ->
        log_error "get_json" (sprintf "JSON parse error: %s (url: %s)" msg url);
        Error (Json_error msg)
    else begin
      log_error "get_json" (sprintf "HTTP %d: %s (url: %s)" status (String.sub body 0 (min 200 (String.length body))) url);
      Error (Http_error (status, body))
    end
  with
  | Request_timeout ->
    log_error "get_json" (sprintf "Timeout (url: %s)" url);
    Error Timeout_error
  | exn ->
    let msg = Printexc.to_string exn in
    log_error "get_json" (sprintf "Network error: %s (url: %s)" msg url);
    Error (Network_error msg)

(** Figma API POST 요청 - 에러 로깅 포함 *)
let post_json ~sw ~clock ~client ~token url body_json : (Yojson.Safe.t, api_error) result =
  let headers = [
    ("X-Figma-Token", token);
    ("Content-Type", "application/json");
    ("Accept", "application/json");
  ] in
  let body_str = Yojson.Safe.to_string body_json in
  try
    let status, body = http_post ~sw ~clock ~client ~headers url body_str in
    if status >= 200 && status < 300 then
      try Ok (Yojson.Safe.from_string body)
      with Yojson.Json_error msg ->
        log_error "post_json" (sprintf "JSON parse error: %s (url: %s)" msg url);
        Error (Json_error msg)
    else begin
      log_error "post_json" (sprintf "HTTP %d: %s (url: %s)" status (String.sub body 0 (min 200 (String.length body))) url);
      Error (Http_error (status, body))
    end
  with
  | Request_timeout ->
    log_error "post_json" (sprintf "Timeout (url: %s)" url);
    Error Timeout_error
  | exn ->
    let msg = Printexc.to_string exn in
    log_error "post_json" (sprintf "Network error: %s (url: %s)" msg url);
    Error (Network_error msg)

(** 파일 다운로드 - 에러 로깅 포함 *)
let download_url ~sw ~clock ~client ~url ~path : (unit, api_error) result =
  let headers = [] in
  try
    let status, body = http_get ~sw ~clock ~client ~headers url in
    if status >= 200 && status < 300 then begin
      (* 디렉토리 생성 *)
      let dir = Filename.dirname path in
      if not (Sys.file_exists dir) then
        Sys.mkdir dir 0o755;
      (* 파일 쓰기 *)
      let oc = open_out_bin path in
      output_string oc body;
      close_out oc;
      Ok ()
    end else begin
      log_error "download_url" (sprintf "HTTP %d (url: %s, path: %s)" status url path);
      Error (Http_error (status, body))
    end
  with
  | Request_timeout ->
    log_error "download_url" (sprintf "Timeout (url: %s)" url);
    Error Timeout_error
  | exn ->
    let msg = Printexc.to_string exn in
    log_error "download_url" (sprintf "Failed: %s (url: %s, path: %s)" msg url path);
    Error (Network_error msg)

(** ============== JSON Utilities ============== *)

(** JSON에서 문자열 추출 *)
let json_string = function
  | `String s -> Some s
  | _ -> None

(** JSON에서 정수 추출 *)
let json_int = function
  | `Int i -> Some i
  | `Float f -> Some (int_of_float f)
  | _ -> None

(** JSON 객체에서 필드 추출 *)
let json_field key json =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

(** Figma_api 호환 별칭 *)
let member = json_field

(** document 노드 추출 *)
let extract_document json =
  json_field "document" json

(** ============== Page/Frame Extraction ============== *)

(** 페이지 목록 추출 *)
let extract_pages json =
  match json_field "document" json with
  | Some (`Assoc doc) ->
    (match List.assoc_opt "children" doc with
     | Some (`List pages) -> pages
     | _ -> [])
  | _ -> []

(** 프레임 추출 *)
let get_frames_from_page page =
  match page with
  | `Assoc fields ->
    (match List.assoc_opt "children" fields with
     | Some (`List frames) ->
       List.filter_map (fun frame ->
         match frame with
         | `Assoc f ->
           (match List.assoc_opt "type" f with
            | Some (`String ("FRAME" | "COMPONENT" | "COMPONENT_SET")) ->
              let id = Option.bind (List.assoc_opt "id" f) json_string in
              let name = Option.bind (List.assoc_opt "name" f) json_string in
              (match id, name with
               | Some id, Some name -> Some (id, name)
               | _ -> None)
            | _ -> None)
         | _ -> None
       ) frames
     | _ -> [])
  | _ -> []

(** 모든 화면(Frame) 목록 *)
let get_all_screens json =
  let pages = extract_pages json in
  List.concat_map get_frames_from_page pages

(** ============== Figma API Endpoints ============== *)

(** 파일 전체 가져오기 *)
let get_file ~clock ?depth ?geometry ?plugin_data ?version ~sw ~client ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result =
  let params =
    []
    |> add_param "version" version
    |> add_param "depth" (Option.map string_of_int depth)
    |> add_param "geometry" geometry
    |> add_param "plugin_data" plugin_data
  in
  let url = with_query (sprintf "%s/files/%s" api_base file_key) params in
  get_json ~sw ~clock ~client ~token url

(** 파일 노드들만 가져오기 (특정 노드 ID들) *)
let get_file_nodes ~clock ?depth ?geometry ?plugin_data ?version ~sw ~client ~token ~file_key ~node_ids ()
  : (Yojson.Safe.t, api_error) result =
  let node_ids = normalize_node_ids node_ids in
  let ids_param = String.concat "," node_ids in
  let params =
    [("ids", [ids_param])]
    |> add_param "version" version
    |> add_param "depth" (Option.map string_of_int depth)
    |> add_param "geometry" geometry
    |> add_param "plugin_data" plugin_data
  in
  let url = with_query (sprintf "%s/files/%s/nodes" api_base file_key) params in
  get_json ~sw ~clock ~client ~token url

(** 이미지 내보내기 URL 가져오기 *)
let get_images ~clock ?use_absolute_bounds ?version ~sw ~client ~token ~file_key ~node_ids ~format ~scale ()
  : (Yojson.Safe.t, api_error) result =
  let node_ids = normalize_node_ids node_ids in
  let ids_param = String.concat "," node_ids in
  let params =
    [("ids", [ids_param]);
     ("format", [format]);
     ("scale", [string_of_int scale])]
    |> add_param "use_absolute_bounds" (Option.map string_of_bool use_absolute_bounds)
    |> add_param "version" version
  in
  let url = with_query (sprintf "%s/images/%s" api_base file_key) params in
  get_json ~sw ~clock ~client ~token url

(** 이미지 채움(image fills) 원본 URL 가져오기 *)
let get_file_images ~clock ?version ~sw ~client ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result =
  let params = [] |> add_param "version" version in
  let url = with_query (sprintf "%s/files/%s/images" api_base file_key) params in
  get_json ~sw ~clock ~client ~token url

(** 파일 메타데이터(components/styles/componentSets) 가져오기 *)
let get_file_meta ~clock ?version ~sw ~client ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result =
  let params = [] |> add_param "version" version in
  let url = with_query (sprintf "%s/files/%s/meta" api_base file_key) params in
  get_json ~sw ~clock ~client ~token url

(** 파일 컴포넌트 목록 *)
let get_file_components ~clock ~sw ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/components" api_base file_key in
  get_json ~sw ~clock ~client ~token url

(** 팀 컴포넌트 목록 *)
let get_team_components ~clock ~sw ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/components" api_base team_id in
  get_json ~sw ~clock ~client ~token url

(** 파일 컴포넌트 셋 목록 *)
let get_file_component_sets ~clock ~sw ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/component_sets" api_base file_key in
  get_json ~sw ~clock ~client ~token url

(** 팀 컴포넌트 셋 목록 *)
let get_team_component_sets ~clock ~sw ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/component_sets" api_base team_id in
  get_json ~sw ~clock ~client ~token url

(** 파일 스타일 목록 *)
let get_file_styles ~clock ~sw ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/styles" api_base file_key in
  get_json ~sw ~clock ~client ~token url

(** 팀 스타일 목록 *)
let get_team_styles ~clock ~sw ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/styles" api_base team_id in
  get_json ~sw ~clock ~client ~token url

(** 개별 컴포넌트 조회 *)
let get_component ~clock ~sw ~client ~token ~component_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/components/%s" api_base component_key in
  get_json ~sw ~clock ~client ~token url

(** 개별 컴포넌트 셋 조회 *)
let get_component_set ~clock ~sw ~client ~token ~component_set_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/component_sets/%s" api_base component_set_key in
  get_json ~sw ~clock ~client ~token url

(** 개별 스타일 조회 *)
let get_style ~clock ~sw ~client ~token ~style_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/styles/%s" api_base style_key in
  get_json ~sw ~clock ~client ~token url

(** 파일 버전 목록 *)
let get_file_versions ~clock ~sw ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/versions" api_base file_key in
  get_json ~sw ~clock ~client ~token url

(** 파일 코멘트 목록 *)
let get_file_comments ~clock ~sw ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/comments" api_base file_key in
  get_json ~sw ~clock ~client ~token url

(** 파일 코멘트 생성 *)
let post_file_comment ~clock ~sw ~client ~token ~file_key ~message ~client_meta
  : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/comments" api_base file_key in
  let body =
    `Assoc [
      ("message", `String message);
      ("client_meta", client_meta);
    ]
  in
  post_json ~sw ~clock ~client ~token url body

(** 현재 사용자 정보 *)
let get_me ~clock ~sw ~client ~token : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/me" api_base in
  get_json ~sw ~clock ~client ~token url

(** 팀의 프로젝트 목록 *)
let get_team_projects ~clock ~sw ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/projects" api_base team_id in
  get_json ~sw ~clock ~client ~token url

(** 프로젝트의 파일 목록 *)
let get_project_files ~clock ~sw ~client ~token ~project_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/projects/%s/files" api_base project_id in
  get_json ~sw ~clock ~client ~token url

(** 파일의 로컬 변수 (Design Tokens) *)
let get_local_variables ~clock ~sw ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/variables/local" api_base file_key in
  get_json ~sw ~clock ~client ~token url

(** ============== URL Parsing ============== *)

type figma_url_info = {
  team_id: string option;
  project_id: string option;
  file_key: string option;
  node_id: string option;
}

(** Figma URL 파싱 *)
let parse_figma_url url =
  let empty = { team_id = None; project_id = None; file_key = None; node_id = None } in
  try
    let uri = Uri.of_string url in
    let path = Uri.path uri in
    let segments = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in

    (* node-id 쿼리 파라미터 추출 *)
    let node_id =
      Uri.get_query_param uri "node-id" |> Option.map normalize_node_id
    in

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
