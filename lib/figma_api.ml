(** Figma REST API 클라이언트 (Lwt + Cohttp) *)

open Lwt.Syntax
open Printf

(** ============== API 설정 ============== *)

let api_base = "https://api.figma.com/v1"

(** 재시도 설정 (429 대응) *)
let () = Random.self_init ()

let max_retries =
  try int_of_string (Sys.getenv "FIGMA_API_MAX_RETRIES")
  with Not_found -> 3

let base_backoff_ms =
  try int_of_string (Sys.getenv "FIGMA_API_BACKOFF_MS")
  with Not_found -> 500

let max_backoff_ms =
  try int_of_string (Sys.getenv "FIGMA_API_BACKOFF_MAX_MS")
  with Not_found -> 4000

(** ============== Node ID 변환 ============== *)

(** URL 형식 (2089-11127) → API 형식 (2089:11127)
    Figma URL에서 추출한 node-id를 API 호출용으로 변환 *)
let url_to_api_node_id id =
  Str.global_replace (Str.regexp "-") ":" id

(** API 형식 (2089:11127) → URL 형식 (2089-11127)
    API 응답의 node id를 URL용으로 변환 *)
let api_to_url_node_id id =
  Str.global_replace (Str.regexp ":") "-" id

(** Normalize node ID input. Convert hyphen form when colon is missing. *)
let normalize_node_id id =
  if String.contains id '-' && not (String.contains id ':') then
    url_to_api_node_id id
  else
    id

let normalize_node_ids ids =
  List.map normalize_node_id ids

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

let host_header uri =
  match Uri.host uri with
  | None -> None
  | Some host ->
      let host =
        match Uri.port uri with
        | None -> host
        | Some port when port = 80 || port = 443 -> host
        | Some port -> sprintf "%s:%d" host port
      in
      Some host

let make_headers ?host token =
  let token = String.trim token in
  let headers = [
    ("X-Figma-Token", token);
    ("Accept", "application/json");
    ("User-Agent", "figma-mcp/0.3.2");
  ] in
  let headers =
    match host with
    | None -> headers
    | Some host -> ("Host", host) :: headers
  in
  Cohttp.Header.of_list headers

let strip_trailing_cr line =
  let len = String.length line in
  if len > 0 && line.[len - 1] = '\r' then
    String.sub line 0 (len - 1)
  else
    line

let header_present name headers =
  let name = String.lowercase_ascii name in
  List.exists (fun (key, _) -> String.lowercase_ascii key = name) headers

let resolve_ipv4 host port =
  let hints = [
    Unix.AI_FAMILY Unix.PF_INET;
    Unix.AI_SOCKTYPE Unix.SOCK_STREAM;
  ] in
  let* infos = Lwt_unix.getaddrinfo host (string_of_int port) hints in
  match infos with
  | { Unix.ai_addr = Unix.ADDR_INET (inet, _); _ } :: _ ->
      let ip = Ipaddr_unix.of_inet_addr inet in
      Lwt.return (Unix.ADDR_INET (inet, port), ip)
  | _ ->
      Lwt.fail_with (sprintf "Failed to resolve host: %s" host)

let connect_plain host port =
  let* (sa, _) = resolve_ipv4 host port in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect fd sa in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.return (ic, oc)

let connect_tls host port =
  let* (sa, ip) = resolve_ipv4 host port in
  let* (_fd, ic, oc) = Conduit_lwt_unix_ssl.Client.connect ~hostname:host ~ip sa in
  Lwt.return (ic, oc)

let parse_status_line line =
  match String.split_on_char ' ' line with
  | _proto :: code :: _ -> (match int_of_string_opt code with Some v -> v | None -> 0)
  | _ -> 0

let rec read_headers ic acc =
  let* line = Lwt_io.read_line ic in
  let line = strip_trailing_cr line in
  if line = "" then
    Lwt.return (List.rev acc)
  else
    match String.split_on_char ':' line with
    | [] -> read_headers ic acc
    | key :: rest ->
        let value = String.concat ":" rest |> String.trim in
        read_headers ic ((String.lowercase_ascii key, value) :: acc)

let read_exact ic len =
  if len <= 0 then
    Lwt.return ""
  else
    let buf = Bytes.create len in
    let rec loop off =
      if off = len then
        Lwt.return (Bytes.to_string buf)
      else
        let* read = Lwt_io.read_into ic buf off (len - off) in
        if read = 0 then
          Lwt.fail_with "Unexpected EOF while reading body"
        else
          loop (off + read)
    in
    loop 0

let read_chunked ic =
  let rec read_chunks acc =
    let* line = Lwt_io.read_line ic in
    let line = strip_trailing_cr line |> String.trim in
    let size_hex =
      match String.split_on_char ';' line with
      | head :: _ -> head
      | [] -> ""
    in
    match int_of_string_opt ("0x" ^ size_hex) with
    | None -> Lwt.fail_with "Invalid chunk size"
    | Some 0 ->
        let rec consume_trailers () =
          let* trailer = Lwt_io.read_line ic in
          let trailer = strip_trailing_cr trailer in
          if trailer = "" then Lwt.return_unit else consume_trailers ()
        in
        let* () = consume_trailers () in
        Lwt.return (String.concat "" (List.rev acc))
    | Some size ->
        let* chunk = read_exact ic size in
        let* _ = read_exact ic 2 in
        read_chunks (chunk :: acc)
  in
  read_chunks []

let read_body ic headers =
  let has_chunked value =
    value
    |> String.lowercase_ascii
    |> String.split_on_char ','
    |> List.exists (fun part -> String.trim part = "chunked")
  in
  match Cohttp.Header.get headers "transfer-encoding" with
  | Some value when has_chunked value -> read_chunked ic
  | _ ->
      (match Cohttp.Header.get headers "content-length" with
       | Some value ->
           (match int_of_string_opt value with
            | Some len -> read_exact ic len
            | None -> Lwt_io.read ic)
       | None -> Lwt_io.read ic)

(* Raw HTTP: avoid Content-Length: 0 on GET (api.figma.com returns 400). *)
let raw_request ~meth ~url ~headers ~body =
  let uri = Uri.of_string url in
  let scheme = Option.value (Uri.scheme uri) ~default:"https" in
  let host =
    match Uri.host uri with
    | Some h -> h
    | None -> raise (Invalid_argument "Missing host in URL")
  in
  let port =
    match Uri.port uri with
    | Some p -> p
    | None -> if scheme = "https" then 443 else 80
  in
  let path =
    match Uri.path_and_query uri with
    | "" -> "/"
    | value -> value
  in
  let body = Option.value body ~default:"" in
  let headers =
    let headers =
      if header_present "Host" headers then headers else ("Host", host) :: headers
    in
    let headers =
      if header_present "Connection" headers then headers else ("Connection", "close") :: headers
    in
    let headers =
      if body <> "" && not (header_present "Content-Length" headers) then
        ("Content-Length", string_of_int (String.length body)) :: headers
      else
        headers
    in
    headers
  in
  let request_line = sprintf "%s %s HTTP/1.1" meth path in
  let header_lines = List.map (fun (k, v) -> sprintf "%s: %s" k v) headers in
  let request_str = String.concat "\r\n" (request_line :: header_lines @ [""; body]) in
  let connect =
    match scheme with
    | "https" -> connect_tls
    | "http" -> connect_plain
    | _ -> raise (Invalid_argument "Unsupported URL scheme")
  in
  let* (ic, oc) = connect host port in
  Lwt.finalize
    (fun () ->
       let* () = Lwt_io.write oc request_str in
       let* () = Lwt_io.flush oc in
       let* status_line = Lwt_io.read_line ic in
       let status_line = strip_trailing_cr status_line in
       let code = parse_status_line status_line in
       let* header_list = read_headers ic [] in
       let headers = Cohttp.Header.of_list header_list in
       let* body_str = read_body ic headers in
       Lwt.return (code, headers, body_str))
    (fun () ->
       let close ch = Lwt.catch (fun () -> Lwt_io.close ch) (fun _ -> Lwt.return_unit) in
       let* () = close ic in
       close oc)

let retry_after_seconds headers =
  match Cohttp.Header.get headers "retry-after" with
  | None -> None
  | Some value ->
      (match int_of_string_opt value with
       | Some seconds when seconds >= 0 -> Some (float_of_int seconds)
       | _ -> None)

let backoff_seconds attempt headers =
  match retry_after_seconds headers with
  | Some seconds -> seconds
  | None ->
      let multiplier = 2. ** float_of_int attempt in
      let raw = float_of_int base_backoff_ms *. multiplier in
      let capped = min raw (float_of_int max_backoff_ms) in
      let jitter = Random.float (capped *. 0.25) in
      (capped +. jitter) /. 1000.0

let request_json ~url ~request : (Yojson.Safe.t, api_error) result Lwt.t =
  let rec loop attempt =
    let* (code, headers, body_str) = request () in
    if Sys.getenv_opt "FIGMA_API_DEBUG" = Some "1" then begin
      let server =
        Cohttp.Header.get headers "server" |> Option.value ~default:""
      in
      let snippet =
        if String.length body_str > 120 then String.sub body_str 0 120 ^ "..."
        else body_str
      in
      eprintf "[Figma API] status=%d server=%s body=%s\n%!" code server snippet
    end;
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
        if attempt < max_retries then begin
          let delay = backoff_seconds attempt headers in
          let* () = Lwt_unix.sleep delay in
          loop (attempt + 1)
        end else
          Lwt.return_error RateLimited
    | _ ->
        Lwt.return_error (UnknownError (code, body_str))
  in
  loop 0

let get_json ~token url : (Yojson.Safe.t, api_error) result Lwt.t =
  let uri = Uri.of_string url in
  let trimmed = String.trim token in
  let headers = make_headers ?host:(host_header uri) token in
  if Sys.getenv_opt "FIGMA_API_DEBUG" = Some "1" then begin
    let host = Cohttp.Header.get headers "Host" |> Option.value ~default:"<none>" in
    let header_keys =
      Cohttp.Header.to_list headers
      |> List.map fst
      |> String.concat ","
    in
    eprintf "[Figma API] GET %s token_len=%d host=%s headers=%s\n%!"
      url (String.length trimmed) host header_keys
  end;
  let raw_headers = Cohttp.Header.to_list headers in
  Lwt.catch
    (fun () ->
       request_json ~url ~request:(fun () ->
         raw_request ~meth:"GET" ~url ~headers:raw_headers ~body:None))
    (fun exn ->
       Lwt.return_error (NetworkError (Printexc.to_string exn)))

let post_json ~token url body_json : (Yojson.Safe.t, api_error) result Lwt.t =
  let uri = Uri.of_string url in
  let trimmed = String.trim token in
  if Sys.getenv_opt "FIGMA_API_DEBUG" = Some "1" then
    eprintf "[Figma API] POST %s token_len=%d\n%!" url (String.length trimmed);
  let headers =
    make_headers ?host:(host_header uri) token
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  in
  let body_str = Yojson.Safe.to_string body_json in
  let body = Cohttp_lwt.Body.of_string body_str in

  Lwt.catch
    (fun () ->
       request_json ~url ~request:(fun () ->
         let* (resp, body) = Cohttp_lwt_unix.Client.post ~headers ~body uri in
         let* body_str = Cohttp_lwt.Body.to_string body in
         let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
         let headers = Cohttp.Response.headers resp in
         Lwt.return (code, headers, body_str)))
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
  Lwt.catch
    (fun () ->
       let headers = [
         ("Accept", "*/*");
         ("User-Agent", "figma-mcp/0.3.2");
       ] in
       let* (code, _headers, body_str) =
         raw_request ~meth:"GET" ~url ~headers ~body:None
       in
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
  get_json ~token url

(** 이미지 내보내기 URL 가져오기 *)
let get_images ?use_absolute_bounds ?version ~token ~file_key ~node_ids ~format ~scale ()
  : (Yojson.Safe.t, api_error) result Lwt.t =
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
