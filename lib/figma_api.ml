(** Figma API utilities (Pure, no Lwt) *)

open Printf

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

(** ============== JSON 파싱 헬퍼 ============== *)

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
