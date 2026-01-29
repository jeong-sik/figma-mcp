(** Figma API 클라이언트 - Eio 네이티브 버전

    Lwt 의존성 없이 순수 Eio 기반 HTTP 클라이언트.
    cohttp-eio + tls-eio 사용.
*)

open Printf

let string_contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  if len_sub = 0 then true else loop 0

(** ============== Types ============== *)

(** 타임아웃 예외 - 내부에서만 사용 *)
exception Request_timeout

type api_error =
  | Http_error of int * string * float option  (** HTTP 상태 코드 + 메시지 + Retry-After *)
  | Json_error of string         (** JSON 파싱 에러 *)
  | Network_error of string      (** 네트워크 에러 *)
  | Timeout_error                (** 타임아웃 *)

(** Smart Error Recovery: 에러 코드별 친화적 메시지와 해결 방법 *)
type error_recovery = {
  message: string;       (** 사용자 친화적 메시지 *)
  suggestion: string;    (** 해결 방법 안내 *)
  retryable: bool;       (** 재시도 가능 여부 *)
  retry_after: float;    (** 재시도 대기 시간 (초) *)
}

(** HTTP 상태 코드별 에러 복구 정보 *)
let get_http_error_recovery code body retry_after =
  match code with
  | 400 -> {
      message = "Invalid request";
      suggestion =
        (let body_lower = String.lowercase_ascii body in
         if string_contains body_lower "invalid" && string_contains body_lower "id" then
           "Invalid ID format. Node IDs should be like '123:456', file keys are alphanumeric strings."
         else if string_contains body_lower "missing" then
           "Missing required parameter. Check file_key, node_id, or other required fields."
         else if string_contains body_lower "node" then
           "Node-related error. Verify the node exists and ID format is correct (e.g., '123:456')."
         else
           "Invalid request parameters. Check file_key and node_id are correct.");
      retryable = false;
      retry_after = 0.0;
    }
  | 401 -> {
      message = "Auth error: Invalid or expired token";
      suggestion = "Verify FIGMA_TOKEN is set correctly. Get a new token from Figma Settings > Personal Access Tokens";
      retryable = false;
      retry_after = 0.0;
    }
  | 403 -> {
      message = "Access denied";
      suggestion =
        (let body_lower = String.lowercase_ascii body in
         if string_contains body_lower "file_variables:read" || string_contains body_lower "invalid scope" then
           "Token missing scope: file_variables:read. Create a token with this scope in Figma Settings > Personal Access Tokens."
         else
           "You don't have permission to access this file. Ask the owner to share it with you, or check if the endpoint requires OAuth (PAT not allowed).");
      retryable = false;
      retry_after = 0.0;
    }
  | 404 -> {
      message = "Not found";
      suggestion =
        (let body_lower = String.lowercase_ascii body in
         if string_contains body_lower "file" then
           "File not found. The file may have been deleted, moved, or you may not have access."
         else if string_contains body_lower "node" then
           "Node not found. The node may have been deleted or the ID is incorrect."
         else if string_contains body_lower "version" then
           "Version not found. The specified version may not exist."
         else
           "Resource not found. Verify file_key and node_id exist and are accessible.");
      retryable = false;
      retry_after = 0.0;
    }
  | 429 ->
      (* Rate limit - prefer Retry-After header, fallback to body *)
      let retry_sec =
        match retry_after with
        | Some s when s > 0.0 -> s
        | _ ->
            (try
               let json = Yojson.Safe.from_string body in
               match Yojson.Safe.Util.member "retry_after" json with
               | `Int n -> float_of_int n
               | `Float f -> f
               | _ -> 60.0
             with _ -> 60.0)
      in
      {
        message = "Rate limited";
        suggestion = sprintf "Too many requests. Waiting %.0f seconds before retry" retry_sec;
        retryable = true;
        retry_after = retry_sec;
      }
  | 500 | 502 | 503 | 504 -> {
      message = "Figma server error";
      suggestion = "Temporary issue with Figma servers. Retry in a few seconds";
      retryable = true;
      retry_after = 2.0;
    }
  | _ -> {
      message = sprintf "HTTP error %d" code;
      suggestion = "Unexpected error. Check Figma status at status.figma.com";
      retryable = false;
      retry_after = 0.0;
    }

(** 네트워크 에러 복구 정보 *)
let get_network_error_recovery msg =
  if String.sub msg 0 (min 3 (String.length msg)) = "DNS" then
    { message = "DNS resolution failed";
      suggestion = "Check your internet connection and try again";
      retryable = true;
      retry_after = 1.0; }
  else if String.sub msg 0 (min 7 (String.length msg)) = "connect" then
    { message = "Connection failed";
      suggestion = "Unable to reach Figma servers. Check your network";
      retryable = true;
      retry_after = 2.0; }
  else if String.sub msg 0 (min 4 (String.length msg)) = "Unix" then
    { message = "System error";
      suggestion = "Temporary system issue. Retrying...";
      retryable = true;
      retry_after = 0.5; }
  else
    { message = "Network error";
      suggestion = msg;
      retryable = true;
      retry_after = 1.0; }

(** 에러를 친화적 문자열로 변환 (복구 정보 포함) *)
let api_error_to_friendly_string = function
  | Http_error (code, body, retry_after) ->
      let recovery = get_http_error_recovery code body retry_after in
      let body_preview =
        if String.length body > 200 then String.sub body 0 200 ^ "..."
        else body
      in
      if body_preview = "" then
        sprintf "%s: %s" recovery.message recovery.suggestion
      else
        sprintf "%s: %s [API response: %s]" recovery.message recovery.suggestion body_preview
  | Json_error msg -> sprintf "Invalid response from Figma: %s" msg
  | Network_error msg ->
      let recovery = get_network_error_recovery msg in
      sprintf "%s: %s" recovery.message recovery.suggestion
  | Timeout_error -> "Request timed out. The file might be too large or the network is slow"

(** 에러를 기술적 문자열로 변환 (디버깅용) *)
let api_error_to_string = function
  | Http_error (code, msg, _) -> sprintf "HTTP %d: %s" code msg
  | Json_error msg -> sprintf "JSON error: %s" msg
  | Network_error msg -> sprintf "Network error: %s" msg
  | Timeout_error -> "Request timeout"

(** 에러가 재시도 가능한지 확인 *)
let is_retryable_error = function
  | Http_error (code, body, retry_after) -> (get_http_error_recovery code body retry_after).retryable
  | Network_error msg -> (get_network_error_recovery msg).retryable
  | Timeout_error -> true  (* 타임아웃은 재시도 가능 *)
  | Json_error _ -> false  (* JSON 파싱 에러는 재시도 불가 *)

(** 재시도 대기 시간 반환 *)
let get_retry_delay = function
  | Http_error (code, body, retry_after) -> (get_http_error_recovery code body retry_after).retry_after
  | Network_error msg -> (get_network_error_recovery msg).retry_after
  | Timeout_error -> 1.0
  | Json_error _ -> 0.0

(** ============== Configuration ============== *)

let api_base = "https://api.figma.com/v1"
let default_timeout = 30.0  (* seconds *)
let max_body_size = 100 * 1024 * 1024  (* 100MB - Figma files can be very large *)

let is_dns_failure exn =
  let msg = Printexc.to_string exn |> String.lowercase_ascii in
  string_contains msg "resolve" || string_contains msg "dns"

let is_html_response body =
  let trimmed = String.trim body in
  string_contains (String.lowercase_ascii trimmed) "<html"

let retry_after_of_headers headers =
  match Cohttp.Header.get headers "retry-after" with
  | None -> None
  | Some value ->
      let trimmed = String.trim value in
      (match int_of_string_opt trimmed with
       | Some secs -> Some (float_of_int secs)
       | None -> None)

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

(** ============== Eio HTTP Client (Direct TLS) ============== *)

(** Raw TLS connection (for DNS fallback path) *)
type raw_conn = {
  flow : [ `Close | `Flow | `R | `Shutdown | `Tls | `W ] Eio.Resource.t;
  buf : Eio.Buf_read.t;
  mutable last_used : float;
}

(** HTTP 클라이언트 타입 - Direct TLS + Cohttp 호환 *)
type client = {
  tls_config : Tls.Config.client;
  cohttp_client : Cohttp_eio.Client.t;  (** LLM provider 등 호환용 *)
  raw_pool : (string, raw_conn) Hashtbl.t;
  raw_pool_lock : Mutex.t;
}

(** TLS 설정 생성 *)
let make_tls_config () =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok auth -> auth
    | Error (`Msg msg) ->
      failwith (sprintf "Failed to load CA certificates: %s" msg)
  in
  match Tls.Config.client ~authenticator () with
  | Ok config -> config
  | Error (`Msg msg) -> failwith (sprintf "TLS config error: %s" msg)

(** Cohttp용 HTTPS 핸들러 *)
let make_cohttp_https_handler tls_config =
  Some (fun uri tcp_flow ->
    let host_str = Uri.host uri |> Option.value ~default:"" in
    let host_domain =
      Domain_name.host_exn (Domain_name.of_string_exn host_str)
    in
    Tls_eio.client_of_flow tls_config ~host:host_domain tcp_flow)

(** HTTP 클라이언트 생성 *)
let make_client (net : _ Eio.Net.t) : client =
  let tls_config = make_tls_config () in
  let https = make_cohttp_https_handler tls_config in
  let cohttp_client = Cohttp_eio.Client.make ~https net in
  {
    tls_config;
    cohttp_client;
    raw_pool = Hashtbl.create 4;
    raw_pool_lock = Mutex.create ();
  }

(** Cohttp 클라이언트 추출 (LLM provider 등 호환용) *)
let get_cohttp_client (client : client) : Cohttp_eio.Client.t =
  client.cohttp_client

(** Decode chunked transfer encoding *)
let decode_chunked body =
  let buf = Buffer.create (String.length body) in
  let rec decode pos =
    if pos >= String.length body then Buffer.contents buf
    else
      (* Find chunk size line (hex number followed by \r\n) *)
      let line_end =
        try String.index_from body pos '\r'
        with Not_found -> String.length body
      in
      let size_str = String.sub body pos (line_end - pos) |> String.trim in
      if size_str = "" || size_str = "0" then Buffer.contents buf
      else
        let chunk_size =
          try int_of_string ("0x" ^ size_str)
          with _ -> 0
        in
        if chunk_size = 0 then Buffer.contents buf
        else
          let data_start = line_end + 2 in  (* skip \r\n *)
          let data_end = min (data_start + chunk_size) (String.length body) in
          let chunk_data = String.sub body data_start (data_end - data_start) in
          Buffer.add_string buf chunk_data;
          (* Skip chunk data + trailing \r\n *)
          decode (data_end + 2)
  in
  decode 0

(** HTTP/1.1 응답 파싱 - 상태코드와 본문 추출 (chunked encoding 지원) *)
let parse_http_response response =
  (* 첫 줄: HTTP/1.1 200 OK *)
  let lines = String.split_on_char '\n' response in
  match lines with
  | [] -> (500, "Empty response")
  | status_line :: rest ->
    (* 상태 코드 추출 *)
    let status =
      try
        let parts = String.split_on_char ' ' status_line in
        match parts with
        | _ :: code :: _ -> int_of_string code
        | _ -> 500
      with _ -> 500
    in
    (* 헤더와 본문 분리 (빈 줄로 구분) + chunked 체크 *)
    let rec find_body_and_check_chunked headers_acc is_chunked = function
      | [] -> List.rev headers_acc, "", is_chunked
      | "" :: rest | "\r" :: rest ->
        (* 빈 줄 발견 - 나머지가 본문 *)
        List.rev headers_acc, String.concat "\n" rest, is_chunked
      | line :: rest ->
        let line_lower = String.lowercase_ascii line in
        let chunked = is_chunked || String.sub line_lower 0 (min 18 (String.length line_lower)) = "transfer-encoding:"
                                    && String.index_opt line_lower 'c' <> None in
        find_body_and_check_chunked (line :: headers_acc) chunked rest
    in
    let _headers, raw_body, is_chunked = find_body_and_check_chunked [] false rest in
    let body = if is_chunked then decode_chunked raw_body else raw_body in
    (status, body)

let with_raw_pool (client : client) f =
  Mutex.lock client.raw_pool_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock client.raw_pool_lock) f

let close_raw_conn conn =
  try Eio.Flow.close conn.flow with _ -> ()

let raw_pool_ttl = 30.0
let raw_pool_max = 4

let prune_raw_pool (client : client) =
  let now = Unix.gettimeofday () in
  let to_close = ref [] in
  with_raw_pool client (fun () ->
    let stale_keys = ref [] in
    Hashtbl.iter (fun key conn ->
      if now -. conn.last_used > raw_pool_ttl then
        stale_keys := key :: !stale_keys
    ) client.raw_pool;
    List.iter (fun key ->
      match Hashtbl.find_opt client.raw_pool key with
      | Some conn ->
          Hashtbl.remove client.raw_pool key;
          to_close := conn :: !to_close
      | None -> ()
    ) !stale_keys
  );
  List.iter close_raw_conn !to_close

let take_raw_conn (client : client) key =
  prune_raw_pool client;
  with_raw_pool client (fun () ->
    match Hashtbl.find_opt client.raw_pool key with
    | None -> None
    | Some conn ->
        Hashtbl.remove client.raw_pool key;
        Some conn)

let return_raw_conn (client : client) key conn =
  prune_raw_pool client;
  let stored =
    with_raw_pool client (fun () ->
      if Hashtbl.length client.raw_pool >= raw_pool_max then
        false
      else begin
        conn.last_used <- Unix.gettimeofday ();
        Hashtbl.replace client.raw_pool key conn;
        true
      end)
  in
  if not stored then close_raw_conn conn

let read_headers br =
  let rec loop acc =
    let line = Eio.Buf_read.line br |> String.trim in
    if line = "" then List.rev acc
    else
      match String.index_opt line ':' with
      | None -> loop acc
      | Some idx ->
          let key = String.sub line 0 idx |> String.trim |> String.lowercase_ascii in
          let value =
            String.sub line (idx + 1) (String.length line - idx - 1)
            |> String.trim
          in
          loop ((key, value) :: acc)
  in
  loop []

let header_value headers key =
  List.find_opt (fun (k, _) -> k = key) headers
  |> Option.map snd

let read_chunked_body br =
  let buf = Buffer.create 256 in
  let rec read_chunks () =
    let line = Eio.Buf_read.line br |> String.trim in
    let size_str =
      match String.split_on_char ';' line with
      | [] -> "0"
      | hd :: _ -> String.trim hd
    in
    let size =
      try int_of_string ("0x" ^ size_str) with _ -> 0
    in
    if size = 0 then begin
      let rec skip_trailers () =
        let tline = Eio.Buf_read.line br |> String.trim in
        if tline = "" then () else skip_trailers ()
      in
      skip_trailers ();
      Buffer.contents buf
    end else begin
      let chunk = Eio.Buf_read.take size br in
      Buffer.add_string buf chunk;
      ignore (Eio.Buf_read.take 2 br);
      read_chunks ()
    end
  in
  read_chunks ()

let read_http_response_from_flow br =
  let status_line = Eio.Buf_read.line br |> String.trim in
  let status =
    try
      let parts = String.split_on_char ' ' status_line in
      match parts with
      | _ :: code :: _ -> int_of_string code
      | _ -> 500
    with _ -> 500
  in
  let headers = read_headers br in
  let connection_close =
    match header_value headers "connection" with
    | Some value -> string_contains (String.lowercase_ascii value) "close"
    | None -> false
  in
  let is_chunked =
    match header_value headers "transfer-encoding" with
    | Some value -> string_contains (String.lowercase_ascii value) "chunked"
    | None -> false
  in
  let content_length =
    match header_value headers "content-length" with
    | Some value -> int_of_string_opt (String.trim value)
    | None -> None
  in
  let body =
    match content_length with
    | Some len -> Eio.Buf_read.take len br
    | None when is_chunked -> read_chunked_body br
    | None -> Eio.Buf_read.take_all br
  in
  let should_close = connection_close || (content_length = None && not is_chunked) in
  (status, headers, body, should_close)

(** DNS fallback via DoH (Cloudflare) - used only when system resolver fails *)
let ipaddr_of_string ip =
  try
    let ipaddr = Ipaddr.of_string_exn ip in
    let raw = Ipaddr.to_octets ipaddr in
    Some (Eio.Net.Ipaddr.of_raw raw)
  with _ -> None

let resolve_via_doh ~sw ~net ~tls_config hostname =
  match ipaddr_of_string "1.1.1.1" with
  | None -> None
  | Some ip ->
      let addr = `Tcp (ip, 443) in
      let tcp_flow = Eio.Net.connect ~sw net addr in
      let host_domain = Domain_name.(host_exn (of_string_exn "cloudflare-dns.com")) in
      let tls_flow = Tls_eio.client_of_flow tls_config ~host:host_domain tcp_flow in
      let path = sprintf "/dns-query?name=%s&type=A" hostname in
      let request =
        sprintf "GET %s HTTP/1.1\r\nHost: cloudflare-dns.com\r\naccept: application/dns-json\r\nconnection: close\r\n\r\n"
          path
      in
      Eio.Flow.copy_string request tls_flow;
      let response = Eio.Buf_read.(of_flow ~max_size:(128 * 1024) tls_flow |> take_all) in
      let status, body = parse_http_response response in
      if status <> 200 then None
      else
        (try
           let json = Yojson.Safe.from_string body in
           match Yojson.Safe.Util.member "Answer" json with
           | `List answers ->
               let ips =
                 List.filter_map (fun ans ->
                     match Yojson.Safe.Util.member "data" ans with
                     | `String ip -> Some ip
                     | _ -> None
                   ) answers
               in
               (match ips with
                | hd :: _ -> Some hd
                | [] -> None)
           | _ -> None
         with _ -> None)

let resolve_host_with_fallback ~sw ~net ~tls_config ~hostname ~port =
  match Eio.Net.getaddrinfo_stream net hostname with
  | addr :: _ -> Some addr
  | [] ->
      (match resolve_via_doh ~sw ~net ~tls_config hostname with
       | Some ip ->
           (match ipaddr_of_string ip with
            | Some ipaddr -> Some (`Tcp (ipaddr, port))
            | None -> None)
       | None -> None)
  | exception _ ->
      (match resolve_via_doh ~sw ~net ~tls_config hostname with
       | Some ip ->
           (match ipaddr_of_string ip with
            | Some ipaddr -> Some (`Tcp (ipaddr, port))
       | None -> None)
       | None -> None)

let open_raw_connection ~sw ~net ~client ~hostname ~port =
  let addr =
    match resolve_host_with_fallback ~sw ~net ~tls_config:client.tls_config ~hostname ~port with
    | Some addr ->
        (match addr with
         | `Tcp (ip, _) -> `Tcp (ip, port)
         | _ -> failwith "Expected TCP address")
    | None -> failwith (sprintf "DNS resolution failed for %s" hostname)
  in
  let tcp_flow = Eio.Net.connect ~sw net addr in
  let host_domain = Domain_name.(host_exn (of_string_exn hostname)) in
  let tls_flow = Tls_eio.client_of_flow client.tls_config ~host:host_domain tcp_flow in
  let buf = Eio.Buf_read.of_flow ~max_size:max_body_size tls_flow in
  { flow = tls_flow; buf; last_used = Unix.gettimeofday () }

(** Direct HTTPS GET 요청 - TCP 연결 + TLS + HTTP/1.1 *)
let https_get_raw ~sw ~net ~client ~headers url =
  let uri = Uri.of_string url in
  let hostname = Uri.host uri |> Option.value ~default:"" in
  let port = Uri.port uri |> Option.value ~default:443 in
  let path =
    let p = Uri.path uri in
    let q = Uri.query uri in
    if q = [] then (if p = "" then "/" else p)
    else sprintf "%s?%s" (if p = "" then "/" else p) (Uri.encoded_of_query q)
  in

  let key = sprintf "%s:%d" hostname port in
  let send_request conn =
    let header_lines = List.map (fun (k, v) -> sprintf "%s: %s" k v) headers in
    let request =
      sprintf "GET %s HTTP/1.1\r\nHost: %s\r\n%s\r\nConnection: keep-alive\r\n\r\n"
        path hostname (String.concat "\r\n" header_lines)
    in
    Eio.Flow.copy_string request conn.flow;
    let status, _resp_headers, body, should_close = read_http_response_from_flow conn.buf in
    conn.last_used <- Unix.gettimeofday ();
    (status, body, should_close)
  in
  let use_conn conn =
    try
      let status, body, should_close = send_request conn in
      Ok (conn, status, body, should_close)
    with exn ->
      close_raw_conn conn;
      Error exn
  in
  let conn, status, body, should_close =
    match take_raw_conn client key with
    | Some conn ->
        (match use_conn conn with
         | Ok res -> res
         | Error _ ->
             let fresh = open_raw_connection ~sw ~net ~client ~hostname ~port in
             (match use_conn fresh with
              | Ok res -> res
              | Error exn -> raise exn))
    | None ->
        let fresh = open_raw_connection ~sw ~net ~client ~hostname ~port in
        (match use_conn fresh with
         | Ok res -> res
         | Error exn -> raise exn)
  in
  if should_close then close_raw_conn conn else return_raw_conn client key conn;
  (status, body)

(** Direct HTTPS POST 요청 *)
let https_post_raw ~sw ~net ~client ~headers url body_content =
  let uri = Uri.of_string url in
  let hostname = Uri.host uri |> Option.value ~default:"" in
  let port = Uri.port uri |> Option.value ~default:443 in
  let path =
    let p = Uri.path uri in
    let q = Uri.query uri in
    if q = [] then (if p = "" then "/" else p)
    else sprintf "%s?%s" (if p = "" then "/" else p) (Uri.encoded_of_query q)
  in

  let key = sprintf "%s:%d" hostname port in
  let send_request conn =
    let header_lines = List.map (fun (k, v) -> sprintf "%s: %s" k v) headers in
    let request =
      sprintf "POST %s HTTP/1.1\r\nHost: %s\r\nContent-Length: %d\r\n%s\r\nConnection: keep-alive\r\n\r\n%s"
        path hostname (String.length body_content) (String.concat "\r\n" header_lines) body_content
    in
    Eio.Flow.copy_string request conn.flow;
    let status, _resp_headers, body, should_close = read_http_response_from_flow conn.buf in
    conn.last_used <- Unix.gettimeofday ();
    (status, body, should_close)
  in
  let use_conn conn =
    try
      let status, body, should_close = send_request conn in
      Ok (conn, status, body, should_close)
    with exn ->
      close_raw_conn conn;
      Error exn
  in
  let conn, status, body, should_close =
    match take_raw_conn client key with
    | Some conn ->
        (match use_conn conn with
         | Ok res -> res
         | Error _ ->
             let fresh = open_raw_connection ~sw ~net ~client ~hostname ~port in
             (match use_conn fresh with
              | Ok res -> res
              | Error exn -> raise exn))
    | None ->
        let fresh = open_raw_connection ~sw ~net ~client ~hostname ~port in
        (match use_conn fresh with
         | Ok res -> res
         | Error exn -> raise exn)
  in
  if should_close then close_raw_conn conn else return_raw_conn client key conn;
  (status, body)

(** Unix.EINVAL 에러 체크 (macOS select 버그 우회용) *)
let is_einval_error exn =
  match exn with
  | Unix.Unix_error (Unix.EINVAL, "select", _) -> true
  | _ -> false

(** GET 요청 with timeout + 재시도 (macOS select 버그 우회) *)
let http_get ~sw ~net ~clock ~client ~headers ?(timeout=default_timeout) ?(max_retries=3) url =
  let headers = Cohttp.Header.of_list headers in
  let headers_list = Cohttp.Header.to_list headers in
  let uri = Uri.of_string url in
  let rec retry n =
    try
      match Eio.Time.with_timeout clock timeout (fun () ->
        let resp, body = Cohttp_eio.Client.get client.cohttp_client ~sw ~headers uri in
        let body_str = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_body_size in
        Ok (resp, body_str)
      ) with
      | Ok result -> Ok result
      | Error `Timeout ->
        log_error "http_get" (sprintf "Timeout after %.1fs: %s" timeout url);
        Error `Timeout
    with exn when is_einval_error exn && n < max_retries ->
      log_warning "http_get" (sprintf "Unix.EINVAL retry %d/%d: %s" (n + 1) max_retries url);
      Eio.Time.sleep clock 0.1;  (* 100ms 대기 *)
      retry (n + 1)
    | exn -> Error (`Exn exn)
  in
  match retry 0 with
  | Ok (resp, body_str) ->
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let resp_headers = Cohttp.Response.headers resp in
      if status = 400 && is_html_response body_str then
        (try
           let status2, body2 = https_get_raw ~sw ~net ~client ~headers:headers_list url in
           if status2 >= 200 && status2 < 300 then
             (status2, Cohttp.Header.init (), body2)
           else
             (status, resp_headers, body_str)
         with _ ->
           (status, resp_headers, body_str))
      else
        (status, resp_headers, body_str)
  | Error `Timeout -> raise Request_timeout
  | Error (`Exn exn) when is_dns_failure exn ->
      let status, body_str = https_get_raw ~sw ~net ~client ~headers:headers_list url in
      (status, Cohttp.Header.init (), body_str)
  | Error (`Exn exn) -> raise exn

(** POST 요청 with timeout + 재시도 (macOS select 버그 우회) *)
let http_post ~sw ~net ~clock ~client ~headers ?(timeout=default_timeout) ?(max_retries=3) url body_str =
  let headers = Cohttp.Header.of_list headers in
  let headers_list = Cohttp.Header.to_list headers in
  let uri = Uri.of_string url in
  let body = Cohttp_eio.Body.of_string body_str in
  let rec retry n =
    try
      match Eio.Time.with_timeout clock timeout (fun () ->
        let resp, resp_body = Cohttp_eio.Client.post client.cohttp_client ~sw ~headers ~body uri in
        let resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_body_size in
        Ok (resp, resp_str)
      ) with
      | Ok result -> Ok result
      | Error `Timeout ->
        log_error "http_post" (sprintf "Timeout after %.1fs: %s" timeout url);
        Error `Timeout
    with exn when is_einval_error exn && n < max_retries ->
      log_warning "http_post" (sprintf "Unix.EINVAL retry %d/%d: %s" (n + 1) max_retries url);
      Eio.Time.sleep clock 0.1;  (* 100ms 대기 *)
      retry (n + 1)
    | exn -> Error (`Exn exn)
  in
  match retry 0 with
  | Ok (resp, resp_str) ->
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let resp_headers = Cohttp.Response.headers resp in
      (status, resp_headers, resp_str)
  | Error `Timeout -> raise Request_timeout
  | Error (`Exn exn) when is_dns_failure exn ->
      let status, body_str = https_post_raw ~sw ~net ~client ~headers:headers_list url body_str in
      (status, Cohttp.Header.init (), body_str)
  | Error (`Exn exn) -> raise exn

(** ============== Figma API Core ============== *)

(** Figma API GET 요청 - 에러 로깅 포함 *)
let get_json ~sw ~net ~clock ~client ~token url : (Yojson.Safe.t, api_error) result =
  let headers = [
    ("X-Figma-Token", token);
    ("Accept", "application/json");
  ] in
  try
    let status, resp_headers, body = http_get ~sw ~net ~clock ~client ~headers url in
    if status >= 200 && status < 300 then
      try Ok (Yojson.Safe.from_string body)
      with Yojson.Json_error msg ->
        log_error "get_json" (sprintf "JSON parse error: %s (url: %s)" msg url);
        Error (Json_error msg)
    else begin
      log_error "get_json" (sprintf "HTTP %d: %s (url: %s)" status (String.sub body 0 (min 200 (String.length body))) url);
      let retry_after = retry_after_of_headers resp_headers in
      Error (Http_error (status, body, retry_after))
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
let post_json ~sw ~net ~clock ~client ~token url body_json : (Yojson.Safe.t, api_error) result =
  let headers = [
    ("X-Figma-Token", token);
    ("Content-Type", "application/json");
    ("Accept", "application/json");
  ] in
  let body_str = Yojson.Safe.to_string body_json in
  try
    let status, resp_headers, body = http_post ~sw ~net ~clock ~client ~headers url body_str in
    if status >= 200 && status < 300 then
      try Ok (Yojson.Safe.from_string body)
      with Yojson.Json_error msg ->
        log_error "post_json" (sprintf "JSON parse error: %s (url: %s)" msg url);
        Error (Json_error msg)
    else begin
      log_error "post_json" (sprintf "HTTP %d: %s (url: %s)" status (String.sub body 0 (min 200 (String.length body))) url);
      let retry_after = retry_after_of_headers resp_headers in
      Error (Http_error (status, body, retry_after))
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
let download_url ~sw ~net ~clock ~client ~url ~path : (unit, api_error) result =
  let headers = [] in
  try
    let status, resp_headers, body = http_get ~sw ~net ~clock ~client ~headers url in
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
      let retry_after = retry_after_of_headers resp_headers in
      Error (Http_error (status, body, retry_after))
    end
  with
  | Request_timeout ->
    log_error "download_url" (sprintf "Timeout (url: %s)" url);
    Error Timeout_error
  | exn ->
    let msg = Printexc.to_string exn in
    log_error "download_url" (sprintf "Failed: %s (url: %s, path: %s)" msg url path);
    Error (Network_error msg)

(** ============== Smart Retry Wrapper ============== *)

(** 재시도 가능한 에러에 대해 자동 재시도하는 래퍼
    @param max_retries 최대 재시도 횟수 (기본 2)
    @param clock Eio clock for sleep
    @param f 실행할 함수 *)
let with_smart_retry ~clock ?(max_retries=2) f =
  let rec loop attempt =
    match f () with
    | Ok _ as result -> result
    | Error err when is_retryable_error err && attempt < max_retries ->
        let delay = get_retry_delay err in
        log_warning "smart_retry" (sprintf "Attempt %d failed, retrying in %.1fs: %s"
          (attempt + 1) delay (api_error_to_string err));
        Eio.Time.sleep clock delay;
        loop (attempt + 1)
    | Error err ->
        (* 재시도 불가하거나 최대 횟수 도달 *)
        if attempt > 0 then
          log_error "smart_retry" (sprintf "All %d attempts failed: %s"
            (attempt + 1) (api_error_to_friendly_string err));
        Error err
  in
  loop 0

(** GET 요청 with smart retry *)
let get_json_with_retry ~sw ~net ~clock ~client ~token ?(max_retries=2) url =
  with_smart_retry ~clock ~max_retries (fun () ->
    get_json ~sw ~net ~clock ~client ~token url
  )

(** POST 요청 with smart retry *)
let post_json_with_retry ~sw ~net ~clock ~client ~token ?(max_retries=2) url body_json =
  with_smart_retry ~clock ~max_retries (fun () ->
    post_json ~sw ~net ~clock ~client ~token url body_json
  )

(** 다운로드 with smart retry *)
let download_url_with_retry ~sw ~net ~clock ~client ~url ~path ?(max_retries=2) () =
  with_smart_retry ~clock ~max_retries (fun () ->
    download_url ~sw ~net ~clock ~client ~url ~path
  )

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
let get_file ~clock ?depth ?geometry ?plugin_data ?version ~sw ~net ~client ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result =
  let params =
    []
    |> add_param "version" version
    |> add_param "depth" (Option.map string_of_int depth)
    |> add_param "geometry" geometry
    |> add_param "plugin_data" plugin_data
  in
  let url = with_query (sprintf "%s/files/%s" api_base file_key) params in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 노드들만 가져오기 (특정 노드 ID들) *)
let get_file_nodes ~clock ?depth ?geometry ?plugin_data ?version ~sw ~net ~client ~token ~file_key ~node_ids ()
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
  get_json ~sw ~net ~clock ~client ~token url

(** 이미지 내보내기 URL 가져오기 - scale은 0.01-4.0 범위의 float *)
let get_images ~clock ?use_absolute_bounds ?version ~sw ~net ~client ~token ~file_key ~node_ids ~format ~scale ()
  : (Yojson.Safe.t, api_error) result =
  let node_ids = normalize_node_ids node_ids in
  let ids_param = String.concat "," node_ids in
  (* Clamp scale to Figma API limits: 0.01 - 4.0 *)
  let clamped_scale = max 0.01 (min 4.0 scale) in
  let params =
    [("ids", [ids_param]);
     ("format", [format]);
     ("scale", [Printf.sprintf "%.2f" clamped_scale])]
    |> add_param "use_absolute_bounds" (Option.map string_of_bool use_absolute_bounds)
    |> add_param "version" version
  in
  let url = with_query (sprintf "%s/images/%s" api_base file_key) params in
  get_json ~sw ~net ~clock ~client ~token url

(** 이미지 채움(image fills) 원본 URL 가져오기 *)
let get_file_images ~clock ?version ~sw ~net ~client ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result =
  let params = [] |> add_param "version" version in
  let url = with_query (sprintf "%s/files/%s/images" api_base file_key) params in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 메타데이터(components/styles/componentSets) 가져오기 *)
let get_file_meta ~clock ?version ~sw ~net ~client ~token ~file_key ()
  : (Yojson.Safe.t, api_error) result =
  let params = [] |> add_param "version" version in
  let url = with_query (sprintf "%s/files/%s/meta" api_base file_key) params in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 컴포넌트 목록 *)
let get_file_components ~clock ~sw ~net ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/components" api_base file_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 팀 컴포넌트 목록 *)
let get_team_components ~clock ~sw ~net ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/components" api_base team_id in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 컴포넌트 셋 목록 *)
let get_file_component_sets ~clock ~sw ~net ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/component_sets" api_base file_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 팀 컴포넌트 셋 목록 *)
let get_team_component_sets ~clock ~sw ~net ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/component_sets" api_base team_id in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 스타일 목록 *)
let get_file_styles ~clock ~sw ~net ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/styles" api_base file_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 팀 스타일 목록 *)
let get_team_styles ~clock ~sw ~net ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/styles" api_base team_id in
  get_json ~sw ~net ~clock ~client ~token url

(** 개별 컴포넌트 조회 *)
let get_component ~clock ~sw ~net ~client ~token ~component_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/components/%s" api_base component_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 개별 컴포넌트 셋 조회 *)
let get_component_set ~clock ~sw ~net ~client ~token ~component_set_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/component_sets/%s" api_base component_set_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 개별 스타일 조회 *)
let get_style ~clock ~sw ~net ~client ~token ~style_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/styles/%s" api_base style_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 버전 목록 *)
let get_file_versions ~clock ~sw ~net ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/versions" api_base file_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 코멘트 목록 *)
let get_file_comments ~clock ~sw ~net ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/comments" api_base file_key in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일 코멘트 생성 *)
let post_file_comment ~clock ~sw ~net ~client ~token ~file_key ~message ~client_meta
  : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/comments" api_base file_key in
  let body =
    `Assoc [
      ("message", `String message);
      ("client_meta", client_meta);
    ]
  in
  post_json ~sw ~net ~clock ~client ~token url body

(** 현재 사용자 정보 *)
let get_me ~clock ~sw ~net ~client ~token : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/me" api_base in
  get_json ~sw ~net ~clock ~client ~token url

(** 팀의 프로젝트 목록 *)
let get_team_projects ~clock ~sw ~net ~client ~token ~team_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/teams/%s/projects" api_base team_id in
  get_json ~sw ~net ~clock ~client ~token url

(** 프로젝트의 파일 목록 *)
let get_project_files ~clock ~sw ~net ~client ~token ~project_id : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/projects/%s/files" api_base project_id in
  get_json ~sw ~net ~clock ~client ~token url

(** 파일의 로컬 변수 (Design Tokens) *)
let get_local_variables ~clock ~sw ~net ~client ~token ~file_key : (Yojson.Safe.t, api_error) result =
  let url = sprintf "%s/files/%s/variables/local" api_base file_key in
  get_json ~sw ~net ~clock ~client ~token url

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
