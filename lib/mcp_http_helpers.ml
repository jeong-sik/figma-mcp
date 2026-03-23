(** HTTP request/response helpers for MCP server.

    Provides [Response] (send HTTP responses with CORS headers),
    [Request] (read and parse HTTP request bodies),
    and shared JSON utility functions. *)

open Printf

(** ============== Response Helpers ============== *)

module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Mcp_cors.headers reqd ~include_methods:false ~include_headers:false) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Mcp_cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let api_key_error message reqd =
    let body = Yojson.Safe.to_string (`Assoc [
      ("error", `String message);
    ]) in
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("www-authenticate", "API-Key");
    ] @ Mcp_cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers `Unauthorized in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd `Unauthorized

  let accepted reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-length", "0");
    ] @ Mcp_cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers `Accepted in
    Httpun.Reqd.respond_with_string reqd response "";
    Server_metrics.finish_reqd ~bytes:0 reqd `Accepted

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-length", "0");
    ] @ Mcp_cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response "";
    Server_metrics.finish_reqd ~bytes:0 reqd `No_content

  (** SSE streaming response for MCP streamable-http protocol *)
  let sse_stream reqd ~on_write =
    let headers = Httpun.Headers.of_list ([
      ("content-type", Mcp_protocol.Http_negotiation.mcp_sse_content_type);
      ("cache-control", "no-cache");
      ("connection", "keep-alive");
    ] @ Mcp_cors.headers reqd ~include_methods:false ~include_headers:false) in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    Server_metrics.finish_reqd ~bytes:0 reqd `OK;
    on_write body

  (** SSE single message response for POST->SSE (MCP Streamable HTTP) *)
  let sse_message ?(session_id="") json_str reqd =
    (* Ensure JSON is single-line to prevent SSE frame corruption *)
    let safe_json = match Yojson.Safe.from_string json_str with
      | json -> Yojson.Safe.to_string json
      | exception (Yojson.Json_error _) -> json_str
    in
    let event_id = Printf.sprintf "s%d-%d" (Unix.getpid ()) (Random.int 10000) in
    let prime = Printf.sprintf "retry: 5000\nid: %s:2\n\n" event_id in
    let message = Printf.sprintf "id: %s:1\ndata: %s\n\n" event_id safe_json in
    let body = prime ^ message in
    let session_headers = if session_id = "" then [] else [("mcp-session-id", session_id)] in
    let headers = Httpun.Headers.of_list ([
      ("content-type", Mcp_protocol.Http_negotiation.mcp_sse_content_type);
      ("content-length", string_of_int (String.length body));
      ("cache-control", "no-cache");
    ] @ Mcp_cors.headers reqd ~include_methods:true ~include_headers:true
      @ session_headers) in
    let response = Httpun.Response.create ~headers `OK in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd `OK
end

(** ============== Request Helpers ============== *)

module Request = struct
  (** Read request body - accumulates chunks until EOF.
      Uses callback pattern - the response MUST be sent from within the callback. *)
  let default_max_body_bytes = 50 * 1024 * 1024

  let parse_positive_int value =
    try
      let v = int_of_string value in
      if v > 0 then Some v else None
    with Failure _ -> None

  let max_body_bytes =
    let from_env name =
      match Sys.getenv_opt name with
      | Some v -> parse_positive_int v
      | None -> None
    in
    match from_env "FIGMA_MCP_MAX_BODY_BYTES" with
    | Some v -> v
    | None ->
        (match from_env "MCP_MAX_BODY_BYTES" with
         | Some v -> v
         | None -> default_max_body_bytes)

  let respond_error reqd status body =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("connection", "close");
    ] @ Mcp_cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let respond_too_large reqd max_bytes =
    let body = Printf.sprintf
      "413 Request Entity Too Large (max %d bytes)" max_bytes
    in
    respond_error reqd `Payload_too_large body

  let respond_internal_error reqd exn =
    eprintf "[http] internal error: %s\n%!" (Printexc.to_string exn);
    respond_error reqd `Internal_server_error "500 Internal Server Error"

  let read_body_async reqd callback =
    let request = Httpun.Reqd.request reqd in
    let content_length =
      match Httpun.Headers.get request.Httpun.Request.headers "content-length" with
      | Some v -> parse_positive_int v
      | None -> None
    in
    let body = Httpun.Reqd.request_body reqd in
    let stopped = ref false in
    let stop () =
      if not !stopped then begin
        stopped := true;
        (try Httpun.Body.Reader.close body
         with exn -> eprintf "[mcp_protocol] Warning: body reader close failed: %s\n%!" (Printexc.to_string exn))
      end
    in
    match content_length with
    | Some len when len > max_body_bytes ->
        stop ();
        respond_too_large reqd max_body_bytes
    | _ ->
        let initial_capacity =
          match content_length with
          | Some len when len > 0 && len < max_body_bytes -> len
          | _ -> 1024
        in
        let buf = Buffer.create initial_capacity in
        let seen_bytes = ref 0 in
        let rec read_loop () =
          Httpun.Body.Reader.schedule_read body
            ~on_eof:(fun () ->
              let body_str = Buffer.contents buf in
              try callback body_str with exn ->
                respond_internal_error reqd exn)
            ~on_read:(fun buffer ~off ~len ->
              if !stopped then ()
              else
                let next_bytes = !seen_bytes + len in
                if next_bytes > max_body_bytes then begin
                  stop ();
                  respond_too_large reqd max_body_bytes
                end else begin
                  seen_bytes := next_bytes;
                  let chunk = Bigstringaf.substring buffer ~off ~len in
                  Buffer.add_string buf chunk;
                  read_loop ()
                end)
        in
        read_loop ()

  let path (request : Httpun.Request.t) =
    request.target |> String.split_on_char '?' |> List.hd

  let method_ (request : Httpun.Request.t) =
    request.meth

  (** Check if client accepts streamable MCP per shared SDK negotiation.
      The current MCP HTTP rules require both JSON and SSE in Accept. *)
  let accepts_sse (request : Httpun.Request.t) =
    match Httpun.Headers.get request.Httpun.Request.headers "accept" with
    | Some accept -> Mcp_protocol.Http_negotiation.accepts_streamable_mcp accept
    | None -> false
end

(** ============== Shared JSON Utilities ============== *)

let json_error ?(status=`Bad_request) msg reqd =
  let body = Yojson.Safe.to_string (`Assoc [("error", `String msg)]) in
  Response.json ~status body reqd

let parse_json body_str =
  if String.trim body_str = "" then Ok `Null
  else
    try Ok (Yojson.Safe.from_string body_str)
    with Yojson.Json_error msg -> Error msg

let get_string_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> Some s
       | _ -> None)
  | _ -> None

let get_int_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Int i) -> Some i
       | Some (`Float f) -> Some (int_of_float f)
       | _ -> None)
  | _ -> None

let get_bool_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Bool b) -> Some b
       | _ -> None)
  | _ -> None

let get_payload_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some json -> Some json
       | _ -> None)
  | _ -> None
