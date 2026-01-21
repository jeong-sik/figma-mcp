(** LLM MCP HTTP Client (Pure Eio) *)

let default_url = "http://127.0.0.1:8932/mcp"
let default_timeout_ms = 120_000

let () = Random.self_init ()

type client = Cohttp_eio.Client.t

let retry_policy = {
  Mcp_resilience.default_policy with
  max_attempts = 3;
  initial_delay_ms = 1000;
  max_delay_ms = 5000;
}

let llm_breaker = Mcp_resilience.create_circuit_breaker ~name:"llm_mcp_eio" ~failure_threshold:3 ()

let getenv_opt name =
  try Some (Sys.getenv name)
  with Not_found -> None

let llm_url () =
  match getenv_opt "MCP_SLOT_URL" with
  | Some url -> url
  | None ->
      (match getenv_opt "LLM_MCP_URL" with
       | Some url -> url
       | None -> default_url)

let timeout_ms () =
  match getenv_opt "MCP_SLOT_TIMEOUT_MS" with
  | Some raw -> (match int_of_string_opt raw with Some v -> v | None -> default_timeout_ms)
  | None ->
      (match getenv_opt "LLM_MCP_TIMEOUT_MS" with
       | Some raw -> (match int_of_string_opt raw with Some v -> v | None -> default_timeout_ms)
       | None -> default_timeout_ms)

let member key json =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let get_string key json =
  match member key json with
  | Some (`String s) -> Some s
  | _ -> None

let get_bool key json =
  match member key json with
  | Some (`Bool b) -> Some b
  | _ -> None

let contains_substring s substring =
  let len = String.length substring in
  let rec loop i =
    if i + len > String.length s then false
    else if String.sub s i len = substring then true
    else loop (i + 1)
  in
  if len = 0 then true else loop 0

let parse_json payload =
  try Some (Yojson.Safe.from_string payload)
  with Yojson.Json_error _ -> None

let parse_sse_stream body_str =
  let lines = String.split_on_char '\n' body_str in
  let rec loop data_lines last_json = function
    | [] ->
        let last_json =
          match data_lines with
          | [] -> last_json
          | _ ->
              let data = String.concat "\n" (List.rev data_lines) in
              (match parse_json data with Some j -> Some j | None -> last_json)
        in
        last_json
    | line :: rest ->
        if String.trim line = "" then
          let last_json =
            match data_lines with
            | [] -> last_json
            | _ ->
                let data = String.concat "\n" (List.rev data_lines) in
                (match parse_json data with Some j -> Some j | None -> last_json)
          in
          loop [] last_json rest
        else if String.length line >= 5 && String.sub line 0 5 = "data:" then
          let data = String.sub line 5 (String.length line - 5) |> String.trim in
          loop (data :: data_lines) last_json rest
        else
          loop data_lines last_json rest
  in
  loop [] None lines

(** With retry (Eio version) - blocking, runs inside Eio *)
let with_retry_eio ~clock ~(policy : Mcp_resilience.retry_policy) ~circuit_breaker ~op_name:_ op =
  let is_retryable_error msg =
    let retryable_prefix = "Retryable" in
    let has_prefix =
      String.length msg >= String.length retryable_prefix
      && String.sub msg 0 (String.length retryable_prefix) = retryable_prefix
    in
    has_prefix || msg = "Timeout"
  in
  let rec attempt n last_error =
    let cb_allows = match circuit_breaker with
      | None -> true
      | Some cb -> Mcp_resilience.circuit_allows cb
    in
    if not cb_allows then
      Mcp_resilience.CircuitOpen
    else if n > policy.max_attempts then
      Mcp_resilience.Error (Option.value last_error ~default:"Max attempts reached")
    else begin
      if n > 1 then begin
        let delay_ms = Mcp_resilience.calculate_delay policy (n - 1) in
        Eio.Time.sleep clock (delay_ms /. 1000.0)
      end;
      let result = op () in
      match result with
      | Ok v ->
          (match circuit_breaker with Some cb -> Mcp_resilience.circuit_record_success cb | None -> ());
          Mcp_resilience.Ok v
      | Error err ->
          (match circuit_breaker with Some cb -> Mcp_resilience.circuit_record_failure cb | None -> ());
          if is_retryable_error err then
            attempt (n + 1) (Some err)
          else
            Mcp_resilience.Error err
    end
  in
  attempt 1 None

(** Post JSON to LLM MCP endpoint (pure Eio) *)
let post_json ~sw ~clock ~client ~url payload : (Yojson.Safe.t, string) result =
  let headers =
    Cohttp.Header.of_list [
      ("Content-Type", "application/json");
      ("Accept", "application/json, text/event-stream");
    ]
  in
  let body_str = Yojson.Safe.to_string payload in
  let body = Cohttp_eio.Body.of_string body_str in
  let uri = Uri.of_string url in
  let timeout_s = float_of_int (timeout_ms ()) /. 1000.0 in

  let op () =
    try
      (* Timeout using Eio.Time.with_timeout *)
      let result =
        Eio.Time.with_timeout clock timeout_s (fun () ->
          let resp, resp_body = Cohttp_eio.Client.post client ~sw ~headers ~body uri in
          let body_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int in
          Ok (resp, body_str))
      in
      match result with
      | Error `Timeout -> Error "Timeout"
      | Ok (resp, body_str) ->
          let status = Cohttp.Response.status resp in
          let code = Cohttp.Code.code_of_status status in
          let resp_headers = Cohttp.Response.headers resp in

          if code = 429 || (code >= 500 && code < 600) then
            Error (Printf.sprintf "Retryable HTTP %d: %s" code body_str)
          else if code < 200 || code >= 300 then
            Error (Printf.sprintf "LLM MCP HTTP %d: %s" code body_str)
          else
            let content_type =
              Cohttp.Header.get resp_headers "content-type"
              |> Option.value ~default:""
              |> String.lowercase_ascii
            in
            if contains_substring content_type "text/event-stream" then
              match parse_sse_stream body_str with
              | Some json -> Ok json
              | None -> Error "LLM MCP SSE parse error: no JSON data"
            else
              match parse_json body_str with
              | Some json -> Ok json
              | None -> Error "LLM MCP JSON parse error: invalid JSON"
    with exn -> Error (Printexc.to_string exn)
  in

  let result = with_retry_eio
    ~clock
    ~policy:retry_policy
    ~circuit_breaker:(Some llm_breaker)
    ~op_name:"llm_mcp_post"
    op
  in
  match result with
  | Mcp_resilience.Ok json -> Ok json
  | Mcp_resilience.Error err -> Error err
  | Mcp_resilience.CircuitOpen -> Error "LLM MCP circuit breaker open"
  | Mcp_resilience.TimedOut -> Error "LLM MCP timed out"

let extract_text_from_content content =
  let extract_item = function
    | `Assoc fields -> (
        match List.assoc_opt "text" fields with
        | Some (`String s) -> Some s
        | _ -> None)
    | _ -> None
  in
  match content with
  | `List items ->
      items |> List.filter_map extract_item |> String.concat "\n"
  | _ -> ""

(** Reuse llm_response type from the Lwt module for type compatibility *)
type llm_response = Llm_mcp_client.llm_response = {
  text: string;
  is_error: bool;
  raw: Yojson.Safe.t;
}

(** Call LLM MCP tool (pure Eio) *)
let call_tool ~sw ~clock ~client ~url ~name ~arguments : (llm_response, string) result =
  let payload =
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int (Random.int 1_000_000));
      ("method", `String "tools/call");
      ("params", `Assoc [
        ("name", `String name);
        ("arguments", arguments);
      ]);
    ]
  in
  let response = post_json ~sw ~clock ~client ~url payload in
  match response with
  | Error msg -> Error msg
  | Ok json ->
      (match member "error" json with
       | Some (`Assoc fields) ->
           let message =
             match List.assoc_opt "message" fields with
             | Some (`String s) -> s
             | _ -> Yojson.Safe.to_string (`Assoc fields)
           in
           Error ("LLM MCP error: " ^ message)
       | Some other ->
           Error ("LLM MCP error: " ^ Yojson.Safe.to_string other)
       | None ->
           match member "result" json with
           | Some result ->
               let content = member "content" result |> Option.value ~default:`Null in
               let text = extract_text_from_content content in
               let is_error = get_bool "isError" result |> Option.value ~default:false in
               let text =
                 if text = "" then Yojson.Safe.pretty_to_string result else text
               in
               Ok { text; is_error; raw = json }
           | None ->
               Error "LLM MCP response missing result")
