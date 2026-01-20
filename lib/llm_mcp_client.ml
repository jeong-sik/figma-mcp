(** LLM MCP HTTP Client (Lwt) *)

open Lwt.Syntax

module Resilience = struct
  type retry_policy = {
    max_attempts: int;
    initial_delay_ms: int;
    max_delay_ms: int;
    backoff_multiplier: float;
  }

  type circuit_state =
    | Closed
    | Open
    | HalfOpen

  type circuit_breaker = {
    name: string;
    failure_threshold: int;
    timeout_ms: int;
    mutable state: circuit_state;
    mutable failure_count: int;
    mutable last_failure_time: float;
  }

  type 'a retry_result =
    | Ok of 'a
    | Error of string
    | CircuitOpen
    | TimedOut

  let default_policy = {
    max_attempts = 3;
    initial_delay_ms = 100;
    max_delay_ms = 10_000;
    backoff_multiplier = 2.0;
  }

  let calculate_delay policy attempt =
    let base_delay = float_of_int policy.initial_delay_ms in
    let multiplied = base_delay *. (policy.backoff_multiplier ** float_of_int (attempt - 1)) in
    min multiplied (float_of_int policy.max_delay_ms)

  let create_circuit_breaker ?(failure_threshold=5) ?(timeout_ms=30_000) ~name () =
    {
      name;
      failure_threshold;
      timeout_ms;
      state = Closed;
      failure_count = 0;
      last_failure_time = 0.0;
    }

  let circuit_allows cb =
    match cb.state with
    | Closed -> true
    | HalfOpen -> true
    | Open ->
        let now = Unix.gettimeofday () in
        let elapsed_ms = (now -. cb.last_failure_time) *. 1000.0 in
        if elapsed_ms >= float_of_int cb.timeout_ms then begin
          cb.state <- HalfOpen;
          true
        end else
          false

  let circuit_record_success cb =
    cb.failure_count <- 0;
    cb.state <- Closed

  let circuit_record_failure cb =
    cb.last_failure_time <- Unix.gettimeofday ();
    cb.failure_count <- cb.failure_count + 1;
    if cb.failure_count >= cb.failure_threshold then
      cb.state <- Open
end

let default_url = "http://127.0.0.1:8932/mcp"
let default_timeout_ms = 120_000

let () = Random.self_init ()

let retry_policy = {
  Resilience.default_policy with
  max_attempts = 3;
  initial_delay_ms = 1000;
  max_delay_ms = 5000;
}

let llm_breaker = Resilience.create_circuit_breaker ~name:"llm_mcp" ~failure_threshold:3 ()

exception LlmRetryableError of string

let with_retry_lwt ~policy ~circuit_breaker ~op_name:_ op =
  let rec attempt n last_error =
    let cb_allows =
      match circuit_breaker with
      | None -> true
      | Some cb -> Resilience.circuit_allows cb
    in
    if not cb_allows then
      Lwt.return Resilience.CircuitOpen
    else if n > policy.Resilience.max_attempts then
      let message = Option.value ~default:"Max attempts reached" last_error in
      Lwt.return (Resilience.Error message)
    else
      let* () =
        if n > 1 then
          let delay_ms = Resilience.calculate_delay policy (n - 1) in
          Lwt_unix.sleep (delay_ms /. 1000.0)
        else
          Lwt.return_unit
      in
      Lwt.catch
        (fun () ->
          let* result = op () in
          (match result with
           | Ok _ ->
               (match circuit_breaker with
                | Some cb -> Resilience.circuit_record_success cb
                | None -> ());
               Lwt.return (Resilience.Ok result)
           | Error msg ->
               (match circuit_breaker with
                | Some cb -> Resilience.circuit_record_failure cb
                | None -> ());
               Lwt.return (Resilience.Ok (Error msg))))
        (function
          | LlmRetryableError msg ->
              (match circuit_breaker with
               | Some cb -> Resilience.circuit_record_failure cb
               | None -> ());
              attempt (n + 1) (Some msg)
          | Lwt_unix.Timeout ->
              (match circuit_breaker with
               | Some cb -> Resilience.circuit_record_failure cb
               | None -> ());
              attempt (n + 1) (Some "Timeout")
          | exn ->
              let message = Printexc.to_string exn in
              (match circuit_breaker with
               | Some cb -> Resilience.circuit_record_failure cb
               | None -> ());
              Lwt.return (Resilience.Error message))
  in
  attempt 1 None

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

let post_json ~url payload : (Yojson.Safe.t, string) result Lwt.t =
  let headers = Cohttp.Header.of_list [
    ("Content-Type", "application/json");
    ("Accept", "application/json, text/event-stream");
  ] in
  let body = Yojson.Safe.to_string payload |> Cohttp_lwt.Body.of_string in
  let uri = Uri.of_string url in
  let timeout_s = float_of_int (timeout_ms ()) /. 1000.0 in

  let op () =
    Lwt.catch
      (fun () ->
        let* result = 
          Lwt_unix.with_timeout timeout_s (fun () ->
            let* (resp, body) = Cohttp_lwt_unix.Client.post ~headers ~body uri in
            let* body_str = Cohttp_lwt.Body.to_string body in
            Lwt.return (resp, body_str))
        in
        let status = Cohttp.Response.status (fst result) in
        let code = Cohttp.Code.code_of_status status in
        let resp_headers = Cohttp.Response.headers (fst result) in
        let body_str = snd result in

        if code = 429 || (code >= 500 && code < 600) then
          Lwt.fail (LlmRetryableError (Printf.sprintf "HTTP %d: %s" code body_str))
        else if code < 200 || code >= 300 then
          Lwt.return (Result.Error (Printf.sprintf "LLM MCP HTTP %d: %s" code body_str))
        else
          let content_type =
            Cohttp.Header.get resp_headers "content-type"
            |> Option.value ~default:""
            |> String.lowercase_ascii
          in
          let contains_substring s substring =
            let len = String.length substring in
            let rec loop i =
              if i + len > String.length s then false
              else if String.sub s i len = substring then true
              else loop (i + 1)
            in
            if len = 0 then true else loop 0
          in
          let parse_json payload =
            try Some (Yojson.Safe.from_string payload)
            with Yojson.Json_error _ -> None
          in
          if contains_substring content_type "text/event-stream" then
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
            (match loop [] None lines with 
             | Some json -> Lwt.return (Result.Ok json)
             | None -> Lwt.return (Result.Error "LLM MCP SSE parse error: no JSON data"))
          else 
            match parse_json body_str with 
            | Some json -> Lwt.return (Result.Ok json)
            | None -> Lwt.return (Result.Error "LLM MCP JSON parse error: invalid JSON"))
      (function
        | Lwt_unix.Timeout -> Lwt.fail (LlmRetryableError "Timeout")
        | LlmRetryableError msg -> Lwt.fail (LlmRetryableError msg)
        | exn -> Lwt.return (Result.Error (Printexc.to_string exn)))
  in

  let* result = with_retry_lwt
    ~policy:retry_policy
    ~circuit_breaker:(Some llm_breaker)
    ~op_name:"llm_mcp_post"
    op
  in
  match result with
  | Resilience.Ok (Ok json) -> Lwt.return (Ok json)
  | Resilience.Ok (Error err) -> Lwt.return (Error err)
  | Resilience.Error err -> Lwt.return (Error ("LLM MCP exhausted: " ^ err))
  | Resilience.CircuitOpen -> Lwt.return (Error "LLM MCP circuit breaker open")
  | Resilience.TimedOut -> Lwt.return (Error "LLM MCP timed out")

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

type llm_response = {
  text: string;
  is_error: bool;
  raw: Yojson.Safe.t;
}

let call_tool ~url ~name ~arguments : (llm_response, string) result Lwt.t =
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
  let* response = post_json ~url payload in
  match response with
  | Error msg -> Lwt.return (Error msg)
  | Ok json ->
      (match member "error" json with
       | Some (`Assoc fields) ->
           let message =
             match List.assoc_opt "message" fields with
             | Some (`String s) -> s
             | _ -> Yojson.Safe.to_string (`Assoc fields)
           in
           Lwt.return (Error ("LLM MCP error: " ^ message))
       | Some other ->
           Lwt.return (Error ("LLM MCP error: " ^ Yojson.Safe.to_string other))
       | None ->
           match member "result" json with
           | Some result ->
               let content = member "content" result |> Option.value ~default:`Null in
               let text = extract_text_from_content content in
               let is_error = get_bool "isError" result |> Option.value ~default:false in
               let text =
                 if text = "" then Yojson.Safe.pretty_to_string result else text
               in
               Lwt.return (Ok { text; is_error; raw = json })
           | None ->
               Lwt.return (Error "LLM MCP response missing result"))
