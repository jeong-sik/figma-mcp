(** Figma Agent Example - agent_core integration demo

    This example demonstrates how to integrate llm_mcp.agent_core with
    figma-mcp tools for AI-powered design-to-code workflows.

    Prerequisites:
    1. opam pin add llm_mcp git+ssh://git@github.com/jeong-sik/llm-mcp.git#main
    2. Add "llm_mcp.agent_core" to your dune libraries

    Usage:
    dune exec ./examples/figma_agent_example.exe -- --token YOUR_FIGMA_TOKEN
*)

open Lwt.Syntax

(** {1 Figma Tool Executor} *)

module Figma_Tools = struct
  open Agent_core.Types

  (** Execute Figma-related tools *)
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    let open Yojson.Safe.Util in
    match tc.name with
    | "figma_get_file" ->
      let token = tc.arguments |> member "token" |> to_string_option |> Option.value ~default:"" in
      let file_key = tc.arguments |> member "file_key" |> to_string_option |> Option.value ~default:"" in
      if token = "" || file_key = "" then
        Lwt.return (Result.Ok (ToolError "Missing token or file_key"))
      else begin
        let* result = Figma_api.get_file ~token ~file_key () in
        match result with
        | Error e -> Lwt.return (Result.Ok (ToolError e))
        | Ok json ->
          let dsl = match Figma_parser.parse_json json with
            | None -> "Parse error"
            | Some node -> Figma_codegen.generate_compact node
          in
          Lwt.return (Result.Ok (ToolSuccess dsl))
      end

    | "figma_get_node" ->
      let token = tc.arguments |> member "token" |> to_string_option |> Option.value ~default:"" in
      let file_key = tc.arguments |> member "file_key" |> to_string_option |> Option.value ~default:"" in
      let node_id = tc.arguments |> member "node_id" |> to_string_option |> Option.value ~default:"" in
      if token = "" || file_key = "" || node_id = "" then
        Lwt.return (Result.Ok (ToolError "Missing token, file_key, or node_id"))
      else begin
        let* result = Figma_api.get_file_nodes ~token ~file_key ~node_ids:[node_id] () in
        match result with
        | Error e -> Lwt.return (Result.Ok (ToolError e))
        | Ok json ->
          let dsl = match Figma_parser.parse_json json with
            | None -> "Parse error"
            | Some node -> Figma_codegen.generate_compact node
          in
          Lwt.return (Result.Ok (ToolSuccess dsl))
      end

    | "figma_to_html" ->
      let dsl = tc.arguments |> member "dsl" |> to_string_option |> Option.value ~default:"" in
      if dsl = "" then
        Lwt.return (Result.Ok (ToolError "Missing dsl"))
      else
        (* Note: In real implementation, you'd parse DSL and generate HTML *)
        Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "<div>Generated from: %s</div>" dsl)))

    | _ -> Lwt.return (Result.Error ("Unknown tool: " ^ tc.name))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.id }

  let available_tools () = ["figma_get_file"; "figma_get_node"; "figma_to_html"]
end

(** {1 Tool Definitions} *)

let figma_tools : Agent_core.Types.tool list = [
  {
    name = "figma_get_file";
    description = "Fetch a Figma file and return its structure as DSL";
    parameters = [
      ("token", { Agent_core.Types.param_type = "string"; description = "Figma API token"; required = true; enum = None });
      ("file_key", { Agent_core.Types.param_type = "string"; description = "Figma file key from URL"; required = true; enum = None });
    ];
  };
  {
    name = "figma_get_node";
    description = "Fetch a specific node from a Figma file";
    parameters = [
      ("token", { Agent_core.Types.param_type = "string"; description = "Figma API token"; required = true; enum = None });
      ("file_key", { Agent_core.Types.param_type = "string"; description = "Figma file key"; required = true; enum = None });
      ("node_id", { Agent_core.Types.param_type = "string"; description = "Node ID (e.g., 123:456)"; required = true; enum = None });
    ];
  };
  {
    name = "figma_to_html";
    description = "Convert Figma DSL to HTML code";
    parameters = [
      ("dsl", { Agent_core.Types.param_type = "string"; description = "Figma DSL string"; required = true; enum = None });
    ];
  };
]

(** {1 Agent Loop Setup} *)

module Figma_Agent = Agent_core.Agent_loop_functor.Make
    (Agent_core.Ollama_backend)
    (Figma_Tools)
    (Agent_core.Default_state)

(** {1 Main} *)

let () =
  let token = ref "" in
  let file_key = ref "" in
  let model = ref "qwen3:1.7b" in

  let usage = "Figma Agent Example - AI-powered design-to-code" in
  let specs = [
    ("--token", Arg.Set_string token, "Figma API token");
    ("--file", Arg.Set_string file_key, "Figma file key");
    ("--model", Arg.Set_string model, "Ollama model name (default: qwen3:1.7b)");
  ] in
  Arg.parse specs (fun _ -> ()) usage;

  if !token = "" then begin
    Printf.printf "Usage: %s --token YOUR_TOKEN [--file FILE_KEY] [--model MODEL]\n" Sys.argv.(0);
    exit 1
  end;

  let backend_config = Agent_core.Ollama_backend.{
    base_url = "http://127.0.0.1:11434";
    model = !model;
    temperature = 0.3;
    stream = false;
    timeout_ms = Some 120_000;
  } in

  let loop_config = Agent_core.Types.{
    default_loop_config with
    max_turns = 10;
    timeout_ms = 300_000;  (* 5 minutes *)
    max_messages = 50;
  } in

  let prompt = if !file_key = "" then
      Printf.sprintf "You are a Figma design assistant. The user's Figma token is: %s. Help them analyze their designs." !token
    else
      Printf.sprintf "Analyze the Figma file with key '%s' using token '%s'. Describe the main structure and suggest improvements." !file_key !token
  in

  Printf.printf "Starting Figma Agent with model: %s\n" !model;
  Printf.printf "Prompt: %s\n\n" prompt;

  let result = Lwt_main.run (
    Figma_Agent.run
      ~config:loop_config
      ~backend_config
      ~initial_prompt:prompt
      ~tools:figma_tools
      ()
  ) in

  match result with
  | Agent_core.Types.Completed { response; turns_used } ->
    Printf.printf "\n=== Completed in %d turns ===\n%s\n" turns_used response
  | Agent_core.Types.MaxTurnsReached { last_response; turns_used } ->
    Printf.printf "\n=== Max turns (%d) reached ===\n%s\n" turns_used last_response
  | Agent_core.Types.TimedOut { turns_completed } ->
    Printf.printf "\n=== Timed out after %d turns ===\n" turns_completed
  | Agent_core.Types.Error msg ->
    Printf.printf "\n=== Error ===\n%s\n" msg
  | Agent_core.Types.CircuitOpen ->
    Printf.printf "\n=== Circuit breaker open ===\n"
