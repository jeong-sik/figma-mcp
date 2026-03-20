(** Figma MCP CLI / Server *)

open Cmdliner

(** ============== MCP 서버 모드 ============== *)

let run_mcp_server_stdio () =
  let server = Figma_mcp.Tools.create_figma_server () in
  Mcp_protocol_server.start_stdio_server server

let run_mcp_server_http ~host ~port ~allow_no_auth =
  let server = Figma_mcp.Tools.create_figma_server () in
  let config = { Mcp_protocol_server.default_config with port; host } in
  Mcp_figma_tool_handlers.set_allow_no_auth allow_no_auth;
  Mcp_protocol_server.start_server ~config server

(** ============== CLI 코드젠 모드 ============== *)

let format_arg =
  let doc = "Output format: fidelity (default), html" in
  Arg.(value & opt string "fidelity" & info ["f"; "format"] ~doc)

let components_arg =
  let doc = "Split each top-level frame as separate component" in
  Arg.(value & flag & info ["c"; "components"] ~doc)

let server_arg =
  let doc = "Run as MCP stdio server (for Claude Code integration)" in
  Arg.(value & flag & info ["server"] ~doc)

let host_arg =
  let default_host =
    match Sys.getenv_opt "FIGMA_MCP_HOST" with
    | Some v when String.trim v <> "" -> v
    | _ -> "127.0.0.1"
  in
  let doc = "Host to bind (default: 127.0.0.1; non-loopback requires --public or FIGMA_MCP_PUBLIC=1)" in
  Arg.(value & opt string default_host & info ["host"] ~doc)

let port_arg =
  let doc = "Run as MCP HTTP server on specified port (e.g., --port 8933)" in
  Arg.(value & opt (some int) None & info ["port"] ~doc)

let public_arg =
  let doc = "Allow binding to non-loopback host (sets FIGMA_MCP_PUBLIC=1)" in
  Arg.(value & flag & info ["public"] ~doc)

let allow_no_auth_arg =
  let doc = "Allow running without API key (sets FIGMA_MCP_ALLOW_NO_AUTH=1; not recommended)" in
  Arg.(value & flag & info ["allow-no-auth"] ~doc)

let normalize_lower s = String.lowercase_ascii s
let normalize_env value =
  match value with
  | None -> None
  | Some v ->
      let trimmed = String.trim v in
      if trimmed = "" then None else Some trimmed

let has_api_key () =
  match normalize_env (Sys.getenv_opt "FIGMA_MCP_API_KEY") with
  | Some _ -> true
  | None ->
      (match normalize_env (Sys.getenv_opt "MCP_API_KEY") with
       | Some _ -> true
       | None -> false)

let is_loopback_host host =
  match normalize_lower host with
  | "localhost" | "127.0.0.1" | "::1" | "[::1]" -> true
  | _ -> false

let require_api_key allow_no_auth =
  if allow_no_auth then ()
  else
    if has_api_key () then ()
    else begin
      Printf.eprintf
        "FIGMA-MCP: FIGMA_MCP_API_KEY (or MCP_API_KEY) is required. Set --allow-no-auth or FIGMA_MCP_ALLOW_NO_AUTH=1 to bypass.\n%!";
      exit 2
    end

let run format components server host port public allow_no_auth =
  if public then Unix.putenv "FIGMA_MCP_PUBLIC" "1";
  if allow_no_auth then Unix.putenv "FIGMA_MCP_ALLOW_NO_AUTH" "1";
  let public_enabled = Figma_mcp.Mcp_http_auth.env_truthy "FIGMA_MCP_PUBLIC" in
  let allow_no_auth_enabled = Figma_mcp.Mcp_http_auth.env_truthy "FIGMA_MCP_ALLOW_NO_AUTH" in
  if (not (is_loopback_host host)) && not public_enabled then begin
    Printf.eprintf
      "FIGMA-MCP: Refusing to bind to non-loopback host (%s) without --public or FIGMA_MCP_PUBLIC=1\n%!"
      host;
    exit 2
  end;
  let http_enabled = match port with Some _ -> true | None -> false in
  if http_enabled
     && (not (is_loopback_host host))
     && allow_no_auth_enabled
     && not (Figma_mcp.Mcp_http_auth.env_truthy "FIGMA_MCP_ALLOW_NO_AUTH_PUBLIC") then begin
    Printf.eprintf
      "FIGMA-MCP: Refusing to run allow-no-auth on non-loopback host (%s). Set FIGMA_MCP_ALLOW_NO_AUTH_PUBLIC=1 to override.\n%!"
      host;
    exit 2
  end;
  if http_enabled then require_api_key allow_no_auth_enabled;
  match port with
  | Some http_p ->
      run_mcp_server_http ~host ~port:http_p ~allow_no_auth:allow_no_auth_enabled
  | None ->
      if server then
        run_mcp_server_stdio ()
      else begin
    (* stdin 읽기 *)
    let buf = Buffer.create 8192 in
    (try
      while true do
        Buffer.add_string buf (input_line stdin);
        Buffer.add_char buf '\n'
      done
    with End_of_file -> ());
    let json_str = Buffer.contents buf in

    match format with
    | "fidelity" | "accuracy" | "pixel" ->
        (try
          let json = Yojson.Safe.from_string json_str in
          print_endline (Figma_mcp.Codegen.generate_fidelity json)
        with Yojson.Json_error _ ->
          print_endline "Error: Failed to parse JSON")
    | "html" ->
        (match Figma_mcp.Parser.parse_json_string json_str with
         | None -> print_endline "Error: Failed to parse JSON"
         | Some node ->
             if components then
               print_endline (Figma_mcp.Codegen.split_to_components node)
             else
               print_endline (Figma_mcp.Codegen.generate_html node))
    | _ ->
        print_endline ("Unknown format: " ^ format)
  end

let cmd =
  let doc = "Figma MCP Server & DSL Codegen Tool" in
  let man = [
    `S "DESCRIPTION";
    `P "Run as MCP server with --server flag, or use as CLI codegen tool.";
    `S "EXAMPLES";
    `P "MCP Server:";
    `Pre "  figma-mcp --server";
    `P "CLI Codegen:";
    `Pre "  cat figma.json | figma-mcp -f fidelity";
  ] in
  let info = Cmd.info "figma-mcp" ~version:Version.version ~doc ~man in
  Cmd.v info
    Term.(const run
      $ format_arg
      $ components_arg
      $ server_arg
      $ host_arg
      $ port_arg
      $ public_arg
      $ allow_no_auth_arg)

let () = exit (Cmd.eval cmd)
