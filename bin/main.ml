(** Figma MCP CLI / Server *)

open Cmdliner

(** ============== MCP 서버 모드 ============== *)

let run_mcp_server_stdio () =
  let server = Figma_mcp.Tools.create_figma_server () in
  Figma_mcp.Protocol.run_stdio_server server

let run_mcp_server_http ~port =
  let server = Figma_mcp.Tools.create_figma_server () in
  let config = { Figma_mcp.Protocol_eio.default_config with port } in
  Figma_mcp.Protocol_eio.start_server ~config server

(** ============== gRPC 서버 모드 ============== *)

let run_grpc_server ~port =
  Figma_mcp.Grpc_server.run_standalone ~port ()

(** ============== HTTP + gRPC 동시 실행 ============== *)

let run_both_servers ~http_port ~grpc_port =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let domain_mgr = Some (Eio.Stdenv.domain_mgr env) in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.both
    (fun () ->
      let server = Figma_mcp.Tools.create_figma_server () in
      let config = { Figma_mcp.Protocol_eio.default_config with port = http_port } in
      Figma_mcp.Protocol_eio.run ~sw ~net ~clock ~domain_mgr config server)
    (fun () ->
      Figma_mcp.Grpc_server.serve ~sw ~env ~port:grpc_port ())

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

let port_arg =
  let doc = "Run as MCP HTTP server on specified port (e.g., --port 8933)" in
  Arg.(value & opt (some int) None & info ["port"] ~doc)

let grpc_port_arg =
  let doc = "Run as gRPC server on specified port for streaming large responses (default: 50052)" in
  Arg.(value & opt (some int) None & info ["grpc-port"] ~doc)

let run format components server port grpc_port =
  match (port, grpc_port) with
  | (Some http_p, Some grpc_p) ->
      (* HTTP + gRPC 동시 실행 *)
      run_both_servers ~http_port:http_p ~grpc_port:grpc_p
  | (Some http_p, None) ->
      run_mcp_server_http ~port:http_p
  | (None, Some grpc_p) ->
      run_grpc_server ~port:grpc_p
  | (None, None) ->
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
  let info = Cmd.info "figma-mcp" ~version:"0.3.2" ~doc ~man in
  Cmd.v info Term.(const run $ format_arg $ components_arg $ server_arg $ port_arg $ grpc_port_arg)

let () = exit (Cmd.eval cmd)
