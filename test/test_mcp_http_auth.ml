open Alcotest

let headers_of_list = Httpun.Headers.of_list

let with_env name value f =
  let prev = Sys.getenv_opt name in
  let restore () =
    match prev with
    | None -> Unix.putenv name ""
    | Some v -> Unix.putenv name v
  in
  (match value with
  | None -> Unix.putenv name ""
  | Some v -> Unix.putenv name v);
  match f () with
  | result ->
      restore ();
      result
  | exception exn ->
      restore ();
      raise exn

let test_extract_x_mcp_api_key () =
  let headers = headers_of_list [ ("X-MCP-API-Key", "abc123") ] in
  check (option string) "x-mcp-api-key" (Some "abc123")
    (Figma_mcp.Mcp_http_auth.extract_api_key headers)

let test_extract_x_api_key_fallback () =
  let headers = headers_of_list [ ("X-API-Key", "fallback") ] in
  check (option string) "x-api-key" (Some "fallback")
    (Figma_mcp.Mcp_http_auth.extract_api_key headers)

let test_extract_bearer_token () =
  let headers = headers_of_list [ ("Authorization", "Bearer token123") ] in
  check (option string) "authorization bearer" (Some "token123")
    (Figma_mcp.Mcp_http_auth.extract_api_key headers)

let test_check_api_key_missing () =
  let headers = headers_of_list [] in
  with_env "FIGMA_MCP_API_KEY" None (fun () ->
      match
        Figma_mcp.Mcp_http_auth.check_api_key ~env_name:"FIGMA_MCP_API_KEY"
          ~allow_no_auth:false headers
      with
      | Error Figma_mcp.Mcp_http_auth.Missing -> ()
      | _ -> fail "expected Missing")

let test_check_api_key_invalid () =
  let headers = headers_of_list [ ("X-MCP-API-Key", "nope") ] in
  with_env "FIGMA_MCP_API_KEY" (Some "secret") (fun () ->
      match
        Figma_mcp.Mcp_http_auth.check_api_key ~env_name:"FIGMA_MCP_API_KEY"
          ~allow_no_auth:false headers
      with
      | Error Figma_mcp.Mcp_http_auth.Invalid -> ()
      | _ -> fail "expected Invalid")

let test_check_api_key_ok () =
  let headers = headers_of_list [ ("X-MCP-API-Key", "secret") ] in
  with_env "FIGMA_MCP_API_KEY" (Some "secret") (fun () ->
      match
        Figma_mcp.Mcp_http_auth.check_api_key ~env_name:"FIGMA_MCP_API_KEY"
          ~allow_no_auth:false headers
      with
      | Ok () -> ()
      | _ -> fail "expected Ok")

let test_check_mcp_api_key_ok () =
  let headers = headers_of_list [ ("X-MCP-API-Key", "secret") ] in
  with_env "MCP_API_KEY" (Some "secret") (fun () ->
      match
        Figma_mcp.Mcp_http_auth.check_api_key ~env_name:"MCP_API_KEY"
          ~allow_no_auth:false headers
      with
      | Ok () -> ()
      | _ -> fail "expected Ok")

let test_allow_no_auth () =
  let headers = headers_of_list [] in
  with_env "FIGMA_MCP_API_KEY" None (fun () ->
      match
        Figma_mcp.Mcp_http_auth.check_api_key ~env_name:"FIGMA_MCP_API_KEY"
          ~allow_no_auth:true headers
      with
      | Ok () -> ()
      | _ -> fail "expected Ok")

let () =
  run "mcp_http_auth"
    [ ( "mcp_http_auth",
        [ test_case "extract x-mcp-api-key" `Quick test_extract_x_mcp_api_key;
          test_case "extract x-api-key" `Quick test_extract_x_api_key_fallback;
          test_case "extract bearer" `Quick test_extract_bearer_token;
          test_case "check missing" `Quick test_check_api_key_missing;
          test_case "check invalid" `Quick test_check_api_key_invalid;
          test_case "check ok" `Quick test_check_api_key_ok;
          test_case "check MCP_API_KEY ok" `Quick test_check_mcp_api_key_ok;
          test_case "allow no auth" `Quick test_allow_no_auth ] ) ]
