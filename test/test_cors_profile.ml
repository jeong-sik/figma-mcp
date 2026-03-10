open Alcotest

let with_env name value f =
  let prev = Sys.getenv_opt name in
  (match value with
   | Some v -> Unix.putenv name v
   | None -> Unix.putenv name "");
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f

let with_envs pairs f =
  let rec loop pairs f =
    match pairs with
    | [] -> f ()
    | (k, v) :: rest -> with_env k v (fun () -> loop rest f)
  in
  loop pairs f

let has_header name headers =
  let name = String.lowercase_ascii name in
  List.exists (fun (k, _) -> String.lowercase_ascii k = name) headers

let find_header name headers =
  let name = String.lowercase_ascii name in
  List.find_opt (fun (k, _) -> String.lowercase_ascii k = name) headers

let test_strict_denies_null_by_default () =
  with_envs
    [ ("FIGMA_MCP_CORS_PROFILE", Some "strict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", None);
      ("FIGMA_MCP_CORS_MODE", None) ]
    (fun () ->
      check bool "origin_allowed null" false (Mcp_protocol_eio.Cors.origin_allowed "null");
      let headers =
        Mcp_protocol_eio.Cors.headers_for_origin_opt (Some "null")
          ~include_methods:false ~include_headers:false
      in
      check bool "no allow-origin header" false
        (has_header "access-control-allow-origin" headers))

let test_strict_disables_private_network_by_default () =
  with_envs
    [ ("FIGMA_MCP_CORS_PROFILE", Some "strict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", None);
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", None);
      ("FIGMA_MCP_CORS_MODE", None) ]
    (fun () ->
      check bool "origin_allowed figma" true
        (Mcp_protocol_eio.Cors.origin_allowed "https://www.figma.com");
      let headers =
        Mcp_protocol_eio.Cors.headers_for_origin_opt (Some "https://www.figma.com")
          ~include_methods:false ~include_headers:false
      in
      check bool "no private network header" false
        (has_header "access-control-allow-private-network" headers))

let test_compat_allows_null_without_private_network_by_default () =
  with_envs
    [ ("FIGMA_MCP_CORS_PROFILE", Some "compat");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", None);
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", None);
      ("FIGMA_MCP_CORS_MODE", None) ]
    (fun () ->
      check bool "origin_allowed null" true (Mcp_protocol_eio.Cors.origin_allowed "null");
      let headers =
        Mcp_protocol_eio.Cors.headers_for_origin_opt (Some "null")
          ~include_methods:false ~include_headers:false
      in
      (match find_header "access-control-allow-origin" headers with
       | Some (_, v) -> check string "allow-origin value" "null" v
       | None -> fail "expected access-control-allow-origin");
      check bool "no private network header" false
        (has_header "access-control-allow-private-network" headers))

let test_strict_override_allowed_origins_and_private_network () =
  with_envs
    [ ("FIGMA_MCP_CORS_PROFILE", Some "strict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", Some "null, https://www.figma.com");
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", Some "true");
      ("FIGMA_MCP_CORS_MODE", None) ]
    (fun () ->
      check bool "origin_allowed null" true (Mcp_protocol_eio.Cors.origin_allowed "null");
      let headers =
        Mcp_protocol_eio.Cors.headers_for_origin_opt (Some "null")
          ~include_methods:false ~include_headers:false
      in
      check bool "private network header present" true
        (has_header "access-control-allow-private-network" headers))

let () =
  run "cors_profile"
    [ ( "cors_profile",
        [ test_case "strict denies null by default" `Quick test_strict_denies_null_by_default;
          test_case "strict disables private network by default" `Quick
            test_strict_disables_private_network_by_default;
          test_case "compat allows null without private network by default" `Quick
            test_compat_allows_null_without_private_network_by_default;
          test_case "strict override envs" `Quick
            test_strict_override_allowed_origins_and_private_network ] ) ]
