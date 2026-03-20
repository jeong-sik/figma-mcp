open Alcotest

let contains ~needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

let test_v2_surface_doc () =
  match Mcp_tool_registry.read_resource "figma://docs/v2-surface" with
  | Error err -> fail err
  | Ok (_mime, body) ->
      check bool "mentions scope" true
        (contains ~needle:"design context extraction and verification only" body)

let test_verification_doc () =
  match Mcp_tool_registry.read_resource "figma://docs/verification" with
  | Error err -> fail err
  | Ok (_mime, body) ->
      check bool "mentions semantic" true
        (contains ~needle:"figma_verify_semantic" body)

let () =
  run "v2-resources"
    [
      ("docs", [
         test_case "surface doc" `Quick test_v2_surface_doc;
         test_case "verification doc" `Quick test_verification_doc;
       ]);
    ]
