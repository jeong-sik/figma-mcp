open Alcotest

let mk_temp_dir prefix =
  let p = Filename.temp_file prefix "" in
  (try Sys.remove p with _ -> ());
  Unix.mkdir p 0o700;
  p

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

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

let test_output_dir_outside_base_rejected () =
  let args =
    `Assoc [
      ("output_dir", `String "/etc");
      ("image_a", `String "/tmp/a.png");
      ("image_b", `String "/tmp/b.png");
      ("regions", `String {|[{"name":"ok","x":0,"y":0,"width":1,"height":1}]|});
    ]
  in
  match Mcp_tools.handle_compare_regions args with
  | Ok _ -> fail "expected error for output_dir outside allowed base"
  | Error msg ->
      check string "error" "output_dir must be under /tmp/figma-evolution" msg

let test_region_name_traversal_rejected () =
  let image_a = Filename.temp_file "figma-mcp-a-" ".png" in
  let image_b = Filename.temp_file "figma-mcp-b-" ".png" in
  write_file image_a "x";
  write_file image_b "x";
  let args =
    `Assoc [
      ("output_dir", `String "/tmp/figma-evolution/regions");
      ("image_a", `String image_a);
      ("image_b", `String image_b);
      ("regions", `String {|[{"name":"../pwn","x":0,"y":0,"width":1,"height":1}]|});
    ]
  in
  match Mcp_tools.handle_compare_regions args with
  | Ok _ -> fail "expected error for unsafe region name"
  | Error msg ->
      check string "error" "Invalid region name: ../pwn" msg

let test_image_a_missing_rejected () =
  let image_b = Filename.temp_file "figma-mcp-b-" ".png" in
  write_file image_b "x";
  let args =
    `Assoc [
      ("output_dir", `String "/tmp/figma-evolution/regions");
      ("image_a", `String "/tmp/does-not-exist-figma-mcp.png");
      ("image_b", `String image_b);
      ("regions", `String {|[{"name":"ok","x":0,"y":0,"width":1,"height":1}]|});
    ]
  in
  match Mcp_tools.handle_compare_regions args with
  | Ok _ -> fail "expected error for missing image_a"
  | Error msg ->
      check string "error" "image_a image not found" msg

let test_image_a_non_png_rejected () =
  let image_a = Filename.temp_file "figma-mcp-a-" ".txt" in
  let image_b = Filename.temp_file "figma-mcp-b-" ".png" in
  write_file image_a "x";
  write_file image_b "x";
  let args =
    `Assoc [
      ("output_dir", `String "/tmp/figma-evolution/regions");
      ("image_a", `String image_a);
      ("image_b", `String image_b);
      ("regions", `String {|[{"name":"ok","x":0,"y":0,"width":1,"height":1}]|});
    ]
  in
  match Mcp_tools.handle_compare_regions args with
  | Ok _ -> fail "expected error for non-png image_a"
  | Error msg ->
      check string "error" "image_a must be a .png file" msg

let test_image_root_restriction_enforced () =
  let root = mk_temp_dir "figma-mcp-roots-" in
  let in_root = Filename.concat root "in.png" in
  write_file in_root "x";
  let outside = Filename.temp_file "figma-mcp-outside-" ".png" in
  write_file outside "x";
  with_env "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Some root) (fun () ->
      let args =
        `Assoc [
          ("output_dir", `String "/tmp/figma-evolution/regions");
          ("image_a", `String outside);
          ("image_b", `String in_root);
          ("regions", `String {|[{"name":"ok","x":0,"y":0,"width":1,"height":1}]|});
        ]
      in
      match Mcp_tools.handle_compare_regions args with
      | Ok _ -> fail "expected error for image_a outside allowed roots"
      | Error msg ->
          check string "error"
            "image_a path not allowed (set FIGMA_MCP_COMPARE_IMAGE_ROOTS)" msg)

let () =
  run "compare-regions-security" [
    ("security", [
      test_case "reject output_dir outside base" `Quick test_output_dir_outside_base_rejected;
      test_case "reject traversal region name" `Quick test_region_name_traversal_rejected;
      test_case "reject missing image_a" `Quick test_image_a_missing_rejected;
      test_case "reject non-png image_a" `Quick test_image_a_non_png_rejected;
      test_case "enforce image roots allowlist" `Quick test_image_root_restriction_enforced;
    ]);
  ]
