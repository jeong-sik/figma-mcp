open Alcotest

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
  let args =
    `Assoc [
      ("output_dir", `String "/tmp/figma-evolution/regions");
      ("image_a", `String "/tmp/a.png");
      ("image_b", `String "/tmp/b.png");
      ("regions", `String {|[{"name":"../pwn","x":0,"y":0,"width":1,"height":1}]|});
    ]
  in
  match Mcp_tools.handle_compare_regions args with
  | Ok _ -> fail "expected error for unsafe region name"
  | Error msg ->
      check string "error" "Invalid region name: ../pwn" msg

let () =
  run "compare-regions-security" [
    ("security", [
      test_case "reject output_dir outside base" `Quick test_output_dir_outside_base_rejected;
      test_case "reject traversal region name" `Quick test_region_name_traversal_rejected;
    ]);
  ]
