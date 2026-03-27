open Alcotest

let mk_temp_dir prefix =
  let p = Filename.temp_file prefix "" in
  (try Sys.remove p with _ -> ());
  Unix.mkdir p 0o700;
  p

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let test_reference_path_allowed_under_root () =
  let dir = mk_temp_dir "figma-mcp-vision-root-" in
  let path = Filename.concat dir "ref.png" in
  write_file path "x";
  match Figma_protocol_eio.validate_reference_image_path ~roots:[dir] ~max_bytes:1024 path with
  | Ok _ -> ()
  | Error e -> fail e

let test_reference_path_reject_outside_root () =
  let dir1 = mk_temp_dir "figma-mcp-vision-root-a-" in
  let dir2 = mk_temp_dir "figma-mcp-vision-root-b-" in
  let path = Filename.concat dir2 "ref.png" in
  write_file path "x";
  match Figma_protocol_eio.validate_reference_image_path ~roots:[dir1] ~max_bytes:1024 path with
  | Ok _ -> fail "expected rejection for path outside allowed roots"
  | Error _ -> ()

let test_reference_path_reject_non_png () =
  let dir = mk_temp_dir "figma-mcp-vision-root-" in
  let path = Filename.concat dir "ref.txt" in
  write_file path "x";
  match Figma_protocol_eio.validate_reference_image_path ~roots:[] ~max_bytes:1024 path with
  | Ok _ -> fail "expected rejection for non-png reference"
  | Error _ -> ()

let test_reference_path_reject_too_large () =
  let dir = mk_temp_dir "figma-mcp-vision-root-" in
  let path = Filename.concat dir "ref.png" in
  write_file path "xx";
  match Figma_protocol_eio.validate_reference_image_path ~roots:[] ~max_bytes:1 path with
  | Ok _ -> fail "expected rejection for oversize reference"
  | Error _ -> ()

let test_reference_path_reject_non_regular_file () =
  let dir = mk_temp_dir "figma-mcp-vision-root-" in
  let dpng = Filename.concat dir "not-a-file.png" in
  Unix.mkdir dpng 0o700;
  match Figma_protocol_eio.validate_reference_image_path ~roots:[] ~max_bytes:1024 dpng with
  | Ok _ -> fail "expected rejection for non-regular file"
  | Error _ -> ()

let () =
  run "Vision Compare Security" [
    "reference_path", [
      "allowed under root", `Quick, test_reference_path_allowed_under_root;
      "reject outside root", `Quick, test_reference_path_reject_outside_root;
      "reject non png", `Quick, test_reference_path_reject_non_png;
      "reject too large", `Quick, test_reference_path_reject_too_large;
      "reject non-regular", `Quick, test_reference_path_reject_non_regular_file;
    ];
  ]

