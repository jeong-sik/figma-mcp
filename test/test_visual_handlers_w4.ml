(** Coverage Wave 4: mcp_visual_handlers.ml uncovered paths.
    Targets: handle_compare_elements (color/box/full/invalid),
             handle_evolution_report (None run_dir, missing dir),
             handle_compare (mode dispatch),
             handle_image_similarity (missing args),
             handle_verify_visual/semantic (missing args). *)

open Mcp_visual_handlers

(* ============== Helpers ============== *)

let args_of pairs =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs)

let args_with kv = `Assoc kv

let check_error msg result =
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error: %s" msg)

let str_contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  if nl > hl then false
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in loop 0

let check_error_contains ~needle msg result =
  match result with
  | Error e ->
      if not (str_contains ~needle e) then
        Alcotest.fail (Printf.sprintf "%s: expected '%s' in error '%s'" msg needle e)
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error: %s" msg)

let check_ok msg result =
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "expected ok for %s: %s" msg e)

let check_ok_contains ~needle msg result =
  match result with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      if not (str_contains ~needle s) then
        Alcotest.fail (Printf.sprintf "%s: expected '%s' in ok '%s'" msg needle s)
  | Error e -> Alcotest.fail (Printf.sprintf "expected ok for %s: %s" msg e)

(* ============== 1. handle_compare_elements — color ============== *)

let test_compare_elements_color_hex () =
  let args = args_of [
    ("type", "color"); ("color1", "#FF0000"); ("color2", "#00FF00");
  ] in
  let result = handle_compare_elements args in
  check_ok "color hex comparison" result;
  check_ok_contains ~needle:"oklab_distance" "has oklab" result

let test_compare_elements_color_rgb () =
  let args = args_of [
    ("type", "color"); ("color1", "rgb(255,0,0)"); ("color2", "rgb(0,255,0)");
  ] in
  let result = handle_compare_elements args in
  check_ok "color rgb comparison" result;
  check_ok_contains ~needle:"ciede2000" "has ciede2000" result

let test_compare_elements_color_invalid () =
  let args = args_of [
    ("type", "color"); ("color1", "notacolor"); ("color2", "#FF0000");
  ] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Invalid color format" "invalid color" result

let test_compare_elements_color_missing () =
  let args = args_of [("type", "color"); ("color1", "#FF0000")] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Missing color1 or color2" "missing color" result

let test_compare_elements_color_both_missing () =
  let args = args_of [("type", "color")] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Missing color1 or color2" "both missing" result

(* ============== 2. handle_compare_elements — box ============== *)

let test_compare_elements_box_ok () =
  let args = args_of [
    ("type", "box"); ("box1", "0,0,100,100"); ("box2", "10,10,100,100");
  ] in
  let result = handle_compare_elements args in
  check_ok "box comparison" result;
  check_ok_contains ~needle:"iou_value" "has iou" result

let test_compare_elements_box_invalid () =
  let args = args_of [
    ("type", "box"); ("box1", "not,a,box,format"); ("box2", "0,0,100,100");
  ] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Invalid box format" "invalid box" result

let test_compare_elements_box_missing () =
  let args = args_of [("type", "box"); ("box1", "0,0,100,100")] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Missing box1 or box2" "missing box2" result

let test_compare_elements_box_wrong_parts () =
  let args = args_of [
    ("type", "box"); ("box1", "0,0,100"); ("box2", "0,0,100,100");
  ] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Invalid box format" "3 parts" result

(* ============== 3. handle_compare_elements — full ============== *)

let test_compare_elements_full_both () =
  let args = args_of [
    ("type", "full");
    ("color1", "#FF0000"); ("color2", "#0000FF");
    ("box1", "0,0,100,100"); ("box2", "50,50,100,100");
  ] in
  let result = handle_compare_elements args in
  check_ok "full with both" result;
  check_ok_contains ~needle:"full" "type full" result

let test_compare_elements_full_color_only () =
  let args = args_of [
    ("type", "full");
    ("color1", "#FF0000"); ("color2", "#00FF00");
  ] in
  let result = handle_compare_elements args in
  check_ok "full color only" result

let test_compare_elements_full_box_only () =
  let args = args_of [
    ("type", "full");
    ("box1", "0,0,200,200"); ("box2", "0,0,100,100");
  ] in
  let result = handle_compare_elements args in
  check_ok "full box only" result

let test_compare_elements_full_neither () =
  let args = args_of [("type", "full")] in
  let result = handle_compare_elements args in
  check_ok "full neither" result

let test_compare_elements_full_invalid_color () =
  let args = args_of [
    ("type", "full");
    ("color1", "xyz"); ("color2", "abc");
    ("box1", "0,0,100,100"); ("box2", "0,0,100,100");
  ] in
  let result = handle_compare_elements args in
  check_ok "full invalid color falls to null" result

let test_compare_elements_full_invalid_box () =
  let args = args_of [
    ("type", "full");
    ("color1", "#FF0000"); ("color2", "#00FF00");
    ("box1", "invalid"); ("box2", "0,0,100,100");
  ] in
  let result = handle_compare_elements args in
  check_ok "full invalid box falls to null" result

(* ============== 4. handle_compare_elements — invalid type ============== *)

let test_compare_elements_invalid_type () =
  let args = args_of [("type", "unknown")] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Invalid type" "unknown type" result

let test_compare_elements_no_type () =
  let args = `Assoc [] in
  let result = handle_compare_elements args in
  check_error_contains ~needle:"Invalid type" "missing type" result

(* ============== 5. handle_evolution_report ============== *)

let test_evolution_report_no_run_dir () =
  (* When run_dir is None, should list recent runs *)
  let args = `Assoc [] in
  let result = handle_evolution_report args in
  check_ok "no run_dir lists runs" result;
  check_ok_contains ~needle:"recent_runs" "has recent_runs" result

let test_evolution_report_missing_dir () =
  let args = args_of [("run_dir", "/tmp/nonexistent-dir-coverage-test-12345")] in
  let result = handle_evolution_report args in
  check_error_contains ~needle:"not found" "missing dir" result

let test_evolution_report_real_dir () =
  (* Create a temp dir to test the existing dir path *)
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "figma-evolution-test-w4" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let args = args_with [
    ("run_dir", `String dir);
    ("generate_image", `Bool false);
  ] in
  let result = handle_evolution_report args in
  check_ok "existing dir" result;
  check_ok_contains ~needle:"run_dir" "has run_dir" result;
  check_ok_contains ~needle:"step_count" "has step_count" result;
  (* Cleanup *)
  (try Unix.rmdir dir with _ -> ())

let test_evolution_report_with_html_dir () =
  (* Create dir structure with html subdir and a .html file *)
  let base = Filename.concat (Filename.get_temp_dir_name ()) "figma-evolution-test-w4-html" in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let html_dir = Filename.concat base "html" in
  (try Unix.mkdir html_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let html_file = Filename.concat html_dir "step_001.html" in
  let oc = open_out html_file in
  output_string oc "<html></html>";
  close_out oc;
  let args = args_with [
    ("run_dir", `String base);
    ("generate_image", `Bool false);
  ] in
  let result = handle_evolution_report args in
  check_ok "dir with html" result;
  check_ok_contains ~needle:"step_001.html" "has step file" result;
  (* Cleanup *)
  (try Sys.remove html_file with _ -> ());
  (try Unix.rmdir html_dir with _ -> ());
  (try Unix.rmdir base with _ -> ())

(* ============== 6. handle_compare — mode dispatch ============== *)

let test_compare_mode_elements () =
  let args = args_of [
    ("mode", "elements"); ("type", "color");
    ("color1", "#FF0000"); ("color2", "#00FF00");
  ] in
  let result = handle_compare args in
  check_ok "compare mode=elements" result

let test_compare_mode_evolution () =
  let args = args_with [
    ("mode", `String "evolution");
  ] in
  let result = handle_compare args in
  check_ok "compare mode=evolution" result

let test_compare_mode_general_missing () =
  let args = args_of [("mode", "general")] in
  let result = handle_compare args in
  check_error "compare mode=general no params" result

let test_compare_mode_batch_missing () =
  let args = args_of [("mode", "batch")] in
  let result = handle_compare args in
  check_error "compare mode=batch no params" result

let test_compare_mode_default_missing () =
  (* No mode specified => defaults to general *)
  let args = `Assoc [] in
  let result = handle_compare args in
  check_error "compare default mode no params" result

(* ============== 7. handle_image_similarity — missing args ============== *)

let test_image_similarity_missing_all () =
  let result = handle_image_similarity (`Assoc []) in
  check_error "image_similarity missing all" result

let test_image_similarity_missing_node_b () =
  let result = handle_image_similarity (args_of [
    ("file_key", "abc"); ("node_a_id", "1:2");
  ]) in
  check_error "image_similarity missing node_b" result

(* ============== 8. handle_verify_visual — missing args ============== *)

let test_verify_visual_missing_all () =
  let result = handle_verify_visual (`Assoc []) in
  check_error "verify_visual missing all" result

(* ============== 9. handle_verify_semantic — missing args ============== *)

let test_verify_semantic_missing_all () =
  let result = handle_verify_semantic (`Assoc []) in
  check_error "verify_semantic missing all" result

(* ============== 10. handle_compare_regions — missing args ============== *)

let test_compare_regions_missing_all () =
  let result = handle_compare_regions (`Assoc []) in
  check_error "compare_regions missing all" result

(* ============== Test Suite ============== *)

let compare_elements_tests = [
  ("color hex", `Quick, test_compare_elements_color_hex);
  ("color rgb", `Quick, test_compare_elements_color_rgb);
  ("color invalid", `Quick, test_compare_elements_color_invalid);
  ("color missing", `Quick, test_compare_elements_color_missing);
  ("color both missing", `Quick, test_compare_elements_color_both_missing);
  ("box ok", `Quick, test_compare_elements_box_ok);
  ("box invalid", `Quick, test_compare_elements_box_invalid);
  ("box missing", `Quick, test_compare_elements_box_missing);
  ("box wrong parts", `Quick, test_compare_elements_box_wrong_parts);
  ("full both", `Quick, test_compare_elements_full_both);
  ("full color only", `Quick, test_compare_elements_full_color_only);
  ("full box only", `Quick, test_compare_elements_full_box_only);
  ("full neither", `Quick, test_compare_elements_full_neither);
  ("full invalid color", `Quick, test_compare_elements_full_invalid_color);
  ("full invalid box", `Quick, test_compare_elements_full_invalid_box);
  ("invalid type", `Quick, test_compare_elements_invalid_type);
  ("no type", `Quick, test_compare_elements_no_type);
]

let evolution_report_tests = [
  ("no run_dir", `Quick, test_evolution_report_no_run_dir);
  ("missing dir", `Quick, test_evolution_report_missing_dir);
  ("real dir", `Quick, test_evolution_report_real_dir);
  ("with html dir", `Quick, test_evolution_report_with_html_dir);
]

let compare_tests = [
  ("mode elements", `Quick, test_compare_mode_elements);
  ("mode evolution", `Quick, test_compare_mode_evolution);
  ("mode general missing", `Quick, test_compare_mode_general_missing);
  ("mode batch missing", `Quick, test_compare_mode_batch_missing);
  ("mode default missing", `Quick, test_compare_mode_default_missing);
]

let image_similarity_tests = [
  ("missing all", `Quick, test_image_similarity_missing_all);
  ("missing node_b", `Quick, test_image_similarity_missing_node_b);
]

let verify_tests = [
  ("verify visual missing", `Quick, test_verify_visual_missing_all);
  ("verify semantic missing", `Quick, test_verify_semantic_missing_all);
  ("compare regions missing", `Quick, test_compare_regions_missing_all);
]

let () =
  Alcotest.run "Visual Handlers W4" [
    ("compare_elements", compare_elements_tests);
    ("evolution_report", evolution_report_tests);
    ("compare", compare_tests);
    ("image_similarity", image_similarity_tests);
    ("verify", verify_tests);
  ]
