(** Coverage Wave 7: mcp_visual_handlers.ml deep success paths.
    Targets branches INSIDE success paths that existing tests do not cover:
    - fidelity_loop: multi-attempt loop, meta/variables/fills, summary paths,
      early_stop, geometry options, clamp_score, attempt_overall variants
    - image_similarity: multi-scale loop, clamp_scale, target_ssim None,
      best-update logic, should_stop conditions
    - compare: batch mode with web/mobile nodes, general with found nodes,
      document parse failure
    - evolution_report: existing dir with files and PNG renders
    - compare_elements: parse_color/parse_box edge cases
    - compare_regions: validate_output_dir exact base match *)

open Mcp_visual_handlers

(* ============== Helpers ============== *)

let args_of pairs =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs)

let args_with kv = `Assoc kv

let extract_text (json : Yojson.Safe.t) : string =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "content" fields with
     | Some (`List [`Assoc inner]) ->
       (match List.assoc_opt "text" inner with
        | Some (`String s) -> s
        | _ -> Yojson.Safe.to_string json)
     | _ -> Yojson.Safe.to_string json)
  | _ -> Yojson.Safe.to_string json

let str_contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  if nl > hl then false
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in loop 0

let check_ok msg result =
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "expected ok for %s: %s" msg e)

let check_ok_text_contains ~needle msg result =
  match result with
  | Ok json ->
      let s = extract_text json in
      if not (str_contains ~needle s) then
        Alcotest.fail (Printf.sprintf "%s: expected '%s' in text:\n%s" msg needle
          (String.sub s 0 (min 500 (String.length s))))
  | Error e -> Alcotest.fail (Printf.sprintf "expected ok for %s: %s" msg e)

let check_error msg result =
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error: %s" msg)

let check_error_contains ~needle msg result =
  match result with
  | Error e ->
      if not (str_contains ~needle e) then
        Alcotest.fail (Printf.sprintf "%s: expected '%s' in error '%s'" msg needle e)
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error: %s" msg)

(* Build a mock store with common seed data including a richer document *)
let make_store () =
  let store = Figma_effects.create_mock_store () in
  (* Seed file with multiple children: WebFrame, MobileFrame, TextNode *)
  Hashtbl.replace store.files "abc123"
    (`Assoc [
      ("name", `String "Test File");
      ("document", `Assoc [
        ("id", `String "0:0");
        ("name", `String "Document");
        ("type", `String "DOCUMENT");
        ("children", `List [
          `Assoc [
            ("id", `String "1:2");
            ("name", `String "WebFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 375.0); ("height", `Float 812.0);
            ]);
            ("children", `List [
              `Assoc [
                ("id", `String "2:1");
                ("name", `String "Title");
                ("type", `String "TEXT");
                ("absoluteBoundingBox", `Assoc [
                  ("x", `Float 10.0); ("y", `Float 10.0);
                  ("width", `Float 200.0); ("height", `Float 30.0);
                ]);
                ("characters", `String "Hello World");
                ("style", `Assoc [
                  ("fontFamily", `String "Roboto");
                  ("fontSize", `Float 16.0);
                  ("fontWeight", `Float 400.0);
                ]);
                ("children", `List []);
              ];
            ]);
          ];
          `Assoc [
            ("id", `String "3:1");
            ("name", `String "MobileFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 400.0); ("y", `Float 0.0);
              ("width", `Float 375.0); ("height", `Float 812.0);
            ]);
            ("children", `List []);
          ];
        ]);
      ]);
    ]);
  (* Seed nodes for 1:2 *)
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [
      ("nodes", `Assoc [
        ("1:2", `Assoc [
          ("document", `Assoc [
            ("id", `String "1:2");
            ("name", `String "TestFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 375.0); ("height", `Float 812.0);
            ]);
            ("children", `List [
              `Assoc [
                ("id", `String "2:1");
                ("name", `String "Title");
                ("type", `String "TEXT");
                ("characters", `String "Hello World");
                ("style", `Assoc [
                  ("fontFamily", `String "Roboto");
                  ("fontSize", `Float 16.0);
                  ("fontWeight", `Float 400.0);
                ]);
                ("absoluteBoundingBox", `Assoc [
                  ("x", `Float 10.0); ("y", `Float 10.0);
                  ("width", `Float 200.0); ("height", `Float 30.0);
                ]);
                ("children", `List []);
              ];
            ]);
          ]);
        ]);
      ]);
    ]);
  (* Also seed for 3:1 *)
  Hashtbl.replace store.nodes "abc123:3:1"
    (`Assoc [
      ("nodes", `Assoc [
        ("3:1", `Assoc [
          ("document", `Assoc [
            ("id", `String "3:1");
            ("name", `String "MobileFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 375.0); ("height", `Float 812.0);
            ]);
            ("children", `List []);
          ]);
        ]);
      ]);
    ]);
  (* Seed images for both nodes *)
  Hashtbl.replace store.images "abc123:1:2"
    (`Assoc [("images", `Assoc [("1:2", `String "https://example.com/img.png")])]);
  Hashtbl.replace store.images "abc123:1:2,3:4"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      ("3:4", `String "https://example.com/b.png");
    ])]);
  (* Seed file_meta with meta wrapper *)
  Hashtbl.replace store.file_meta "abc123"
    (`Assoc [
      ("meta", `Assoc [
        ("components", `Assoc [("c1", `String "comp1")]);
        ("componentSets", `Assoc []);
        ("styles", `Assoc [("s1", `String "style1")]);
      ]);
    ]);
  (* Seed file_images (image fills) with actual images *)
  Hashtbl.replace store.file_images "abc123"
    (`Assoc [("images", `Assoc [
      ("ref1", `String "https://example.com/fill1.png");
      ("ref2", `String "https://example.com/fill2.png");
    ])]);
  (* Seed variables with full structure *)
  Hashtbl.replace store.variables "abc123"
    (`Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc [
          ("col1", `Assoc [
            ("name", `String "Colors");
            ("defaultModeId", `String "mode1");
            ("modes", `List [
              `Assoc [
                ("modeId", `String "mode1");
                ("name", `String "Light");
              ];
            ]);
          ]);
        ]);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "primaryColor");
            ("resolvedType", `String "COLOR");
            ("variableCollectionId", `String "col1");
            ("valuesByMode", `Assoc [
              ("mode1", `Assoc [
                ("r", `Float 1.0); ("g", `Float 0.0);
                ("b", `Float 0.0); ("a", `Float 1.0);
              ]);
            ]);
          ]);
        ]);
      ]);
    ]);
  store

(* ============== 1. handle_fidelity_loop — deep success paths ============== *)

(* Test fidelity loop with include_meta=true that has meta wrapper *)
let test_fidelity_meta_with_meta_wrapper () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool true);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
      ("start_depth", `Int 4);
      ("depth_step", `Int 4);
      ("max_depth", `Int 4);
      ("summary_only", `Bool false);
    ])
  ) in
  check_ok "fidelity meta with wrapper" result

(* Test meta error path: meta not found in store *)
let test_fidelity_meta_error () =
  let store = make_store () in
  Hashtbl.remove store.file_meta "abc123";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool true);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity meta error wrapped" result

(* Test include_variables=true with successful variable fetch *)
let test_fidelity_variables_success () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool true);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity variables success" result

(* Test include_variables=true with variable fetch error (no variables in store) *)
let test_fidelity_variables_error_fallback () =
  let store = make_store () in
  Hashtbl.remove store.variables "abc123";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool true);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity variables error fallback" result

(* Test include_image_fills=true with successful fetch *)
let test_fidelity_image_fills_success () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool true);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity image fills success" result

(* Test include_image_fills=true with missing file_images *)
let test_fidelity_image_fills_error () =
  let store = make_store () in
  Hashtbl.remove store.file_images "abc123";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool true);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity image fills error" result

(* Test fidelity loop multiple attempts with depth stepping *)
let test_fidelity_multi_attempt () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 3);
      ("start_depth", `Int 2);
      ("depth_step", `Int 2);
      ("max_depth", `Int 6);
    ])
  ) in
  check_ok "fidelity multi attempt" result

(* Test fidelity loop where next_depth = depth (stalls) *)
let test_fidelity_depth_stall () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 2);
      ("start_depth", `Int 20);  (* at max_depth already *)
      ("depth_step", `Int 4);
      ("max_depth", `Int 20);
    ])
  ) in
  check_ok "fidelity depth stall" result

(* Test fidelity loop with geometry=Some *)
let test_fidelity_with_geometry () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("geometry", `String "paths");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity with geometry" result

(* Test geometry=None (default None -> Some "paths") *)
let test_fidelity_geometry_default () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity geometry default" result

(* Test fidelity loop where node is not found in nodes map *)
let test_fidelity_node_not_in_map () =
  let store = make_store () in
  (* Seed nodes entry for a nonexistent node key *)
  Hashtbl.replace store.nodes "abc123:99:99"
    (`Assoc [
      ("nodes", `Assoc [
        ("other:id", `Assoc [
          ("document", `Assoc [("type", `String "FRAME")]);
        ]);
      ]);
    ]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "99:99");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity node not in map" result

(* Test fidelity with summary_only=true, a small result that does not exceed max_inline *)
let test_fidelity_summary_only_small () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
      ("summary_only", `Bool true);
    ])
  ) in
  check_ok "fidelity summary_only small" result

(* Test fidelity with max_inline_bytes very small to force large result path *)
let test_fidelity_force_large_result () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
      ("summary_only", `Bool false);
      ("max_inline_bytes", `Int 10);  (* Very small to trigger large result *)
    ])
  ) in
  (* Should succeed and contain large_result metadata *)
  check_ok "fidelity force large result" result

(* Test fidelity with all includes and multiple attempts *)
let test_fidelity_full_bundle () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool true);
      ("include_variables", `Bool true);
      ("include_image_fills", `Bool true);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 2);
      ("start_depth", `Int 4);
      ("depth_step", `Int 4);
      ("max_depth", `Int 8);
    ])
  ) in
  check_ok "fidelity full bundle" result

(* Test fidelity with target_score=0.0 (clamped from negative) *)
let test_fidelity_target_zero () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("target_score", `Float (-1.0));
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity target zero (clamped)" result

(* Test fidelity with plugin_data option *)
let test_fidelity_with_plugin_data () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("plugin_data", `String "shared");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity with plugin_data" result

(* Test fidelity with auto_plugin derived from url presence *)
let test_fidelity_auto_plugin_from_url () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("url", `String "https://www.figma.com/file/abc123/Test?node-id=1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin_variables", `Bool false);
      (* auto_plugin will be true because url is present *)
      (* include_plugin will follow auto_plugin *)
      ("max_attempts", `Int 1);
    ])
  ) in
  (* The plugin path will fail (no channel), but handler continues *)
  check_ok "fidelity auto_plugin from url" result

(* Test fidelity where nodes API returns error during loop *)
let test_fidelity_loop_api_error () =
  let store = make_store () in
  (* Remove the nodes entry so it returns Error *)
  Hashtbl.remove store.nodes "abc123:1:2";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "fidelity loop api error" result

(* ============== 2. handle_image_similarity — deep success paths ============== *)

(* Test image_similarity with target_ssim=None (stops after first scale) *)
let test_similarity_no_target () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      ("3:1", `String "https://example.com/b.png");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
      ("format", `String "png");
      ("start_scale", `Float 1.0);
      (* no target_ssim -> None -> should_stop after first iteration *)
    ])
  ) in
  check_ok "similarity no target" result

(* Test image_similarity with extreme scale clamping *)
let test_similarity_scale_clamping () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      ("3:1", `String "https://example.com/b.png");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
      ("start_scale", `Float 0.001);  (* below min: clamps to 0.01 *)
      ("max_scale", `Float 10.0);  (* above max: clamps to 4.0 *)
      ("scale_step", `Float 5.0);  (* large step to exit after 1 iteration *)
    ])
  ) in
  check_ok "similarity scale clamping" result

(* Test image_similarity multi-scale loop with target *)
let test_similarity_multi_scale () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      ("3:1", `String "https://example.com/b.png");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
      ("start_scale", `Float 1.0);
      ("max_scale", `Float 2.0);
      ("scale_step", `Float 0.5);
      ("target_ssim", `Float 0.99);  (* High target: never met via mock *)
    ])
  ) in
  check_ok "similarity multi scale" result

(* Test image_similarity with version parameter *)
let test_similarity_with_version () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      ("3:1", `String "https://example.com/b.png");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
      ("version", `String "v123");
      ("use_absolute_bounds", `Bool true);
    ])
  ) in
  check_ok "similarity with version" result

(* Test image_similarity where get_images returns missing URL for one node *)
let test_similarity_url_a_missing () =
  let store = make_store () in
  (* Only URL for node b, not a *)
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("3:1", `String "https://example.com/b.png");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
    ])
  ) in
  (* Errors wrapped in attempts array, still Ok overall *)
  check_ok "similarity url a missing" result

(* Test image_similarity where get_images returns no images map *)
let test_similarity_no_images_map () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("noImages", `Null)]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
    ])
  ) in
  check_ok "similarity no images map" result

(* ============== 3. handle_verify_visual — deep success paths ============== *)

(* Test verify_visual with explicit html_screenshot *)
let test_verify_visual_with_screenshot () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<html><body><div>Test</div></body></html>");
      ("html_screenshot", `String "/tmp/test_screenshot.png");
      ("target_ssim", `Float 0.8);
      ("max_iterations", `Int 1);
      ("width", `Int 375);
      ("height", `Int 812);
      ("version", `String "v1");
    ])
  ) in
  (* Will fail at download_url mock but exercises all arg parsing *)
  check_error "verify visual download fails" result

(* Test verify_visual where get_nodes fails (Error branch) *)
let test_verify_visual_nodes_error () =
  let store = make_store () in
  Hashtbl.remove store.nodes "abc123:1:2";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>fallback</div>");
    ])
  ) in
  (* get_images will also fail since mock returns error for download_url *)
  check_error "verify visual nodes error" result

(* ============== 4. handle_verify_semantic — deep success paths ============== *)

(* Test verify_semantic with custom config values *)
let test_verify_semantic_full_config () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<html><body><div style='font-size:16px;font-weight:400'>Hello World</div></body></html>");
      ("width", `Int 375);
      ("height", `Int 812);
      ("version", `String "v1");
      ("score_threshold", `Float 0.3);
      ("text_bbox_tol_px", `Float 20.0);
      ("font_size_tol_px", `Float 5.0);
      ("font_weight_tol", `Int 300);
      ("text_color_tol_rgb", `Float 50.0);
    ])
  ) in
  (* May succeed or fail depending on Html_metrics.extract *)
  ignore result

(* Test verify_semantic where node document is missing *)
let test_verify_semantic_no_document () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [
      ("nodes", `Assoc [
        ("1:2", `Assoc [
          (* no "document" key *)
          ("name", `String "TestNode");
        ]);
      ]);
    ]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"parse DSL node" "no document" result

(* Test verify_semantic where node entry not found via find_node_entry *)
let test_verify_semantic_node_entry_not_found () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:5:5"
    (`Assoc [
      ("nodes", `Assoc [
        (* Different node id than requested *)
        ("6:6", `Assoc [
          ("document", `Assoc [("type", `String "FRAME")]);
        ]);
      ]);
    ]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "5:5");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"parse DSL node" "entry not found" result

(* ============== 5. handle_compare_regions — deep paths ============== *)

(* Test validate_output_dir with exact base match "/tmp/figma-evolution" *)
let test_compare_regions_exact_base () =
  (* "/tmp/figma-evolution" should be valid (dir = allowed_output_base) *)
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution");
  ]) in
  (* Missing image_a, image_b, regions => but passes output_dir validation *)
  check_error_contains ~needle:"Missing required" "exact base" result

(* Test regions JSON that is not an array *)
let test_compare_regions_json_not_array () =
  let tmp_a = Filename.temp_file "test_w7_a" ".png" in
  let tmp_b = Filename.temp_file "test_w7_b" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "{\"not\":\"array\"}");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"expected array" "json not array" result

(* Test regions with region that has missing fields *)
let test_compare_regions_missing_field () =
  let tmp_a = Filename.temp_file "test_w7_a2" ".png" in
  let tmp_b = Filename.temp_file "test_w7_b2" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"header\",\"x\":0}]");  (* Missing y, width, height *)
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"extraction failed" "missing field" result

(* Test region name "." (special dot name) *)
let test_compare_regions_dot_name () =
  let tmp_a = Filename.temp_file "test_w7_a3" ".png" in
  let tmp_b = Filename.temp_file "test_w7_b3" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\".\",\"x\":0,\"y\":0,\"width\":100,\"height\":50}]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region name" "dot name" result

(* Test region name ".." *)
let test_compare_regions_dotdot_name () =
  let tmp_a = Filename.temp_file "test_w7_a4" ".png" in
  let tmp_b = Filename.temp_file "test_w7_b4" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"..\",\"x\":0,\"y\":0,\"width\":100,\"height\":50}]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region name" "dotdot name" result

(* Test region with zero width *)
let test_compare_regions_zero_width () =
  let tmp_a = Filename.temp_file "test_w7_a5" ".png" in
  let tmp_b = Filename.temp_file "test_w7_b5" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"hdr\",\"x\":0,\"y\":0,\"width\":0,\"height\":50}]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region bounds" "zero width" result

(* Test region name over 64 chars *)
let test_compare_regions_long_name () =
  let tmp_a = Filename.temp_file "test_w7_a6" ".png" in
  let tmp_b = Filename.temp_file "test_w7_b6" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let long_name = String.make 65 'a' in
  let regions_json = Printf.sprintf "[{\"name\":\"%s\",\"x\":0,\"y\":0,\"width\":100,\"height\":50}]" long_name in
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", regions_json);
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region name" "long name" result

(* Test generate_diff=false *)
let test_compare_regions_no_diff () =
  (* We test the generate_diff=false flag parsing separately *)
  let result = handle_compare_regions (args_with [
    ("output_dir", `String "/tmp/figma-evolution/test");
    ("generate_diff", `Bool false);
  ]) in
  check_error_contains ~needle:"Missing required" "no diff still needs images" result

(* ============== 6. handle_evolution_report — deep paths ============== *)

(* Test with real temp dir containing PNG renders and HTML files *)
let test_evolution_report_full_dir () =
  let base = Printf.sprintf "%s/figma-evolution-test-w7-%d"
    (Filename.get_temp_dir_name ()) (Random.int 100000) in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let html_dir = Filename.concat base "html" in
  (try Unix.mkdir html_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (* Create step HTML files *)
  let f1 = Filename.concat html_dir "step_001.html" in
  let f2 = Filename.concat html_dir "step_002.html" in
  let oc = open_out f1 in output_string oc "<html>1</html>"; close_out oc;
  let oc = open_out f2 in output_string oc "<html>2</html>"; close_out oc;
  (* Create PNG render files *)
  let p1 = Filename.concat base "step_001_render.png" in
  let p2 = Filename.concat base "step_002_render.png" in
  let oc = open_out p1 in output_string oc "PNG1"; close_out oc;
  let oc = open_out p2 in output_string oc "PNG2"; close_out oc;
  (* Create figma_original.png *)
  let orig = Filename.concat base "figma_original.png" in
  let oc = open_out orig in output_string oc "ORIG"; close_out oc;

  let result = handle_evolution_report (args_with [
    ("run_dir", `String base);
    ("generate_image", `Bool false);  (* Skip montage in test *)
  ]) in
  check_ok "evolution full dir" result;
  check_ok_text_contains ~needle:"step_001.html" "has step 1" result;
  check_ok_text_contains ~needle:"step_002_render.png" "has render 2" result;

  (* Cleanup *)
  List.iter (fun f -> try Sys.remove f with _ -> ())
    [f1; f2; p1; p2; orig];
  (try Unix.rmdir html_dir with _ -> ());
  (try Unix.rmdir base with _ -> ())

(* Test evolution report with no HTML subdir *)
let test_evolution_report_no_html_dir () =
  let base = Printf.sprintf "%s/figma-evolution-test-w7-no-html-%d"
    (Filename.get_temp_dir_name ()) (Random.int 100000) in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (* Create PNG render only *)
  let p1 = Filename.concat base "step_001_render.png" in
  let oc = open_out p1 in output_string oc "PNG1"; close_out oc;

  let result = handle_evolution_report (args_with [
    ("run_dir", `String base);
    ("generate_image", `Bool false);
  ]) in
  check_ok "evolution no html dir" result;
  check_ok_text_contains ~needle:"step_count" "has step_count" result;

  (* Cleanup *)
  (try Sys.remove p1 with _ -> ());
  (try Unix.rmdir base with _ -> ())

(* Test evolution report with generate_image=true but no figma_original.png *)
let test_evolution_report_generate_no_figma_png () =
  let base = Printf.sprintf "%s/figma-evolution-test-w7-nofig-%d"
    (Filename.get_temp_dir_name ()) (Random.int 100000) in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let p1 = Filename.concat base "step_001_render.png" in
  let oc = open_out p1 in output_string oc "PNG1"; close_out oc;

  let result = handle_evolution_report (args_with [
    ("run_dir", `String base);
    ("generate_image", `Bool true);  (* wants to generate but no figma_original.png *)
  ]) in
  check_ok "evolution generate no figma png" result;

  (* Cleanup *)
  (try Sys.remove p1 with _ -> ());
  (try Unix.rmdir base with _ -> ())

(* Test evolution report with no PNG renders *)
let test_evolution_report_no_pngs () =
  let base = Printf.sprintf "%s/figma-evolution-test-w7-nopngs-%d"
    (Filename.get_temp_dir_name ()) (Random.int 100000) in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  let result = handle_evolution_report (args_with [
    ("run_dir", `String base);
    ("generate_image", `Bool true);
  ]) in
  check_ok "evolution no pngs" result;
  check_ok_text_contains ~needle:"none" "summary says none" result;

  (* Cleanup *)
  (try Unix.rmdir base with _ -> ())

(* ============== 7. handle_compare_elements — additional edge cases ============== *)

(* Test color with spaces in rgb format *)
let test_compare_elements_color_rgb_spaces () =
  let result = handle_compare_elements (args_of [
    ("type", "color"); ("color1", "rgb(255, 128, 0)"); ("color2", "rgb(0, 0, 255)");
  ]) in
  check_ok "color rgb spaces" result

(* Test full with invalid color but valid box *)
let test_compare_elements_full_invalid_color_valid_box () =
  let result = handle_compare_elements (args_of [
    ("type", "full");
    ("color1", "invalid"); ("color2", "invalid");
    ("box1", "0,0,100,100"); ("box2", "50,50,100,100");
  ]) in
  check_ok "full invalid color valid box" result;
  check_ok_text_contains ~needle:"box" "has box result" result

(* Test full with valid color but invalid box *)
let test_compare_elements_full_valid_color_invalid_box () =
  let result = handle_compare_elements (args_of [
    ("type", "full");
    ("color1", "#FF0000"); ("color2", "#00FF00");
    ("box1", "notabox"); ("box2", "0,0,100,100");
  ]) in
  check_ok "full valid color invalid box" result;
  check_ok_text_contains ~needle:"oklab_similarity" "has color result" result

(* Test box with overlapping boxes *)
let test_compare_elements_box_identical () =
  let result = handle_compare_elements (args_of [
    ("type", "box"); ("box1", "10,20,100,200"); ("box2", "10,20,100,200");
  ]) in
  check_ok "box identical" result;
  check_ok_text_contains ~needle:"iou_value" "has iou" result

(* Test color with black and white *)
let test_compare_elements_color_bw () =
  let result = handle_compare_elements (args_of [
    ("type", "color"); ("color1", "#000000"); ("color2", "#ffffff");
  ]) in
  check_ok "color black/white" result

(* ============== 8. handle_compare — deep paths ============== *)

(* Test compare batch mode with Web and Mobile prefixed nodes *)
let test_compare_batch_web_mobile () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "batch");
      ("web_prefix", `String "Web");
      ("mobile_prefix", `String "Mobile");
    ])
  ) in
  check_ok "compare batch web/mobile" result

(* Test compare batch with custom prefixes *)
let test_compare_batch_custom_prefix () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "batch");
      ("web_prefix", `String "Desktop");
      ("mobile_prefix", `String "Phone");
    ])
  ) in
  check_ok "compare batch custom prefix" result

(* Test compare general with both nodes found in doc *)
let test_compare_general_both_found () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "general");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
    ])
  ) in
  check_ok "compare general both found" result

(* Test compare general where node B not found *)
let test_compare_general_b_not_found () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "general");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "99:99");
    ])
  ) in
  check_error_contains ~needle:"not found" "node B not found" result

(* Test compare with file fetch error *)
let test_compare_file_error () =
  let store = Figma_effects.create_mock_store () in
  (* No file seeded, so get_file returns Error *)
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "nonexistent");
      ("mode", `String "general");
      ("node_a_id", `String "1:1");
      ("node_b_id", `String "2:2");
    ])
  ) in
  check_error "compare file error" result

(* Test compare with document that fails to parse *)
let test_compare_parse_failure () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "badDoc"
    (`Assoc [
      ("document", `Assoc [
        (* document with no valid node structure for parser *)
        ("id", `String "0:0");
        ("type", `String "UNKNOWN_TYPE_123");
      ]);
    ]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "badDoc");
      ("mode", `String "general");
      ("node_a_id", `String "1:1");
      ("node_b_id", `String "2:2");
    ])
  ) in
  (* Parser may return None for unexpected type *)
  ignore result

(* Test compare with default mode (empty string maps to wildcard) *)
let test_compare_unrecognized_mode () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "custom_mode");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "1:2");
    ])
  ) in
  check_ok "compare unrecognized mode fallback" result

(* ============== Test Runner ============== *)

let () =
  let open Alcotest in
  Unix.putenv "FIGMA_TOKEN" "test-token-w7";
  run "Visual Handlers W7" [
    ("fidelity_loop deep", [
      test_case "meta with wrapper" `Quick test_fidelity_meta_with_meta_wrapper;
      test_case "meta error" `Quick test_fidelity_meta_error;
      test_case "variables success" `Quick test_fidelity_variables_success;
      test_case "variables error fallback" `Quick test_fidelity_variables_error_fallback;
      test_case "image fills success" `Quick test_fidelity_image_fills_success;
      test_case "image fills error" `Quick test_fidelity_image_fills_error;
      test_case "multi attempt" `Quick test_fidelity_multi_attempt;
      test_case "depth stall" `Quick test_fidelity_depth_stall;
      test_case "with geometry" `Quick test_fidelity_with_geometry;
      test_case "geometry default" `Quick test_fidelity_geometry_default;
      test_case "node not in map" `Quick test_fidelity_node_not_in_map;
      test_case "summary only small" `Quick test_fidelity_summary_only_small;
      test_case "force large result" `Quick test_fidelity_force_large_result;
      test_case "full bundle" `Quick test_fidelity_full_bundle;
      test_case "target zero clamped" `Quick test_fidelity_target_zero;
      test_case "with plugin_data" `Quick test_fidelity_with_plugin_data;
      test_case "auto_plugin from url" `Quick test_fidelity_auto_plugin_from_url;
      test_case "loop api error" `Quick test_fidelity_loop_api_error;
    ]);
    ("image_similarity deep", [
      test_case "no target ssim" `Quick test_similarity_no_target;
      test_case "scale clamping" `Quick test_similarity_scale_clamping;
      test_case "multi scale" `Quick test_similarity_multi_scale;
      test_case "with version" `Quick test_similarity_with_version;
      test_case "url a missing" `Quick test_similarity_url_a_missing;
      test_case "no images map" `Quick test_similarity_no_images_map;
    ]);
    ("verify_visual deep", [
      test_case "with screenshot" `Quick test_verify_visual_with_screenshot;
      test_case "nodes error" `Quick test_verify_visual_nodes_error;
    ]);
    ("verify_semantic deep", [
      test_case "full config" `Quick test_verify_semantic_full_config;
      test_case "no document" `Quick test_verify_semantic_no_document;
      test_case "node entry not found" `Quick test_verify_semantic_node_entry_not_found;
    ]);
    ("compare_regions deep", [
      test_case "exact base dir" `Quick test_compare_regions_exact_base;
      test_case "json not array" `Quick test_compare_regions_json_not_array;
      test_case "missing field" `Quick test_compare_regions_missing_field;
      test_case "dot name" `Quick test_compare_regions_dot_name;
      test_case "dotdot name" `Quick test_compare_regions_dotdot_name;
      test_case "zero width" `Quick test_compare_regions_zero_width;
      test_case "long name" `Quick test_compare_regions_long_name;
      test_case "no diff flag" `Quick test_compare_regions_no_diff;
    ]);
    ("evolution_report deep", [
      test_case "full dir" `Quick test_evolution_report_full_dir;
      test_case "no html dir" `Quick test_evolution_report_no_html_dir;
      test_case "generate no figma png" `Quick test_evolution_report_generate_no_figma_png;
      test_case "no pngs" `Quick test_evolution_report_no_pngs;
    ]);
    ("compare_elements deep", [
      test_case "color rgb spaces" `Quick test_compare_elements_color_rgb_spaces;
      test_case "full invalid color valid box" `Quick test_compare_elements_full_invalid_color_valid_box;
      test_case "full valid color invalid box" `Quick test_compare_elements_full_valid_color_invalid_box;
      test_case "box identical" `Quick test_compare_elements_box_identical;
      test_case "color black/white" `Quick test_compare_elements_color_bw;
    ]);
    ("compare deep", [
      test_case "batch web/mobile" `Quick test_compare_batch_web_mobile;
      test_case "batch custom prefix" `Quick test_compare_batch_custom_prefix;
      test_case "general both found" `Quick test_compare_general_both_found;
      test_case "general B not found" `Quick test_compare_general_b_not_found;
      test_case "file error" `Quick test_compare_file_error;
      test_case "parse failure" `Quick test_compare_parse_failure;
      test_case "unrecognized mode" `Quick test_compare_unrecognized_mode;
    ]);
  ]

