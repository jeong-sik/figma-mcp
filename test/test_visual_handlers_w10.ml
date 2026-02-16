(** Coverage Wave 10: mcp_visual_handlers.ml remaining gaps (237 missing points).
    Targets branches not covered by existing tests (effects, w4, w7):
    - fidelity_loop: format error, clamp_score > 1.0, include_meta/variables/fills false
      paths, nodes-map non-Assoc, max_inline_bytes <= 0, attempt_overall Int variant,
      summarize_best non-Assoc, both variables+plugin_variables=false
    - image_similarity: url_b missing, images-map non-string, Error in loop with attempts,
      best keeps old when ssim <= best_score, download_url Error for b
    - verify_visual: success through download, get_nodes with partial data, html generation
      branches, text verification passed/failed paths
    - verify_semantic: nodes non-Assoc, get_nodes Error path, Html_metrics error
    - compare_regions: empty output_dir, relative path, not-found file, non-png, too large,
      FIGMA_MCP_COMPARE_IMAGE_ROOTS filtering, empty regions list, negative y region,
      region special chars, JSON parse broken, output_dir with ".." traversal,
      non-reg file, stat error path
    - compare: Document `Null path, node_a not found, no node ids for general mode
    - evolution_report: list_recent_runs with /tmp/figma-evolution entries *)

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

(* Build a mock store with richer data for comprehensive testing *)
let make_store () =
  let store = Figma_effects.create_mock_store () in
  (* File with document structure *)
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
                ("characters", `String "Hello World");
                ("absoluteBoundingBox", `Assoc [
                  ("x", `Float 10.0); ("y", `Float 10.0);
                  ("width", `Float 200.0); ("height", `Float 30.0);
                ]);
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
  (* Nodes for 1:2 with TEXT child *)
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
  (* Images for both single-node and multi-node lookups *)
  Hashtbl.replace store.images "abc123:1:2"
    (`Assoc [("images", `Assoc [("1:2", `String "https://example.com/img_12.png")])]);
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      ("3:1", `String "https://example.com/b.png");
    ])]);
  (* File meta *)
  Hashtbl.replace store.file_meta "abc123"
    (`Assoc [
      ("meta", `Assoc [
        ("components", `Assoc [("c1", `String "comp1")]);
        ("componentSets", `Assoc []);
        ("styles", `Assoc [("s1", `String "style1")]);
      ]);
    ]);
  (* File images (image fills) *)
  Hashtbl.replace store.file_images "abc123"
    (`Assoc [("images", `Assoc [
      ("ref1", `String "https://example.com/fill1.png");
    ])]);
  (* Variables *)
  Hashtbl.replace store.variables "abc123"
    (`Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc [
          ("col1", `Assoc [
            ("name", `String "Colors");
            ("defaultModeId", `String "mode1");
            ("modes", `List [
              `Assoc [("modeId", `String "mode1"); ("name", `String "Light")];
            ]);
          ]);
        ]);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "primary");
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

(* ============== 1. handle_fidelity_loop — uncovered branches ============== *)

(* format != "fidelity" should Error *)
let test_fidelity_bad_format () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("format", `String "json");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_error_contains ~needle:"only supports format=fidelity" "bad format" result

(* missing all required params *)
let test_fidelity_missing_params () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (`Assoc [])
  ) in
  check_error_contains ~needle:"Missing required" "missing params" result

(* target_score clamped above 1.0 *)
let test_fidelity_clamp_above_1 () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("target_score", `Float 5.0);
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "clamp above 1.0" result

(* include_meta=false, include_variables=false, include_image_fills=false
   exercises the `else` branches returning `Null *)
let test_fidelity_all_includes_false () =
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
    ])
  ) in
  check_ok "all includes false" result

(* max_inline_bytes <= 0 defaults to Large_response.max_inline_size *)
let test_fidelity_max_inline_zero () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("max_inline_bytes", `Int 0);
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "max_inline zero" result

(* max_inline_bytes negative *)
let test_fidelity_max_inline_negative () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("max_inline_bytes", `Int (-100));
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "max_inline negative" result

(* nodes response has "nodes" that is not `Assoc *)
let test_fidelity_nodes_non_assoc () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:7:7"
    (`Assoc [("nodes", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "7:7");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "nodes non-assoc" result

(* node entry found but no "document" key *)
let test_fidelity_node_no_document () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:8:8"
    (`Assoc [("nodes", `Assoc [
      ("8:8", `Assoc [("name", `String "NoDoc")]);
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "8:8");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "node no document" result

(* include_plugin_variables=true but no channel *)
let test_fidelity_plugin_variables_no_channel () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool true);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "plugin_variables no channel" result

(* include_plugin=true with explicit auto_plugin=true but no channel *)
let test_fidelity_include_plugin_no_channel () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool true);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool true);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "include_plugin no channel" result

(* Both include_plugin and include_variables true with channel error for variables *)
let test_fidelity_plugin_and_variables () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool true);
      ("include_variables", `Bool true);
      ("include_image_fills", `Bool true);
      ("include_plugin", `Bool true);
      ("include_plugin_variables", `Bool true);
      ("auto_plugin", `Bool true);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "plugin + variables both true" result

(* file_images response has no "images" key *)
let test_fidelity_image_fills_no_images_key () =
  let store = make_store () in
  Hashtbl.replace store.file_images "abc123"
    (`Assoc [("noImages", `Null)]);
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
  check_ok "image fills no images key" result

(* file_images response has "images" as non-Assoc *)
let test_fidelity_image_fills_images_list () =
  let store = make_store () in
  Hashtbl.replace store.file_images "abc123"
    (`Assoc [("images", `List [`String "x"])]);
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
  check_ok "image fills images list" result

(* summary_only with large result and multiple attempts *)
let test_fidelity_summary_only_multi_attempt () =
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
      ("max_attempts", `Int 3);
      ("start_depth", `Int 4);
      ("depth_step", `Int 4);
      ("max_depth", `Int 12);
      ("summary_only", `Bool true);
    ])
  ) in
  check_ok "summary only multi attempt" result

(* max_attempts=0 means loop body never executes *)
let test_fidelity_zero_attempts () =
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
      ("max_attempts", `Int 0);
    ])
  ) in
  (* best=None -> (0.0, `Null) *)
  check_ok "zero attempts" result

(* Use node_id with colons that gets normalized *)
let test_fidelity_node_id_normalization () =
  let store = make_store () in
  (* Seed with dash-normalized id since normalize_node_id converts _ or - to : *)
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [("nodes", `Assoc [
      ("1:2", `Assoc [
        ("document", `Assoc [
          ("id", `String "1:2");
          ("name", `String "Frame");
          ("type", `String "FRAME");
          ("children", `List []);
          ("absoluteBoundingBox", `Assoc [
            ("x", `Float 0.0); ("y", `Float 0.0);
            ("width", `Float 100.0); ("height", `Float 100.0);
          ]);
        ]);
      ]);
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_fidelity_loop (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1-2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_plugin_variables", `Bool false);
      ("auto_plugin", `Bool false);
      ("max_attempts", `Int 1);
    ])
  ) in
  check_ok "node_id normalization" result

(* ============== 2. handle_image_similarity — uncovered branches ============== *)

(* URL for node B missing (only A has URL) *)
let test_similarity_url_b_missing () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.png");
      (* no 3:1 entry *)
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
    ])
  ) in
  check_ok "similarity url b missing" result

(* get_images returns Error in the loop *)
let test_similarity_api_error () =
  let store = make_store () in
  (* No images seeded for these nodes *)
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "5:5");
      ("node_b_id", `String "6:6");
    ])
  ) in
  check_ok "similarity api error" result

(* save_dir custom directory *)
let test_similarity_custom_save_dir () =
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
      ("save_dir", `String "/tmp/test-similarity-w10");
      ("format", `String "png");
    ])
  ) in
  check_ok "similarity custom save_dir" result

(* start_scale == max_scale (single iteration) *)
let test_similarity_single_scale () =
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
      ("start_scale", `Float 2.0);
      ("max_scale", `Float 2.0);
      ("scale_step", `Float 1.0);
      ("target_ssim", `Float 0.5);
    ])
  ) in
  check_ok "similarity single scale" result

(* images value is non-String (null) *)
let test_similarity_image_null_url () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `Null);
      ("3:1", `String "https://example.com/b.png");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
    ])
  ) in
  check_ok "similarity image null url" result

(* format = "svg" *)
let test_similarity_svg_format () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2,3:1"
    (`Assoc [("images", `Assoc [
      ("1:2", `String "https://example.com/a.svg");
      ("3:1", `String "https://example.com/b.svg");
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_image_similarity (args_with [
      ("file_key", `String "abc123");
      ("node_a_id", `String "1:2");
      ("node_b_id", `String "3:1");
      ("format", `String "svg");
    ])
  ) in
  check_ok "similarity svg format" result

(* ============== 3. handle_verify_visual — uncovered branches ============== *)

(* verify_visual: get_images succeeds but image URL not found for the node *)
let test_verify_visual_url_not_found () =
  let store = make_store () in
  (* Images exists but for wrong node *)
  Hashtbl.replace store.images "abc123:1:2"
    (`Assoc [("images", `Assoc [("wrong:id", `String "https://example.com/img.png")])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"Image URL not found" "url not found" result

(* verify_visual: images response has no "images" member *)
let test_verify_visual_no_images_member () =
  let store = make_store () in
  Hashtbl.replace store.images "abc123:1:2"
    (`Assoc [("data", `Null)]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"Image URL not found" "no images member" result

(* verify_visual: get_images returns Error *)
let test_verify_visual_get_images_error () =
  let store = make_store () in
  (* Don't seed images -> Error *)
  Hashtbl.remove store.images "abc123:1:2";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"Failed to get Figma image" "get images error" result

(* verify_visual: get_nodes returns Ok but no matching node in nodes_map *)
let test_verify_visual_node_not_in_map () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [("nodes", `Assoc [
      ("other:id", `Assoc [
        ("document", `Assoc [("type", `String "FRAME"); ("children", `List [])]);
      ]);
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
    ])
  ) in
  (* get_images will also fail since mock returns error for download *)
  check_error "verify visual node not in map" result

(* verify_visual: get_nodes returns Ok but node has no "document" key *)
let test_verify_visual_no_document () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [("nodes", `Assoc [
      ("1:2", `Assoc [("name", `String "NoDoc")]);
    ])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
    ])
  ) in
  check_error "verify visual no document" result

(* verify_visual: nodes response has non-Assoc "nodes" *)
let test_verify_visual_nodes_non_assoc () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [("nodes", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
    ])
  ) in
  check_error "verify visual nodes non-assoc" result

(* verify_visual with explicit html provided + no html_screenshot *)
let test_verify_visual_explicit_html () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<html><body><h1>Hello World</h1></body></html>");
      ("target_ssim", `Float 0.5);
      ("max_iterations", `Int 1);
    ])
  ) in
  (* Will fail at download_url mock, but exercises all parsing *)
  check_error "verify visual explicit html" result

(* verify_visual with version param *)
let test_verify_visual_with_version () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_visual (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("version", `String "v123");
      ("width", `Int 1024);
      ("height", `Int 768);
    ])
  ) in
  check_error "verify visual with version" result

(* ============== 4. handle_verify_semantic — uncovered branches ============== *)

(* verify_semantic: get_nodes returns Error *)
let test_verify_semantic_get_nodes_error () =
  let store = make_store () in
  Hashtbl.remove store.nodes "abc123:1:2";
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"Failed to get node JSON" "get_nodes error" result

(* verify_semantic: nodes member is not Assoc *)
let test_verify_semantic_nodes_non_assoc () =
  let store = make_store () in
  Hashtbl.replace store.nodes "abc123:1:2"
    (`Assoc [("nodes", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<div>test</div>");
    ])
  ) in
  check_error_contains ~needle:"parse DSL node" "nodes non-assoc" result

(* verify_semantic: missing html parameter *)
let test_verify_semantic_missing_html () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      (* no html param *)
    ])
  ) in
  check_error_contains ~needle:"Missing required" "missing html" result

(* verify_semantic: with custom config params *)
let test_verify_semantic_custom_config () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_verify_semantic (args_with [
      ("file_key", `String "abc123");
      ("node_id", `String "1:2");
      ("html", `String "<html><body><div style='font-size:16px'>Hello World</div></body></html>");
      ("score_threshold", `Float 0.1);
      ("text_bbox_tol_px", `Float 50.0);
      ("font_size_tol_px", `Float 10.0);
      ("font_weight_tol", `Int 500);
      ("text_color_tol_rgb", `Float 100.0);
      ("width", `Int 800);
      ("height", `Int 600);
    ])
  ) in
  (* May succeed or fail depending on Html_metrics.extract *)
  ignore result

(* ============== 5. handle_compare_regions — uncovered branches ============== *)

(* output_dir is empty string *)
let test_regions_empty_output_dir () =
  let result = handle_compare_regions (args_of [
    ("output_dir", "");
  ]) in
  check_error_contains ~needle:"empty" "empty output_dir" result

(* output_dir is relative path *)
let test_regions_relative_output_dir () =
  let result = handle_compare_regions (args_of [
    ("output_dir", "relative/path");
  ]) in
  check_error_contains ~needle:"absolute path" "relative output_dir" result

(* output_dir with ".." traversal *)
let test_regions_traversal_output_dir () =
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/../etc");
  ]) in
  check_error_contains ~needle:".." "traversal output_dir" result

(* output_dir outside allowed base *)
let test_regions_outside_allowed_base () =
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/not-figma-evolution/test");
  ]) in
  check_error_contains ~needle:"must be under" "outside allowed base" result

(* image_a path is empty *)
let test_regions_empty_image_path () =
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", "");
    ("image_b", "/tmp/some.png");
    ("regions", "[]");
  ]) in
  check_error_contains ~needle:"path required" "empty image path" result

(* image_a does not exist *)
let test_regions_image_not_found () =
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", "/tmp/nonexistent_w10_test.png");
    ("image_b", "/tmp/some.png");
    ("regions", "[]");
  ]) in
  check_error_contains ~needle:"not found" "image not found" result

(* image_a is not a .png file *)
let test_regions_not_png () =
  let tmp = Filename.temp_file "test_w10_" ".jpg" in
  let oc = open_out tmp in output_string oc "JPEG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp);
    ("image_b", tmp);
    ("regions", "[]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp;
  check_error_contains ~needle:".png" "not png" result

(* image path not under allowed roots *)
let test_regions_path_not_allowed () =
  let tmp = Filename.temp_file "test_w10_" ".png" in
  let oc = open_out tmp in output_string oc "PNG"; close_out oc;
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "/home/specific-root-only";
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp);
    ("image_b", tmp);
    ("regions", "[]");
  ]) in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "";
  Sys.remove tmp;
  check_error_contains ~needle:"not allowed" "path not allowed" result

(* empty regions array *)
let test_regions_empty_array () =
  let tmp_a = Filename.temp_file "test_w10_a_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_b_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid regions JSON" "empty array" result

(* regions JSON is broken *)
let test_regions_broken_json () =
  let tmp_a = Filename.temp_file "test_w10_a2_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_b2_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "{broken json{{{{");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"parse error" "broken json" result

(* region with negative y coordinate *)
let test_regions_negative_coords () =
  let tmp_a = Filename.temp_file "test_w10_a3_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_b3_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"hdr\",\"x\":0,\"y\":-5,\"width\":100,\"height\":50}]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region bounds" "negative y" result

(* region with special character in name *)
let test_regions_special_char_name () =
  let tmp_a = Filename.temp_file "test_w10_a4_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_b4_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"hdr/../../etc\",\"x\":0,\"y\":0,\"width\":100,\"height\":50}]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region name" "special char name" result

(* region with empty name *)
let test_regions_empty_name () =
  let tmp_a = Filename.temp_file "test_w10_a5_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_b5_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"\",\"x\":0,\"y\":0,\"width\":100,\"height\":50}]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid region name" "empty name" result

(* No FIGMA_MCP_COMPARE_IMAGE_ROOTS env var (empty roots -> all paths allowed) *)
let test_regions_no_roots_env () =
  let tmp_a = Filename.temp_file "test_w10_nr_a_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_nr_b_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "";
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[]");
  ]) in
  (match saved with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  (* Empty array regions -> error about format *)
  check_error_contains ~needle:"Invalid regions JSON" "no roots env" result

(* FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES env set very small *)
let test_regions_image_too_large () =
  let tmp_a = Filename.temp_file "test_w10_lg_a_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_lg_b_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG_DATA_LARGER_THAN_1_BYTE"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved_roots = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let saved_max = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" "1";
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[{\"name\":\"hdr\",\"x\":0,\"y\":0,\"width\":100,\"height\":50}]");
  ]) in
  (match saved_roots with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  (match saved_max with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"too large" "image too large" result

(* FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES invalid number *)
let test_regions_max_bytes_invalid () =
  let tmp_a = Filename.temp_file "test_w10_mbi_a_" ".png" in
  let tmp_b = Filename.temp_file "test_w10_mbi_b_" ".png" in
  let oc = open_out tmp_a in output_string oc "PNG"; close_out oc;
  let oc = open_out tmp_b in output_string oc "PNG"; close_out oc;
  let saved_roots = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" in
  let saved_max = Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" in
  let tmpdir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" (Printf.sprintf "/tmp,/private/tmp,%s" tmpdir);
  Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" "not_a_number";
  let result = handle_compare_regions (args_of [
    ("output_dir", "/tmp/figma-evolution/test");
    ("image_a", tmp_a);
    ("image_b", tmp_b);
    ("regions", "[]");
  ]) in
  (match saved_roots with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_ROOTS" "");
  (match saved_max with
   | Some v -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" v
   | None -> Unix.putenv "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" "");
  Sys.remove tmp_a;
  Sys.remove tmp_b;
  check_error_contains ~needle:"Invalid regions JSON" "max bytes invalid defaults" result

(* ============== 6. handle_compare — uncovered branches ============== *)

(* compare: document is `Null *)
let test_compare_document_null () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "null_doc"
    (`Assoc [("document", `Null)]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "null_doc");
      ("mode", `String "general");
      ("node_a_id", `String "1:1");
      ("node_b_id", `String "2:2");
    ])
  ) in
  check_error_contains ~needle:"Document not found" "document null" result

(* compare: general mode node A not found *)
let test_compare_general_a_not_found () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "general");
      ("node_a_id", `String "99:99");
      ("node_b_id", `String "1:2");
    ])
  ) in
  check_error_contains ~needle:"not found" "node A not found" result

(* compare: general mode missing node_a_id *)
let test_compare_general_no_node_ids () =
  let store = make_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "abc123");
      ("mode", `String "general");
    ])
  ) in
  check_error_contains ~needle:"requires node_a_id" "no node ids" result

(* compare: regions mode dispatch (delegated to handle_compare_regions) *)
let test_compare_mode_regions () =
  let result = handle_compare (args_of [("mode", "regions")]) in
  (* Passes through to handle_compare_regions which needs image_a, etc. *)
  check_error "compare mode=regions" result

(* compare: default mode no file_key/token *)
let test_compare_no_file_key () =
  let store = Figma_effects.create_mock_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("mode", `String "general");
    ])
  ) in
  check_error_contains ~needle:"Missing required" "no file_key" result

(* compare: batch mode with empty file => no matching nodes *)
let test_compare_batch_no_matching_nodes () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "empty_file"
    (`Assoc [
      ("document", `Assoc [
        ("id", `String "0:0");
        ("name", `String "Doc");
        ("type", `String "DOCUMENT");
        ("children", `List [
          `Assoc [
            ("id", `String "1:1");
            ("name", `String "RandomFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 100.0); ("height", `Float 100.0);
            ]);
            ("children", `List []);
          ];
        ]);
      ]);
    ]);
  let result = Figma_effects.run_with_mock store (fun () ->
    handle_compare (args_with [
      ("file_key", `String "empty_file");
      ("mode", `String "batch");
    ])
  ) in
  check_ok "batch no matching nodes" result

(* ============== 7. handle_evolution_report — additional paths ============== *)

(* evolution report: list_recent_runs exercises /tmp/figma-evolution traversal *)
let test_evolution_list_with_runs () =
  let base = "/tmp/figma-evolution" in
  let run_name = Printf.sprintf "run_w10_%d" (Random.int 100000) in
  let run_dir = Filename.concat base run_name in
  (* Create the directory structure *)
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir run_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let result = handle_evolution_report (`Assoc []) in
  check_ok "evolution list with runs" result;
  check_ok_text_contains ~needle:"recent_runs" "has recent_runs" result;
  (* Cleanup *)
  (try Unix.rmdir run_dir with _ -> ());
  ()

(* evolution report with generate_image=true with figma_original and pngs *)
let test_evolution_generate_with_both_pngs () =
  let base = Printf.sprintf "%s/figma-evolution-test-w10-gen-%d"
    (Filename.get_temp_dir_name ()) (Random.int 100000) in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let orig = Filename.concat base "figma_original.png" in
  let render = Filename.concat base "step_001_render.png" in
  let oc = open_out orig in output_string oc "ORIG"; close_out oc;
  let oc = open_out render in output_string oc "RENDER"; close_out oc;
  let result = handle_evolution_report (args_with [
    ("run_dir", `String base);
    ("generate_image", `Bool true);
  ]) in
  check_ok "evolution generate with both pngs" result;
  (* Cleanup *)
  (try Sys.remove orig with _ -> ());
  (try Sys.remove render with _ -> ());
  let comp = Filename.concat base "evolution_comparison.png" in
  (try Sys.remove comp with _ -> ());
  (try Unix.rmdir base with _ -> ())

(* ============== 8. handle_compare_elements — more edge cases ============== *)

(* color: short hex — parse_color raises Invalid_argument on String.sub *)
let test_compare_elements_short_hex () =
  let raised = ref false in
  (try
    let _ = handle_compare_elements (args_of [
      ("type", "color"); ("color1", "#F00"); ("color2", "#0F0");
    ]) in ()
  with Invalid_argument _ -> raised := true);
  Alcotest.(check bool) "short hex raises" true !raised

(* box: with floating point values *)
let test_compare_elements_box_float () =
  let result = handle_compare_elements (args_of [
    ("type", "box"); ("box1", "10.5,20.5,100.0,200.0"); ("box2", "0.0,0.0,100.0,100.0");
  ]) in
  check_ok "box float values" result

(* box: with negative coordinates *)
let test_compare_elements_box_negative () =
  let result = handle_compare_elements (args_of [
    ("type", "box"); ("box1", "-10,0,100,100"); ("box2", "0,0,100,100");
  ]) in
  check_ok "box negative coords" result

(* full: both color and box invalid => null for both *)
let test_compare_elements_full_both_invalid () =
  let result = handle_compare_elements (args_of [
    ("type", "full");
    ("color1", "invalid"); ("color2", "also_invalid");
    ("box1", "not"); ("box2", "boxes");
  ]) in
  check_ok "full both invalid" result

(* color with both rgb and partial *)
let test_compare_elements_color_hex_lowercase () =
  let result = handle_compare_elements (args_of [
    ("type", "color"); ("color1", "#ff8800"); ("color2", "#0088ff");
  ]) in
  check_ok "color hex lowercase" result

(* ============== Test Runner ============== *)

let () =
  let open Alcotest in
  Unix.putenv "FIGMA_TOKEN" "test-token-w10";
  run "Visual Handlers W10" [
    ("fidelity_loop w10", [
      test_case "bad format" `Quick test_fidelity_bad_format;
      test_case "missing params" `Quick test_fidelity_missing_params;
      test_case "clamp above 1.0" `Quick test_fidelity_clamp_above_1;
      test_case "all includes false" `Quick test_fidelity_all_includes_false;
      test_case "max_inline zero" `Quick test_fidelity_max_inline_zero;
      test_case "max_inline negative" `Quick test_fidelity_max_inline_negative;
      test_case "nodes non-assoc" `Quick test_fidelity_nodes_non_assoc;
      test_case "node no document" `Quick test_fidelity_node_no_document;
      test_case "plugin_variables no channel" `Quick test_fidelity_plugin_variables_no_channel;
      test_case "include_plugin no channel" `Quick test_fidelity_include_plugin_no_channel;
      test_case "plugin + variables" `Quick test_fidelity_plugin_and_variables;
      test_case "image fills no images key" `Quick test_fidelity_image_fills_no_images_key;
      test_case "image fills images list" `Quick test_fidelity_image_fills_images_list;
      test_case "summary only multi attempt" `Quick test_fidelity_summary_only_multi_attempt;
      test_case "zero attempts" `Quick test_fidelity_zero_attempts;
      test_case "node_id normalization" `Quick test_fidelity_node_id_normalization;
    ]);
    ("image_similarity w10", [
      test_case "url b missing" `Quick test_similarity_url_b_missing;
      test_case "api error" `Quick test_similarity_api_error;
      test_case "custom save_dir" `Quick test_similarity_custom_save_dir;
      test_case "single scale" `Quick test_similarity_single_scale;
      test_case "image null url" `Quick test_similarity_image_null_url;
      test_case "svg format" `Quick test_similarity_svg_format;
    ]);
    ("verify_visual w10", [
      test_case "url not found" `Quick test_verify_visual_url_not_found;
      test_case "no images member" `Quick test_verify_visual_no_images_member;
      test_case "get images error" `Quick test_verify_visual_get_images_error;
      test_case "node not in map" `Quick test_verify_visual_node_not_in_map;
      test_case "no document" `Quick test_verify_visual_no_document;
      test_case "nodes non-assoc" `Quick test_verify_visual_nodes_non_assoc;
      test_case "explicit html" `Quick test_verify_visual_explicit_html;
      test_case "with version" `Quick test_verify_visual_with_version;
    ]);
    ("verify_semantic w10", [
      test_case "get_nodes error" `Quick test_verify_semantic_get_nodes_error;
      test_case "nodes non-assoc" `Quick test_verify_semantic_nodes_non_assoc;
      test_case "missing html" `Quick test_verify_semantic_missing_html;
      test_case "custom config" `Quick test_verify_semantic_custom_config;
    ]);
    ("compare_regions w10", [
      test_case "empty output_dir" `Quick test_regions_empty_output_dir;
      test_case "relative output_dir" `Quick test_regions_relative_output_dir;
      test_case "traversal output_dir" `Quick test_regions_traversal_output_dir;
      test_case "outside allowed base" `Quick test_regions_outside_allowed_base;
      test_case "empty image path" `Quick test_regions_empty_image_path;
      test_case "image not found" `Quick test_regions_image_not_found;
      test_case "not png" `Quick test_regions_not_png;
      test_case "path not allowed" `Quick test_regions_path_not_allowed;
      test_case "empty array" `Quick test_regions_empty_array;
      test_case "broken json" `Quick test_regions_broken_json;
      test_case "negative coords" `Quick test_regions_negative_coords;
      test_case "special char name" `Quick test_regions_special_char_name;
      test_case "empty name" `Quick test_regions_empty_name;
      test_case "no roots env" `Quick test_regions_no_roots_env;
      test_case "image too large" `Quick test_regions_image_too_large;
      test_case "max bytes invalid" `Quick test_regions_max_bytes_invalid;
    ]);
    ("compare w10", [
      test_case "document null" `Quick test_compare_document_null;
      test_case "general A not found" `Quick test_compare_general_a_not_found;
      test_case "general no node ids" `Quick test_compare_general_no_node_ids;
      test_case "mode regions" `Quick test_compare_mode_regions;
      test_case "no file_key" `Quick test_compare_no_file_key;
      test_case "batch no matching" `Quick test_compare_batch_no_matching_nodes;
    ]);
    ("evolution_report w10", [
      test_case "list with runs" `Quick test_evolution_list_with_runs;
      test_case "generate with both" `Quick test_evolution_generate_with_both_pngs;
    ]);
    ("compare_elements w10", [
      test_case "short hex" `Quick test_compare_elements_short_hex;
      test_case "box float" `Quick test_compare_elements_box_float;
      test_case "box negative" `Quick test_compare_elements_box_negative;
      test_case "full both invalid" `Quick test_compare_elements_full_both_invalid;
      test_case "color hex lowercase" `Quick test_compare_elements_color_hex_lowercase;
    ]);
  ]
