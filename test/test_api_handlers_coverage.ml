(** Coverage tests for mcp_api_handlers.ml — pure node-selection helpers.
    Targets 0/1071 coverage points: normalize_patterns, string_contains,
    matches_any, find_matching_pattern, node_text_blob, node_is_text,
    node_is_container, node_is_component, node_has_image_fill, node_area,
    node_area_score, node_has_auto_layout, node_has_mask_hint, node_duplicate_key,
    default_exclude_patterns, default_note_patterns *)

let () =
  let open Alcotest in
  let open Figma_types in

  let make_node ?(id="n1") ?(name="Node") ?(node_type=Frame)
      ?(visible=true) ?(children=[]) ?(characters=None)
      ?(fills=[]) ?(bbox=None) ?(layout_mode=None') () =
    { default_node with id; name; node_type; visible; children;
      characters; fills; bbox; layout_mode }
  in

  let make_bbox w h = Some { x = 0.; y = 0.; width = w; height = h } in
  let make_paint ?(paint_type=Solid) ?(visible=true) ?(opacity=1.0) () =
    { paint_type; visible; opacity; color = None;
      gradient_stops = []; image_ref = None; scale_mode = None }
  in

  (* ============== default patterns ============== *)
  let test_exclude_patterns_nonempty () =
    check bool "not empty" true (List.length Mcp_api_handlers.default_exclude_patterns > 0)
  in
  let test_note_patterns_nonempty () =
    check bool "not empty" true (List.length Mcp_api_handlers.default_note_patterns > 0)
  in
  let test_exclude_patterns_has_guide () =
    check bool "has guide" true
      (List.mem "guide" Mcp_api_handlers.default_exclude_patterns)
  in
  let test_note_patterns_has_memo () =
    check bool "has memo" true
      (List.mem "memo" Mcp_api_handlers.default_note_patterns)
  in

  (* ============== normalize_patterns ============== *)
  let test_normalize_basic () =
    let result = Mcp_api_handlers.normalize_patterns ["  hello  "; "world"] in
    check (list string) "trimmed" ["hello"; "world"] result
  in
  let test_normalize_empty_filter () =
    let result = Mcp_api_handlers.normalize_patterns ["a"; ""; "  "; "b"] in
    check (list string) "no empties" ["a"; "b"] result
  in
  let test_normalize_empty_input () =
    let result = Mcp_api_handlers.normalize_patterns [] in
    check (list string) "empty" [] result
  in

  (* ============== string_contains ============== *)
  let test_string_contains_basic () =
    check bool "found" true
      (Mcp_api_handlers.string_contains ~haystack:"say hello world" ~needle:"hello")
  in
  let test_string_contains_case () =
    check bool "case insensitive" true
      (Mcp_api_handlers.string_contains ~haystack:"hello world" ~needle:"HELLO")
  in
  let test_string_contains_not_found () =
    check bool "not found" false
      (Mcp_api_handlers.string_contains ~haystack:"hello" ~needle:"xyz")
  in
  let test_string_contains_empty_needle () =
    check bool "empty needle" false
      (Mcp_api_handlers.string_contains ~haystack:"hello" ~needle:"")
  in
  let test_string_contains_empty_haystack () =
    check bool "empty haystack" false
      (Mcp_api_handlers.string_contains ~haystack:"" ~needle:"a")
  in
  let test_string_contains_needle_trim () =
    check bool "needle trimmed" true
      (Mcp_api_handlers.string_contains ~haystack:"hello world" ~needle:"  hello  ")
  in

  (* ============== matches_any ============== *)
  let test_matches_any_found () =
    check bool "matches" true
      (Mcp_api_handlers.matches_any ["cat"; "dog"] "the dog barked")
  in
  let test_matches_any_not_found () =
    check bool "no match" false
      (Mcp_api_handlers.matches_any ["cat"; "dog"] "the bird sang")
  in
  let test_matches_any_empty_patterns () =
    check bool "empty patterns" false
      (Mcp_api_handlers.matches_any [] "hello")
  in

  (* ============== find_matching_pattern ============== *)
  let test_find_matching_some () =
    check (option string) "found" (Some "dog")
      (Mcp_api_handlers.find_matching_pattern ["cat"; "dog"] "the dog barked")
  in
  let test_find_matching_none () =
    check (option string) "not found" None
      (Mcp_api_handlers.find_matching_pattern ["cat"; "dog"] "the bird")
  in
  let test_find_matching_first () =
    (* If both match, returns first pattern *)
    let result = Mcp_api_handlers.find_matching_pattern ["a"; "b"] "ab" in
    check (option string) "first" (Some "a") result
  in

  (* ============== node_text_blob ============== *)
  let test_text_blob_no_chars () =
    let node = make_node ~name:"Button" () in
    check string "name only" "Button" (Mcp_api_handlers.node_text_blob node)
  in
  let test_text_blob_with_chars () =
    let node = make_node ~name:"Label" ~characters:(Some "Hello") () in
    check string "name + chars" "Label Hello" (Mcp_api_handlers.node_text_blob node)
  in

  (* ============== node_is_text ============== *)
  let test_is_text_yes () =
    let node = make_node ~node_type:Text () in
    check bool "is text" true (Mcp_api_handlers.node_is_text node)
  in
  let test_is_text_no () =
    let node = make_node ~node_type:Frame () in
    check bool "not text" false (Mcp_api_handlers.node_is_text node)
  in

  (* ============== node_is_container ============== *)
  let test_is_container_frame () =
    check bool "frame" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Frame ()))
  in
  let test_is_container_group () =
    check bool "group" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Group ()))
  in
  let test_is_container_document () =
    check bool "document" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Document ()))
  in
  let test_is_container_canvas () =
    check bool "canvas" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Canvas ()))
  in
  let test_is_container_section () =
    check bool "section" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Section ()))
  in
  let test_is_container_component () =
    check bool "component" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Component ()))
  in
  let test_is_container_component_set () =
    check bool "component_set" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:ComponentSet ()))
  in
  let test_is_container_instance () =
    check bool "instance" true
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Instance ()))
  in
  let test_is_container_text () =
    check bool "text not container" false
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Text ()))
  in
  let test_is_container_rectangle () =
    check bool "rect not container" false
      (Mcp_api_handlers.node_is_container (make_node ~node_type:Rectangle ()))
  in

  (* ============== node_is_component ============== *)
  let test_is_component_yes () =
    check bool "component" true
      (Mcp_api_handlers.node_is_component (make_node ~node_type:Component ()))
  in
  let test_is_component_set () =
    check bool "component_set" true
      (Mcp_api_handlers.node_is_component (make_node ~node_type:ComponentSet ()))
  in
  let test_is_component_instance () =
    check bool "instance" true
      (Mcp_api_handlers.node_is_component (make_node ~node_type:Instance ()))
  in
  let test_is_component_frame () =
    check bool "frame not component" false
      (Mcp_api_handlers.node_is_component (make_node ~node_type:Frame ()))
  in

  (* ============== node_has_image_fill ============== *)
  let test_has_image_fill_yes () =
    let paint = make_paint ~paint_type:Image ~visible:true ~opacity:1.0 () in
    let node = make_node ~fills:[paint] () in
    check bool "has image" true (Mcp_api_handlers.node_has_image_fill node)
  in
  let test_has_image_fill_invisible () =
    let paint = make_paint ~paint_type:Image ~visible:false ~opacity:1.0 () in
    let node = make_node ~fills:[paint] () in
    check bool "invisible" false (Mcp_api_handlers.node_has_image_fill node)
  in
  let test_has_image_fill_low_opacity () =
    let paint = make_paint ~paint_type:Image ~visible:true ~opacity:0.001 () in
    let node = make_node ~fills:[paint] () in
    check bool "low opacity" false (Mcp_api_handlers.node_has_image_fill node)
  in
  let test_has_image_fill_solid () =
    let paint = make_paint ~paint_type:Solid ~visible:true ~opacity:1.0 () in
    let node = make_node ~fills:[paint] () in
    check bool "solid not image" false (Mcp_api_handlers.node_has_image_fill node)
  in
  let test_has_image_fill_empty () =
    let node = make_node ~fills:[] () in
    check bool "no fills" false (Mcp_api_handlers.node_has_image_fill node)
  in

  (* ============== node_area ============== *)
  let test_node_area_basic () =
    let node = make_node ~bbox:(make_bbox 100. 50.) () in
    check (float 0.001) "100x50" 5000. (Mcp_api_handlers.node_area node)
  in
  let test_node_area_no_bbox () =
    let node = make_node ~bbox:None () in
    check (float 0.001) "no bbox" 0. (Mcp_api_handlers.node_area node)
  in
  let test_node_area_zero () =
    let node = make_node ~bbox:(make_bbox 0. 100.) () in
    check (float 0.001) "zero width" 0. (Mcp_api_handlers.node_area node)
  in

  (* ============== node_area_score ============== *)
  let test_area_score_zero () =
    check (float 0.001) "log10(1)" 0. (Mcp_api_handlers.node_area_score 0.)
  in
  let test_area_score_100 () =
    let expected = Float.log10 101. in
    check (float 0.001) "log10(101)" expected (Mcp_api_handlers.node_area_score 100.)
  in

  (* ============== node_has_auto_layout ============== *)
  let test_auto_layout_none () =
    let node = make_node ~layout_mode:None' () in
    check bool "none" false (Mcp_api_handlers.node_has_auto_layout node)
  in
  let test_auto_layout_horizontal () =
    let node = make_node ~layout_mode:Horizontal () in
    check bool "horizontal" true (Mcp_api_handlers.node_has_auto_layout node)
  in
  let test_auto_layout_vertical () =
    let node = make_node ~layout_mode:Vertical () in
    check bool "vertical" true (Mcp_api_handlers.node_has_auto_layout node)
  in

  (* ============== node_has_mask_hint ============== *)
  let test_mask_hint_yes () =
    let node = make_node ~name:"icon-mask-layer" () in
    check bool "has mask" true (Mcp_api_handlers.node_has_mask_hint node)
  in
  let test_mask_hint_clip () =
    let node = make_node ~name:"Clip Region" () in
    check bool "has clip" true (Mcp_api_handlers.node_has_mask_hint node)
  in
  let test_mask_hint_no () =
    let node = make_node ~name:"Button" () in
    check bool "no hint" false (Mcp_api_handlers.node_has_mask_hint node)
  in
  let test_mask_hint_in_chars () =
    let node = make_node ~name:"Layer" ~characters:(Some "mask overlay") () in
    check bool "mask in chars" true (Mcp_api_handlers.node_has_mask_hint node)
  in

  (* ============== node_duplicate_key ============== *)
  let test_dup_key_with_bbox () =
    let node = make_node ~name:" Button " ~node_type:Frame ~bbox:(make_bbox 200. 44.) () in
    let key = Mcp_api_handlers.node_duplicate_key node in
    check bool "has pipe" true (String.contains key '|');
    check bool "has size" true (
      try ignore (Str.search_forward (Str.regexp_string "200x44") key 0); true
      with Not_found -> false)
  in
  let test_dup_key_no_bbox () =
    let node = make_node ~name:"Icon" ~node_type:Text ~bbox:None () in
    let key = Mcp_api_handlers.node_duplicate_key node in
    check bool "has ?" true (
      try ignore (Str.search_forward (Str.regexp_string "?") key 0); true
      with Not_found -> false)
  in
  let test_dup_key_case_insensitive () =
    let n1 = make_node ~name:"Button" ~node_type:Frame ~bbox:(make_bbox 100. 40.) () in
    let n2 = make_node ~name:"BUTTON" ~node_type:Frame ~bbox:(make_bbox 100. 40.) () in
    check string "same key" (Mcp_api_handlers.node_duplicate_key n1) (Mcp_api_handlers.node_duplicate_key n2)
  in

  run "Mcp_api_handlers Coverage" [
    ("default_patterns", [
      test_case "exclude nonempty" `Quick test_exclude_patterns_nonempty;
      test_case "note nonempty" `Quick test_note_patterns_nonempty;
      test_case "exclude has guide" `Quick test_exclude_patterns_has_guide;
      test_case "note has memo" `Quick test_note_patterns_has_memo;
    ]);
    ("normalize_patterns", [
      test_case "basic" `Quick test_normalize_basic;
      test_case "empty filter" `Quick test_normalize_empty_filter;
      test_case "empty input" `Quick test_normalize_empty_input;
    ]);
    ("string_contains", [
      test_case "basic" `Quick test_string_contains_basic;
      test_case "case insensitive" `Quick test_string_contains_case;
      test_case "not found" `Quick test_string_contains_not_found;
      test_case "empty needle" `Quick test_string_contains_empty_needle;
      test_case "empty haystack" `Quick test_string_contains_empty_haystack;
      test_case "needle trimmed" `Quick test_string_contains_needle_trim;
    ]);
    ("matches_any", [
      test_case "found" `Quick test_matches_any_found;
      test_case "not found" `Quick test_matches_any_not_found;
      test_case "empty patterns" `Quick test_matches_any_empty_patterns;
    ]);
    ("find_matching_pattern", [
      test_case "some" `Quick test_find_matching_some;
      test_case "none" `Quick test_find_matching_none;
      test_case "first" `Quick test_find_matching_first;
    ]);
    ("node_text_blob", [
      test_case "no chars" `Quick test_text_blob_no_chars;
      test_case "with chars" `Quick test_text_blob_with_chars;
    ]);
    ("node_is_text", [
      test_case "yes" `Quick test_is_text_yes;
      test_case "no" `Quick test_is_text_no;
    ]);
    ("node_is_container", [
      test_case "frame" `Quick test_is_container_frame;
      test_case "group" `Quick test_is_container_group;
      test_case "document" `Quick test_is_container_document;
      test_case "canvas" `Quick test_is_container_canvas;
      test_case "section" `Quick test_is_container_section;
      test_case "component" `Quick test_is_container_component;
      test_case "component_set" `Quick test_is_container_component_set;
      test_case "instance" `Quick test_is_container_instance;
      test_case "text" `Quick test_is_container_text;
      test_case "rectangle" `Quick test_is_container_rectangle;
    ]);
    ("node_is_component", [
      test_case "component" `Quick test_is_component_yes;
      test_case "component_set" `Quick test_is_component_set;
      test_case "instance" `Quick test_is_component_instance;
      test_case "frame" `Quick test_is_component_frame;
    ]);
    ("node_has_image_fill", [
      test_case "image" `Quick test_has_image_fill_yes;
      test_case "invisible" `Quick test_has_image_fill_invisible;
      test_case "low opacity" `Quick test_has_image_fill_low_opacity;
      test_case "solid" `Quick test_has_image_fill_solid;
      test_case "empty" `Quick test_has_image_fill_empty;
    ]);
    ("node_area", [
      test_case "basic" `Quick test_node_area_basic;
      test_case "no bbox" `Quick test_node_area_no_bbox;
      test_case "zero" `Quick test_node_area_zero;
    ]);
    ("node_area_score", [
      test_case "zero" `Quick test_area_score_zero;
      test_case "100" `Quick test_area_score_100;
    ]);
    ("node_has_auto_layout", [
      test_case "none" `Quick test_auto_layout_none;
      test_case "horizontal" `Quick test_auto_layout_horizontal;
      test_case "vertical" `Quick test_auto_layout_vertical;
    ]);
    ("node_has_mask_hint", [
      test_case "mask" `Quick test_mask_hint_yes;
      test_case "clip" `Quick test_mask_hint_clip;
      test_case "no hint" `Quick test_mask_hint_no;
      test_case "in chars" `Quick test_mask_hint_in_chars;
    ]);
    ("node_duplicate_key", [
      test_case "with bbox" `Quick test_dup_key_with_bbox;
      test_case "no bbox" `Quick test_dup_key_no_bbox;
      test_case "case insensitive" `Quick test_dup_key_case_insensitive;
    ]);
  ]
