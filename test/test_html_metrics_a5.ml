(** Coverage A5: html_metrics.ml — remaining branch coverage.
    Targets uncovered lines: rgba_of_css malformed int/float paths (L96, L99),
    float_of_px exception path (L123), parse bg_node None branches (L234, L239),
    ensure_dir EEXIST (L41-42), parse with multiple bg_nodes, edge cases. *)

open Alcotest

let rgba_testable =
  testable
    (fun ppf opt ->
      match opt with
      | None -> Fmt.pf ppf "None"
      | Some (c : Figma_types.rgba) ->
          Fmt.pf ppf "Some {r=%.4f; g=%.4f; b=%.4f; a=%.4f}" c.r c.g c.b c.a)
    (fun a b ->
      match a, b with
      | None, None -> true
      | Some a, Some b ->
          Float.abs (a.r -. b.r) < 0.002
          && Float.abs (a.g -. b.g) < 0.002
          && Float.abs (a.b -. b.b) < 0.002
          && Float.abs (a.a -. b.a) < 0.002
      | _ -> false)

let float_opt_testable =
  testable
    (fun ppf opt ->
      match opt with
      | None -> Fmt.pf ppf "None"
      | Some f -> Fmt.pf ppf "Some %.4f" f)
    (fun a b ->
      match a, b with
      | None, None -> true
      | Some a, Some b -> Float.abs (a -. b) < 0.001
      | _ -> false)

let int_opt = option int

(* ============== rgba_of_css: malformed int values (L96) ============== *)

let test_css_rgb_malformed_ints () =
  (* "rgb(abc, def, ghi)" -> to_int catches exception, defaults to 0 *)
  let expected = Some Figma_types.{ r = 0.0; g = 0.0; b = 0.0; a = 1.0 } in
  check rgba_testable "malformed ints default to 0"
    expected (Html_metrics.rgba_of_css "rgb(abc, def, ghi)")

let test_css_rgb_partial_malformed () =
  (* "rgb(255, abc, 0)" -> second part malformed defaults to 0 *)
  let expected = Some Figma_types.{ r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  check rgba_testable "partial malformed"
    expected (Html_metrics.rgba_of_css "rgb(255, abc, 0)")

(* ============== rgba_of_css: malformed alpha (L99) ============== *)

let test_css_rgba_malformed_alpha () =
  (* "rgba(0, 0, 255, xyz)" -> to_float catches exception, defaults to 0.0 *)
  let expected = Some Figma_types.{ r = 0.0; g = 0.0; b = 1.0; a = 0.0 } in
  check rgba_testable "malformed alpha defaults to 0.0"
    expected (Html_metrics.rgba_of_css "rgba(0, 0, 255, xyz)")

let test_css_rgba_empty_alpha () =
  (* "rgba(128, 0, 0, )" -> to_float catches empty string, defaults to 0.0 *)
  let expected = Some Figma_types.{ r = 128.0 /. 255.0; g = 0.0; b = 0.0; a = 0.0 } in
  check rgba_testable "empty alpha defaults to 0.0"
    expected (Html_metrics.rgba_of_css "rgba(128, 0, 0, )")

(* ============== rgba_of_css: 5+ parts ============== *)

let test_css_rgb_too_many_parts () =
  (* "rgb(1, 2, 3, 4, 5)" -> 5 parts, falls to _ -> None *)
  check rgba_testable "too many parts" None
    (Html_metrics.rgba_of_css "rgb(1, 2, 3, 4, 5)")

let test_css_rgb_one_part () =
  check rgba_testable "single part" None
    (Html_metrics.rgba_of_css "rgb(255)")

(* ============== float_of_px: exception path (L123) ============== *)

let test_px_dots_only () =
  (* "..." -> take_num matches all dots, but float_of_string "..." raises -> None *)
  check float_opt_testable "dots only" None
    (Html_metrics.float_of_px "...")

let test_px_dot_px () =
  (* ".px" -> take_num gets ".", float_of_string "." may succeed or raise *)
  (* On OCaml, float_of_string "." raises Failure -> None *)
  check float_opt_testable "single dot" None
    (Html_metrics.float_of_px ".px")

let test_px_double_dot () =
  (* "1.2.3px" -> take_num gets "1.2.3", float_of_string fails -> None *)
  check float_opt_testable "double dot" None
    (Html_metrics.float_of_px "1.2.3px")

let test_px_minus_only () =
  (* "-px" -> take_num matches "-", float_of_string "-" raises -> None *)
  check float_opt_testable "minus only" None
    (Html_metrics.float_of_px "-px")

let test_px_zero () =
  check float_opt_testable "0px" (Some 0.0) (Html_metrics.float_of_px "0px")

(* ============== parse: bg_nodes with missing style keys (L234, L239) ============== *)

let test_parse_bg_node_no_background_key () =
  (* backgroundColor key absent -> to_string_option returns None -> background = None *)
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [],
    "bg_nodes": [{
      "bbox": {"x": 0.0, "y": 0.0, "width": 100.0, "height": 50.0},
      "styles": {}
    }]
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      check int "1 bg node" 1 (List.length m.bg_nodes);
      let bg = List.hd m.bg_nodes in
      check rgba_testable "no background" None bg.background;
      check float_opt_testable "no borderRadius" None bg.border_radius_px

let test_parse_bg_node_null_styles () =
  (* styles with explicit null values *)
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [],
    "bg_nodes": [{
      "bbox": {"x": 5.0, "y": 10.0, "width": 200.0, "height": 100.0},
      "styles": {"backgroundColor": null, "borderRadius": null}
    }]
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      let bg = List.hd m.bg_nodes in
      check rgba_testable "null bg" None bg.background;
      check float_opt_testable "null radius" None bg.border_radius_px

let test_parse_multiple_bg_nodes () =
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [],
    "bg_nodes": [
      {
        "bbox": {"x": 0.0, "y": 0.0, "width": 375.0, "height": 100.0},
        "styles": {"backgroundColor": "#00FF00", "borderRadius": "4px"}
      },
      {
        "bbox": {"x": 10.0, "y": 110.0, "width": 200.0, "height": 50.0},
        "styles": {"backgroundColor": "rgb(128, 128, 128)"}
      }
    ]
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      check int "2 bg nodes" 2 (List.length m.bg_nodes);
      let bg1 = List.nth m.bg_nodes 0 in
      let bg2 = List.nth m.bg_nodes 1 in
      check bool "bg1 has background" true (bg1.background <> None);
      check float_opt_testable "bg1 radius" (Some 4.0) bg1.border_radius_px;
      check bool "bg2 has background" true (bg2.background <> None);
      check float_opt_testable "bg2 no radius" None bg2.border_radius_px

(* ============== parse: text node with transparent color ============== *)

let test_parse_text_node_transparent_color () =
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [{
      "text": "Hidden",
      "bbox": {"x": 0.0, "y": 0.0, "width": 100.0, "height": 20.0},
      "styles": {"color": "transparent"}
    }],
    "bg_nodes": []
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      let tn = List.hd m.text_nodes in
      check rgba_testable "transparent color" None tn.color

(* ============== parse: text node with rgba color ============== *)

let test_parse_text_node_rgba_color () =
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [{
      "text": "Semi",
      "bbox": {"x": 0.0, "y": 0.0, "width": 100.0, "height": 20.0},
      "styles": {"color": "rgba(0, 128, 255, 0.75)"}
    }],
    "bg_nodes": []
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      let tn = List.hd m.text_nodes in
      check bool "has color" true (tn.color <> None);
      let c = Option.get tn.color in
      check bool "alpha ~0.75" true (Float.abs (c.a -. 0.75) < 0.01)

(* ============== parse: root with transparent background ============== *)

let test_parse_root_transparent_bg () =
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": {
      "bbox": {"x": 0.0, "y": 0.0, "width": 375.0, "height": 812.0},
      "styles": {"backgroundColor": "transparent", "borderRadius": "0px"}
    },
    "text_nodes": [],
    "bg_nodes": []
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      check bool "root present" true (m.root <> None);
      let root = Option.get m.root in
      check rgba_testable "transparent bg" None root.background;
      check float_opt_testable "border 0" (Some 0.0) root.border_radius_px

(* ============== ensure_dir: existing directory (EEXIST path L41-42) ============== *)

let test_ensure_dir_existing () =
  (* Call ensure_dir on a directory that already exists — no error *)
  Html_metrics.ensure_dir "/tmp";
  check bool "no error on existing dir" true true

let test_ensure_dir_new_then_again () =
  (* Create a temp dir, then call ensure_dir twice.
     Second call hits the Sys.file_exists=true early return. *)
  let dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "html_metrics_test_%d" (Unix.getpid ())) in
  (try Unix.rmdir dir with Unix.Unix_error _ -> ());
  Html_metrics.ensure_dir dir;
  check bool "dir created" true (Sys.file_exists dir);
  Html_metrics.ensure_dir dir;  (* second call: Sys.file_exists -> true, early return *)
  check bool "still exists" true (Sys.file_exists dir);
  (try Unix.rmdir dir with Unix.Unix_error _ -> ())

(* ============== rgba_of_hex: edge cases ============== *)

let test_hex_empty () =
  check rgba_testable "empty string" None (Html_metrics.rgba_of_hex "")

let test_hex_just_hash () =
  check rgba_testable "just hash" None (Html_metrics.rgba_of_hex "#")

let test_hex_lowercase () =
  let expected = Some Figma_types.{ r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  check rgba_testable "lowercase" expected (Html_metrics.rgba_of_hex "#ff0000")

let test_hex8_full_alpha () =
  let expected = Some Figma_types.{ r = 0.0; g = 0.0; b = 0.0; a = 1.0 } in
  check rgba_testable "8-char full alpha" expected (Html_metrics.rgba_of_hex "#000000FF")

let test_hex8_zero_alpha () =
  let expected = Some Figma_types.{ r = 1.0; g = 1.0; b = 1.0; a = 0.0 } in
  check rgba_testable "8-char zero alpha" expected (Html_metrics.rgba_of_hex "#FFFFFF00")

(* ============== int_of_string_opt: whitespace-padded valid ============== *)

let test_int_whitespace_padded () =
  check int_opt "padded 42" (Some 42) (Html_metrics.int_of_string_opt "  42  ")

let test_int_float_string () =
  check int_opt "float string" None (Html_metrics.int_of_string_opt "3.14")

(* ============== parse: fontWeight with null/missing ============== *)

let test_parse_text_null_weight () =
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [{
      "text": "NullWeight",
      "bbox": {"x": 0.0, "y": 0.0, "width": 50.0, "height": 20.0},
      "styles": {"fontWeight": null}
    }],
    "bg_nodes": []
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      let tn = List.hd m.text_nodes in
      check int_opt "null fontWeight" None tn.font_weight

let test_parse_text_bool_weight () =
  let json = Yojson.Safe.from_string {|{
    "viewport": {"width": 375, "height": 812},
    "root": null,
    "text_nodes": [{
      "text": "BoolWeight",
      "bbox": {"x": 0.0, "y": 0.0, "width": 50.0, "height": 20.0},
      "styles": {"fontWeight": true}
    }],
    "bg_nodes": []
  }|} in
  match Html_metrics.parse json with
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
  | Ok m ->
      let tn = List.hd m.text_nodes in
      (* true is not String, Int, or Float -> falls to _ -> None *)
      check int_opt "bool fontWeight" None tn.font_weight

(* ============== starts_with: equal strings ============== *)

let test_starts_with_exact () =
  check bool "exact match" true (Html_metrics.starts_with ~prefix:"abc" "abc")

let test_starts_with_empty_string () =
  check bool "empty string empty prefix" true (Html_metrics.starts_with ~prefix:"" "")

let () =
  run "html_metrics_a5"
    [ ("rgba_of_css_malformed", [
        test_case "malformed int values" `Quick test_css_rgb_malformed_ints;
        test_case "partial malformed" `Quick test_css_rgb_partial_malformed;
        test_case "malformed alpha" `Quick test_css_rgba_malformed_alpha;
        test_case "empty alpha" `Quick test_css_rgba_empty_alpha;
        test_case "too many parts" `Quick test_css_rgb_too_many_parts;
        test_case "single part" `Quick test_css_rgb_one_part;
      ]);
      ("float_of_px_edge", [
        test_case "dots only" `Quick test_px_dots_only;
        test_case "single dot" `Quick test_px_dot_px;
        test_case "double dot" `Quick test_px_double_dot;
        test_case "minus only" `Quick test_px_minus_only;
        test_case "0px" `Quick test_px_zero;
      ]);
      ("parse_bg_nodes", [
        test_case "empty styles" `Quick test_parse_bg_node_no_background_key;
        test_case "null styles" `Quick test_parse_bg_node_null_styles;
        test_case "multiple bg nodes" `Quick test_parse_multiple_bg_nodes;
        test_case "root transparent" `Quick test_parse_root_transparent_bg;
      ]);
      ("parse_text_color", [
        test_case "transparent color" `Quick test_parse_text_node_transparent_color;
        test_case "rgba color" `Quick test_parse_text_node_rgba_color;
      ]);
      ("parse_fontWeight", [
        test_case "null weight" `Quick test_parse_text_null_weight;
        test_case "bool weight" `Quick test_parse_text_bool_weight;
      ]);
      ("ensure_dir", [
        test_case "existing dir" `Quick test_ensure_dir_existing;
        test_case "create then re-enter" `Quick test_ensure_dir_new_then_again;
      ]);
      ("rgba_of_hex_edge", [
        test_case "empty" `Quick test_hex_empty;
        test_case "just hash" `Quick test_hex_just_hash;
        test_case "lowercase" `Quick test_hex_lowercase;
        test_case "8-char full alpha" `Quick test_hex8_full_alpha;
        test_case "8-char zero alpha" `Quick test_hex8_zero_alpha;
      ]);
      ("int_of_string_opt_edge", [
        test_case "whitespace padded" `Quick test_int_whitespace_padded;
        test_case "float string" `Quick test_int_float_string;
      ]);
      ("starts_with_edge", [
        test_case "exact match" `Quick test_starts_with_exact;
        test_case "both empty" `Quick test_starts_with_empty_string;
      ]);
    ]
