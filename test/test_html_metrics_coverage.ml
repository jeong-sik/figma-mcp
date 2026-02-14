(** Coverage tests for html_metrics.ml — pure computation functions only.
    Targets: rgba_of_hex, rgba_of_css, float_of_px, int_of_string_opt,
    starts_with, parse, temp_file, ensure_dir *)

let () =
  let open Alcotest in
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
  in
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
  in
  let int_opt = option int in

  (* starts_with *)
  let test_starts_with_true () =
    check bool "prefix match" true (Html_metrics.starts_with ~prefix:"#" "#FF0000")
  in
  let test_starts_with_false () =
    check bool "no prefix" false (Html_metrics.starts_with ~prefix:"#" "FF0000")
  in
  let test_starts_with_empty_prefix () =
    check bool "empty prefix always matches" true (Html_metrics.starts_with ~prefix:"" "anything")
  in
  let test_starts_with_longer_prefix () =
    check bool "prefix longer than string" false (Html_metrics.starts_with ~prefix:"abcdef" "abc")
  in

  (* rgba_of_hex *)
  let test_hex6 () =
    let expected = Some Figma_types.{ r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    check rgba_testable "red 6-char" expected (Html_metrics.rgba_of_hex "#FF0000")
  in
  let test_hex6_no_hash () =
    let expected = Some Figma_types.{ r = 0.0; g = 1.0; b = 0.0; a = 1.0 } in
    check rgba_testable "green no hash" expected (Html_metrics.rgba_of_hex "00FF00")
  in
  let test_hex8 () =
    let expected = Some Figma_types.{ r = 1.0; g = 1.0; b = 1.0; a = 0.502 } in
    check rgba_testable "white 50% alpha" expected (Html_metrics.rgba_of_hex "#FFFFFF80")
  in
  let test_hex_invalid_length () =
    check rgba_testable "wrong length" None (Html_metrics.rgba_of_hex "#FFF")
  in
  let test_hex_whitespace () =
    let expected = Some Figma_types.{ r = 0.0; g = 0.0; b = 0.0; a = 1.0 } in
    check rgba_testable "trimmed" expected (Html_metrics.rgba_of_hex "  #000000  ")
  in
  let test_hex_malformed () =
    (* "ZZZZZZ" -> hex_to_int returns 0 for malformed *)
    let expected = Some Figma_types.{ r = 0.0; g = 0.0; b = 0.0; a = 1.0 } in
    check rgba_testable "malformed hex chars default to 0" expected (Html_metrics.rgba_of_hex "ZZZZZZ")
  in

  (* rgba_of_css *)
  let test_css_empty () =
    check rgba_testable "empty string" None (Html_metrics.rgba_of_css "")
  in
  let test_css_transparent () =
    check rgba_testable "transparent" None (Html_metrics.rgba_of_css "transparent")
  in
  let test_css_transparent_rgba () =
    check rgba_testable "rgba(0,0,0,0)" None (Html_metrics.rgba_of_css "rgba(0, 0, 0, 0)")
  in
  let test_css_hash () =
    let expected = Some Figma_types.{ r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    check rgba_testable "hex via css" expected (Html_metrics.rgba_of_css "#FF0000")
  in
  let test_css_rgb3 () =
    let expected = Some Figma_types.{ r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    check rgba_testable "rgb(255,0,0)" expected (Html_metrics.rgba_of_css "rgb(255, 0, 0)")
  in
  let test_css_rgba4 () =
    let expected = Some Figma_types.{ r = 0.0; g = 0.0; b = 1.0; a = 0.5 } in
    check rgba_testable "rgba(0,0,255,0.5)" expected (Html_metrics.rgba_of_css "rgba(0, 0, 255, 0.5)")
  in
  let test_css_rgb_uppercase () =
    let expected = Some Figma_types.{ r = 1.0; g = 1.0; b = 1.0; a = 1.0 } in
    check rgba_testable "RGB uppercase" expected (Html_metrics.rgba_of_css "RGB(255, 255, 255)")
  in
  let test_css_unknown_format () =
    check rgba_testable "unknown" None (Html_metrics.rgba_of_css "hsl(0, 100%, 50%)")
  in
  let test_css_malformed_rgb () =
    (* rgb(a, b) -> only 2 parts, falls to _ -> None *)
    check rgba_testable "too few parts" None (Html_metrics.rgba_of_css "rgb(255, 0)")
  in
  let test_css_malformed_parens () =
    (* Missing closing paren -> inside="" -> parts=[""] -> no match *)
    check rgba_testable "bad parens" None (Html_metrics.rgba_of_css "rgb(255, 0, 0")
  in

  (* float_of_px *)
  let test_px_normal () =
    check float_opt_testable "16px" (Some 16.0) (Html_metrics.float_of_px "16px")
  in
  let test_px_float () =
    check float_opt_testable "1.5em" (Some 1.5) (Html_metrics.float_of_px "1.5em")
  in
  let test_px_negative () =
    check float_opt_testable "-2.5px" (Some (-2.5)) (Html_metrics.float_of_px "-2.5px")
  in
  let test_px_bare_number () =
    check float_opt_testable "bare 42" (Some 42.0) (Html_metrics.float_of_px "42")
  in
  let test_px_no_number () =
    check float_opt_testable "no digits" None (Html_metrics.float_of_px "px")
  in
  let test_px_empty () =
    check float_opt_testable "empty" None (Html_metrics.float_of_px "")
  in
  let test_px_whitespace () =
    check float_opt_testable "trimmed" (Some 10.0) (Html_metrics.float_of_px "  10px  ")
  in

  (* int_of_string_opt *)
  let test_int_normal () =
    check int_opt "42" (Some 42) (Html_metrics.int_of_string_opt "42")
  in
  let test_int_empty () =
    check int_opt "empty" None (Html_metrics.int_of_string_opt "")
  in
  let test_int_whitespace () =
    check int_opt "spaces" None (Html_metrics.int_of_string_opt "   ")
  in
  let test_int_invalid () =
    check int_opt "abc" None (Html_metrics.int_of_string_opt "abc")
  in
  let test_int_negative () =
    check int_opt "-5" (Some (-5)) (Html_metrics.int_of_string_opt "-5")
  in

  (* parse — full JSON → typed record *)
  let test_parse_minimal () =
    let json = Yojson.Safe.from_string {|{
      "viewport": {"width": 375, "height": 812},
      "root": null,
      "text_nodes": [],
      "bg_nodes": []
    }|} in
    match Html_metrics.parse json with
    | Error e -> fail (Printf.sprintf "parse error: %s" e)
    | Ok m ->
        check int "viewport_width" 375 m.viewport_width;
        check int "viewport_height" 812 m.viewport_height;
        check bool "root is None" true (m.root = None);
        check int "no text_nodes" 0 (List.length m.text_nodes);
        check int "no bg_nodes" 0 (List.length m.bg_nodes)
  in
  let test_parse_with_root () =
    let json = Yojson.Safe.from_string {|{
      "viewport": {"width": 400, "height": 600},
      "root": {
        "bbox": {"x": 0.0, "y": 0.0, "width": 400.0, "height": 600.0},
        "styles": {"backgroundColor": "#FF0000", "borderRadius": "8px"}
      },
      "text_nodes": [],
      "bg_nodes": []
    }|} in
    match Html_metrics.parse json with
    | Error e -> fail (Printf.sprintf "parse error: %s" e)
    | Ok m ->
        check bool "root present" true (m.root <> None);
        let root = Option.get m.root in
        check (float 0.1) "root width" 400.0 root.bbox.width;
        check bool "root has bg" true (root.background <> None)
  in
  let test_parse_with_text_nodes () =
    let json = Yojson.Safe.from_string {|{
      "viewport": {"width": 375, "height": 812},
      "root": null,
      "text_nodes": [{
        "text": "Hello",
        "bbox": {"x": 10.0, "y": 20.0, "width": 100.0, "height": 30.0},
        "styles": {
          "fontSize": "16px",
          "fontWeight": "700",
          "fontFamily": "Inter",
          "color": "rgb(255, 0, 0)"
        }
      }],
      "bg_nodes": [{
        "bbox": {"x": 0.0, "y": 0.0, "width": 375.0, "height": 100.0},
        "styles": {"backgroundColor": "", "borderRadius": ""}
      }]
    }|} in
    match Html_metrics.parse json with
    | Error e -> fail (Printf.sprintf "parse error: %s" e)
    | Ok m ->
        check int "1 text node" 1 (List.length m.text_nodes);
        check int "1 bg node" 1 (List.length m.bg_nodes);
        let tn = List.hd m.text_nodes in
        check string "text" "Hello" tn.text;
        check (float 0.1) "x" 10.0 tn.bbox.x;
        check float_opt_testable "fontSize" (Some 16.0) tn.font_size_px;
        check int_opt "fontWeight" (Some 700) tn.font_weight;
        check (option string) "fontFamily" (Some "Inter") tn.font_family;
        check bool "color present" true (tn.color <> None)
  in
  let test_parse_text_node_int_weight () =
    let json = Yojson.Safe.from_string {|{
      "viewport": {"width": 375, "height": 812},
      "root": null,
      "text_nodes": [{
        "text": "Test",
        "bbox": {"x": 0.0, "y": 0.0, "width": 50.0, "height": 20.0},
        "styles": {"fontWeight": 400}
      }],
      "bg_nodes": []
    }|} in
    match Html_metrics.parse json with
    | Error e -> fail (Printf.sprintf "parse error: %s" e)
    | Ok m ->
        let tn = List.hd m.text_nodes in
        check int_opt "fontWeight int" (Some 400) tn.font_weight
  in
  let test_parse_text_node_float_weight () =
    let json = Yojson.Safe.from_string {|{
      "viewport": {"width": 375, "height": 812},
      "root": null,
      "text_nodes": [{
        "text": "Test",
        "bbox": {"x": 0.0, "y": 0.0, "width": 50.0, "height": 20.0},
        "styles": {"fontWeight": 600.0}
      }],
      "bg_nodes": []
    }|} in
    match Html_metrics.parse json with
    | Error e -> fail (Printf.sprintf "parse error: %s" e)
    | Ok m ->
        let tn = List.hd m.text_nodes in
        check int_opt "fontWeight float" (Some 600) tn.font_weight
  in
  let test_parse_text_node_no_styles () =
    let json = Yojson.Safe.from_string {|{
      "viewport": {"width": 375, "height": 812},
      "root": null,
      "text_nodes": [{
        "text": "Bare",
        "bbox": {"x": 0.0, "y": 0.0, "width": 50.0, "height": 20.0},
        "styles": {}
      }],
      "bg_nodes": []
    }|} in
    match Html_metrics.parse json with
    | Error e -> fail (Printf.sprintf "parse error: %s" e)
    | Ok m ->
        let tn = List.hd m.text_nodes in
        check float_opt_testable "no fontSize" None tn.font_size_px;
        check int_opt "no fontWeight" None tn.font_weight;
        check (option string) "no fontFamily" None tn.font_family;
        check rgba_testable "no color" None tn.color
  in
  let test_parse_invalid_json () =
    let json = `String "not an object" in
    match Html_metrics.parse json with
    | Ok _ -> fail "should have errored"
    | Error _ -> ()
  in

  (* temp_file *)
  let test_temp_file_unique () =
    let a = Html_metrics.temp_file ~prefix:"test" ~ext:"html" in
    let b = Html_metrics.temp_file ~prefix:"test" ~ext:"html" in
    check bool "unique paths" true (a <> b)
  in

  run "Html_metrics Coverage" [
    ("starts_with", [
      test_case "true" `Quick test_starts_with_true;
      test_case "false" `Quick test_starts_with_false;
      test_case "empty prefix" `Quick test_starts_with_empty_prefix;
      test_case "longer prefix" `Quick test_starts_with_longer_prefix;
    ]);
    ("rgba_of_hex", [
      test_case "6-char red" `Quick test_hex6;
      test_case "6-char no hash" `Quick test_hex6_no_hash;
      test_case "8-char alpha" `Quick test_hex8;
      test_case "invalid length" `Quick test_hex_invalid_length;
      test_case "whitespace" `Quick test_hex_whitespace;
      test_case "malformed" `Quick test_hex_malformed;
    ]);
    ("rgba_of_css", [
      test_case "empty" `Quick test_css_empty;
      test_case "transparent" `Quick test_css_transparent;
      test_case "transparent rgba" `Quick test_css_transparent_rgba;
      test_case "hash" `Quick test_css_hash;
      test_case "rgb(3)" `Quick test_css_rgb3;
      test_case "rgba(4)" `Quick test_css_rgba4;
      test_case "RGB uppercase" `Quick test_css_rgb_uppercase;
      test_case "unknown format" `Quick test_css_unknown_format;
      test_case "malformed rgb" `Quick test_css_malformed_rgb;
      test_case "malformed parens" `Quick test_css_malformed_parens;
    ]);
    ("float_of_px", [
      test_case "16px" `Quick test_px_normal;
      test_case "1.5em" `Quick test_px_float;
      test_case "-2.5px" `Quick test_px_negative;
      test_case "bare" `Quick test_px_bare_number;
      test_case "no number" `Quick test_px_no_number;
      test_case "empty" `Quick test_px_empty;
      test_case "whitespace" `Quick test_px_whitespace;
    ]);
    ("int_of_string_opt", [
      test_case "42" `Quick test_int_normal;
      test_case "empty" `Quick test_int_empty;
      test_case "spaces" `Quick test_int_whitespace;
      test_case "invalid" `Quick test_int_invalid;
      test_case "negative" `Quick test_int_negative;
    ]);
    ("parse", [
      test_case "minimal" `Quick test_parse_minimal;
      test_case "with root" `Quick test_parse_with_root;
      test_case "with text nodes" `Quick test_parse_with_text_nodes;
      test_case "int weight" `Quick test_parse_text_node_int_weight;
      test_case "float weight" `Quick test_parse_text_node_float_weight;
      test_case "no styles" `Quick test_parse_text_node_no_styles;
      test_case "invalid json" `Quick test_parse_invalid_json;
    ]);
    ("temp_file", [
      test_case "unique" `Quick test_temp_file_unique;
    ]);
  ]
