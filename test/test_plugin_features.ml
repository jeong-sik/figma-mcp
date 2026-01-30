(** Unit tests for Figma_plugin_features module *)

open Alcotest

(* ============== Animation Tests ============== *)

let test_animation_dissolve () =
  let node = `Assoc [
    ("name", `String "FadeBox");
    ("type", `String "FRAME");
    ("opacity", `Float 0.5);
  ] in
  let animations = Figma_plugin_features.extract_animations node in
  check int "one animation" 1 (List.length animations);
  let anim = List.hd animations in
  check string "element name" "FadeBox" anim.element;
  check string "trigger" "state_change" anim.trigger

let test_animation_push () =
  let node = `Assoc [
    ("name", `String "Modal");
    ("type", `String "FRAME");
    ("opacity", `Float 1.0);  (* Explicit opacity to prevent extra animation *)
    ("reactions", `List [
      `Assoc [
        ("trigger", `Assoc [("type", `String "ON_CLICK")]);
        ("action", `Assoc [
          ("type", `String "NODE");
          ("transition", `Assoc [
            ("type", `String "PUSH");
            ("duration", `Float 0.4);
            ("easing", `Assoc [("type", `String "EASE_IN_AND_OUT")]);
          ]);
        ]);
      ]
    ]);
  ] in
  let animations = Figma_plugin_features.extract_animations node in
  check int "one animation" 1 (List.length animations);
  let anim = List.hd animations in
  check string "trans_type" "PUSH" anim.trans_type;
  check (float 0.01) "duration" 0.4 anim.duration

let test_animation_css_generation () =
  let anim = Figma_plugin_features.{
    element = "Button";
    trigger = "ON_HOVER";
    trans_type = "PUSH";
    duration = 0.3;
    easing = "EASE_OUT";
    properties = [];
  } in
  let css = Figma_plugin_features.animation_to_css 0 anim in
  check bool "contains scale" true (String.length css > 0 &&
    try ignore (Str.search_forward (Str.regexp "scale") css 0); true
    with Not_found -> false)

let test_animation_swiftui_generation () =
  let anim = Figma_plugin_features.{
    element = "Card";
    trigger = "ON_CLICK";
    trans_type = "DISSOLVE";
    duration = 0.5;
    easing = "EASE_IN";
    properties = [("opacity", 0.3)];
  } in
  let swift = Figma_plugin_features.animation_to_swiftui 0 anim in
  check bool "contains animation" true (String.length swift > 0 &&
    try ignore (Str.search_forward (Str.regexp ".animation") swift 0); true
    with Not_found -> false)

(* ============== Accessibility Tests ============== *)

let test_accessibility_button () =
  let node = `Assoc [
    ("name", `String "Submit Button");
    ("type", `String "FRAME");
  ] in
  let (attrs, _suggestions) = Figma_plugin_features.analyze_accessibility node in
  check int "one attr" 1 (List.length attrs);
  let attr = List.hd attrs in
  check string "role" "button" attr.role

let test_accessibility_image () =
  let node = `Assoc [
    ("name", `String "Profile Photo");
    ("type", `String "FRAME");
  ] in
  let (attrs, _) = Figma_plugin_features.analyze_accessibility node in
  check int "one attr" 1 (List.length attrs);
  let attr = List.hd attrs in
  check string "role" "img" attr.role

let test_accessibility_modal () =
  let node = `Assoc [
    ("name", `String "Confirmation Dialog");
    ("type", `String "FRAME");
  ] in
  let (attrs, suggestions) = Figma_plugin_features.analyze_accessibility node in
  check int "one attr" 1 (List.length attrs);
  let attr = List.hd attrs in
  check string "role" "dialog" attr.role;
  check bool "has suggestion" true (List.length suggestions > 0)

let test_accessibility_nested () =
  let node = `Assoc [
    ("name", `String "Card");
    ("type", `String "FRAME");
    ("children", `List [
      `Assoc [("name", `String "Action Button"); ("type", `String "FRAME")];
      `Assoc [("name", `String "Avatar Image"); ("type", `String "FRAME")];
    ]);
  ] in
  let (attrs, _) = Figma_plugin_features.analyze_accessibility node in
  check int "two attrs" 2 (List.length attrs)

(* ============== Variant Tests ============== *)

let test_variant_extraction () =
  let node = `Assoc [
    ("name", `String "Button");
    ("type", `String "COMPONENT_SET");
    ("children", `List [
      `Assoc [("name", `String "State=Default, Size=Small"); ("type", `String "COMPONENT")];
      `Assoc [("name", `String "State=Hover, Size=Large"); ("type", `String "COMPONENT")];
    ]);
  ] in
  let (variants, props) = Figma_plugin_features.extract_variants node in
  check int "two variants" 2 (List.length variants);
  check bool "has State" true (Hashtbl.mem props "State");
  check bool "has Size" true (Hashtbl.mem props "Size")

let test_variant_without_type () =
  (* Children without explicit type should be treated as COMPONENT *)
  let node = `Assoc [
    ("name", `String "Icon");
    ("type", `String "COMPONENT_SET");
    ("children", `List [
      `Assoc [("name", `String "Style=Filled")];
      `Assoc [("name", `String "Style=Outlined")];
    ]);
  ] in
  let (variants, props) = Figma_plugin_features.extract_variants node in
  check int "two variants" 2 (List.length variants);
  check bool "has Style" true (Hashtbl.mem props "Style")

let test_variant_typescript_generation () =
  let node = `Assoc [
    ("name", `String "Chip");
    ("type", `String "COMPONENT_SET");
    ("children", `List [
      `Assoc [("name", `String "Color=Primary"); ("type", `String "COMPONENT")];
      `Assoc [("name", `String "Color=Secondary"); ("type", `String "COMPONENT")];
    ]);
  ] in
  let (variants, props) = Figma_plugin_features.extract_variants node in
  let json = Figma_plugin_features.variants_to_json "Chip" (variants, props) in
  let ts = Yojson.Safe.Util.(member "typescript" json |> to_string) in
  check bool "contains interface" true (
    try ignore (Str.search_forward (Str.regexp "interface ChipProps") ts 0); true
    with Not_found -> false)

(* ============== Responsive Tests ============== *)

let test_responsive_breakpoints () =
  let json = Figma_plugin_features.generate_responsive_css "Card" 320 200 in
  let breakpoints = Yojson.Safe.Util.(member "breakpoints" json |> to_list) in
  check int "three breakpoints" 3 (List.length breakpoints)

let test_responsive_css () =
  let json = Figma_plugin_features.generate_responsive_css "Button" 100 40 in
  let css = Yojson.Safe.Util.(member "css" json |> to_string) in
  check bool "contains media query" true (
    try ignore (Str.search_forward (Str.regexp "@media") css 0); true
    with Not_found -> false)

(* ============== Webhook Tests ============== *)

let test_webhook_file_update () =
  let json = Figma_plugin_features.process_webhook
    "FILE_UPDATE" "abc123" "Design System" "2026-01-29T12:00:00Z" in
  let action = Yojson.Safe.Util.(member "recommended_action" json |> to_string) in
  check string "action" "regenerate_code" action

let test_webhook_library_publish () =
  let json = Figma_plugin_features.process_webhook
    "LIBRARY_PUBLISH" "lib456" "Component Library" "2026-01-29T12:00:00Z" in
  let action = Yojson.Safe.Util.(member "recommended_action" json |> to_string) in
  check string "action" "sync_design_tokens" action

(* ============== Test Suite ============== *)

let animation_tests = [
  ("DISSOLVE detection", `Quick, test_animation_dissolve);
  ("PUSH transition", `Quick, test_animation_push);
  ("CSS generation", `Quick, test_animation_css_generation);
  ("SwiftUI generation", `Quick, test_animation_swiftui_generation);
]

let accessibility_tests = [
  ("Button role", `Quick, test_accessibility_button);
  ("Image role", `Quick, test_accessibility_image);
  ("Modal role", `Quick, test_accessibility_modal);
  ("Nested children", `Quick, test_accessibility_nested);
]

let variant_tests = [
  ("Extraction", `Quick, test_variant_extraction);
  ("Without type", `Quick, test_variant_without_type);
  ("TypeScript generation", `Quick, test_variant_typescript_generation);
]

let responsive_tests = [
  ("Breakpoints", `Quick, test_responsive_breakpoints);
  ("CSS generation", `Quick, test_responsive_css);
]

let webhook_tests = [
  ("FILE_UPDATE", `Quick, test_webhook_file_update);
  ("LIBRARY_PUBLISH", `Quick, test_webhook_library_publish);
]

let () =
  run "Figma Plugin Features" [
    ("Animation", animation_tests);
    ("Accessibility", accessibility_tests);
    ("Variant", variant_tests);
    ("Responsive", responsive_tests);
    ("Webhook", webhook_tests);
  ]
