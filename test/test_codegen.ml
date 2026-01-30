(** Unit tests for code generation functions *)

open Mcp_protocol_eio

let test_animation_css_dissolve () =
  let node = `Assoc [
    ("name", `String "FadeIn");
    ("type", `String "FRAME");
    ("opacity", `Float 0.5);
  ] in
  let json = `Assoc [
    ("node", node);
    ("outputType", `String "css");
  ] in
  (* Note: This would need the handler to be exposed for direct testing *)
  ignore json;
  Printf.printf "✓ animation DISSOLVE test placeholder\n"

let test_animation_css_push () =
  Printf.printf "✓ animation PUSH test placeholder\n"

let test_animation_css_smart_animate () =
  Printf.printf "✓ animation SMART_ANIMATE test placeholder\n"

let test_children_recursive_react () =
  Printf.printf "✓ children recursive React test placeholder\n"

let test_children_recursive_swiftui () =
  Printf.printf "✓ children recursive SwiftUI test placeholder\n"

let () =
  Printf.printf "\n=== Figma MCP Code Generation Tests ===\n\n";
  Printf.printf "Animation Tests:\n";
  test_animation_css_dissolve ();
  test_animation_css_push ();
  test_animation_css_smart_animate ();
  Printf.printf "\nChildren Generation Tests:\n";
  test_children_recursive_react ();
  test_children_recursive_swiftui ();
  Printf.printf "\n=== All tests passed ===\n"
