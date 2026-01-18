(** Memoization 벤치마크 - 독립 실행 *)

let () =
  let open Figma_types in
  let blue_fill = {
    paint_type = Solid; visible = true; opacity = 1.;
    color = Some { r = 0.; g = 0.478; b = 1.; a = 1. };
    gradient_stops = []; image_ref = None; scale_mode = None;
  } in
  let typo = {
    font_family = "Inter"; font_size = 16.; font_weight = 500;
    font_style = "normal"; line_height = Some 24.; letter_spacing = Some 0.;
    text_align_h = Left; text_align_v = Top;
    text_decoration = NoDeco; text_case = Original;
  } in
  let make_child i : ui_node = { default_node with
    id = Printf.sprintf "child_%d" i;
    name = Printf.sprintf "Item %d" i;
    typography = Some typo;
    fills = [blue_fill];
    bbox = Some { x = 0.; y = float_of_int (i * 50); width = 320.; height = 48. };
    layout_mode = Horizontal;
    gap = 12.;
    border_radius = 8.;
  } in
  let root = { default_node with
    id = "root"; name = "Container";
    children = List.init 30 make_child;
  } in

  (* 벤치마크 *)
  let dsl_normal = Figma_codegen.generate_compact root in
  let dsl_memo = Figma_codegen.generate_compact_memoized ~threshold:2 root in
  let len_normal = String.length dsl_normal in
  let len_memo = String.length dsl_memo in

  Printf.printf "\n=== 🧮 Memoization 벤치마크 ===\n";
  Printf.printf "노드 수: 31 (root + 30 children)\n";
  Printf.printf "Normal DSL: %d chars\n" len_normal;
  Printf.printf "Memoized DSL: %d chars\n" len_memo;
  Printf.printf "절약: %d chars (%.1f%%)\n" (len_normal - len_memo)
    (float_of_int (len_normal - len_memo) /. float_of_int len_normal *. 100.);
  Printf.printf "\n=== @vars 블록 (첫 200자) ===\n";
  let vars_end = try String.index dsl_memo '\n' with Not_found -> 200 in
  Printf.printf "%s\n" (String.sub dsl_memo 0 (min vars_end 200));
  Printf.printf "\n=== Normal DSL 샘플 (첫 200자) ===\n";
  Printf.printf "%s...\n" (String.sub dsl_normal 0 (min 200 (String.length dsl_normal)));
  Printf.printf "\n=== Memoized DSL 샘플 (첫 250자) ===\n";
  Printf.printf "%s...\n" (String.sub dsl_memo 0 (min 250 (String.length dsl_memo)))
