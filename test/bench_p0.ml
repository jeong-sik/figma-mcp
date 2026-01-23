(** P0 CSS Fidelity 성능 벤치마크 *)

open Figma_types
open Figma_codegen

let time_it name iterations f =
  let start = Unix.gettimeofday () in
  for _ = 1 to iterations do
    ignore (f ())
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-30s: %d iterations in %.3fs (%.0f µs/iter)\n"
    name iterations elapsed (elapsed /. float_of_int iterations *. 1_000_000.)

let () =
  Printf.printf "\n=== 🚀 P0 CSS Fidelity Benchmark ===\n\n";

  (* P0-4: Gradient 벤치마크 *)
  let gradient_stops = [
    (0.0, { r = 1.0; g = 0.0; b = 0.0; a = 1.0 });
    (0.25, { r = 1.0; g = 0.5; b = 0.0; a = 1.0 });
    (0.5, { r = 0.0; g = 1.0; b = 0.0; a = 1.0 });
    (0.75, { r = 0.0; g = 0.5; b = 1.0; a = 1.0 });
    (1.0, { r = 0.0; g = 0.0; b = 1.0; a = 1.0 });
  ] in
  time_it "gradient_to_css (5 stops)" 10_000 (fun () ->
    gradient_to_css gradient_stops
  );
  time_it "gradient_to_css ~precise (5 stops)" 10_000 (fun () ->
    gradient_to_css ~precise:true gradient_stops
  );

  (* P0-3: Effects 벤치마크 *)
  let effects = [
    { fx_type = DropShadow; visible = true; radius = 10.0;
      offset = Some (4.0, 4.0); spread = Some 2.0;
      color = Some { r = 0.0; g = 0.0; b = 0.0; a = 0.25 } };
    { fx_type = InnerShadow; visible = true; radius = 5.0;
      offset = Some (2.0, 2.0); spread = None;
      color = Some { r = 1.0; g = 1.0; b = 1.0; a = 0.5 } };
    { fx_type = LayerBlur; visible = true; radius = 8.0;
      offset = None; spread = None; color = None };
    { fx_type = BackgroundBlur; visible = true; radius = 12.0;
      offset = None; spread = None; color = None };
  ] in
  time_it "effects_to_css (4 effects)" 10_000 (fun () ->
    effects_to_css effects
  );
  time_it "effects_to_css ~precise (4 effects)" 10_000 (fun () ->
    effects_to_css ~precise:true effects
  );

  (* Invisible effects (should be fast) *)
  let invisible_fx = [
    { fx_type = DropShadow; visible = false; radius = 10.0;
      offset = Some (4.0, 4.0); spread = Some 2.0;
      color = Some { r = 0.0; g = 0.0; b = 0.0; a = 0.25 } };
    { fx_type = LayerBlur; visible = false; radius = 8.0;
      offset = None; spread = None; color = None };
  ] in
  time_it "effects_to_css (all invisible)" 10_000 (fun () ->
    effects_to_css invisible_fx
  );

  (* Output sample *)
  Printf.printf "\n=== 📄 Sample Outputs ===\n";
  Printf.printf "gradient_to_css: %s\n"
    (Option.value ~default:"None" (gradient_to_css gradient_stops));
  Printf.printf "effects_to_css: %s\n"
    (effects_to_css effects);

  Printf.printf "\n=== ✅ Benchmark Complete ===\n"
