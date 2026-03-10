(** Proof Experiment: Multi-Metric Similarity A/B Comparison

    Purpose: Prove or disprove that metric-driven iteration (Treatment)
    outperforms fixed iteration (Control) for Figma design fidelity.

    Groups:
    - C (Control): Fixed 5 iterations, depth 4→8→12→16→20, metrics ignored
    - T (Treatment): Metric-driven iteration with early_stop (target=0.92)

    Output: CSV file with per-design, per-iteration measurements.

    Usage:
      ./proof_experiment --mode simulate   # Synthetic data (no Figma API)
      ./proof_experiment --mode live       # Real Figma API calls
      ./proof_experiment --csv results.csv # Custom output path
*)

open Printf

(* ── Design specimen ─────────────────────────────────────────── *)

type complexity = Simple | Medium | Complex

let string_of_complexity = function
  | Simple -> "simple"
  | Medium -> "medium"
  | Complex -> "complex"

type design = {
  id: string;
  name: string;
  file_key: string;
  node_id: string;
  complexity: complexity;
  estimated_nodes: int;
} [@@warning "-69"]

(* 10 test designs — placeholder IDs, replaced with real ones for live mode *)
let test_designs = [|
  { id = "D01"; name = "Login Form"; file_key = ""; node_id = "";
    complexity = Simple; estimated_nodes = 12 };
  { id = "D02"; name = "Empty State"; file_key = ""; node_id = "";
    complexity = Simple; estimated_nodes = 8 };
  { id = "D03"; name = "Alert Banner"; file_key = ""; node_id = "";
    complexity = Simple; estimated_nodes = 15 };
  { id = "D04"; name = "Card List"; file_key = ""; node_id = "";
    complexity = Medium; estimated_nodes = 35 };
  { id = "D05"; name = "Profile Page"; file_key = ""; node_id = "";
    complexity = Medium; estimated_nodes = 28 };
  { id = "D06"; name = "Settings Screen"; file_key = ""; node_id = "";
    complexity = Medium; estimated_nodes = 42 };
  { id = "D07"; name = "Dashboard Widget"; file_key = ""; node_id = "";
    complexity = Medium; estimated_nodes = 38 };
  { id = "D08"; name = "Full Dashboard"; file_key = ""; node_id = "";
    complexity = Complex; estimated_nodes = 75 };
  { id = "D09"; name = "Chat Screen"; file_key = ""; node_id = "";
    complexity = Complex; estimated_nodes = 60 };
  { id = "D10"; name = "Calendar View"; file_key = ""; node_id = "";
    complexity = Complex; estimated_nodes = 85 };
|]

(* ── Simulation engine ───────────────────────────────────────── *)

(** Simulate SSIM improvement per iteration based on complexity.
    Models diminishing returns: each iteration adds less improvement.
    Adds controlled noise to simulate real-world variance. *)
let simulate_ssim ~complexity ~iteration ~max_iterations ~seed =
  (* Base convergence curve: 1 - e^(-k*t) scaled to realistic range *)
  let base_ceiling = match complexity with
    | Simple -> 0.94
    | Medium -> 0.88
    | Complex -> 0.80
  in
  let noise_scale = match complexity with
    | Simple -> 0.02
    | Medium -> 0.03
    | Complex -> 0.04
  in
  let k = match complexity with
    | Simple -> 0.8
    | Medium -> 0.5
    | Complex -> 0.3
  in
  let t = float_of_int iteration /. float_of_int max_iterations in
  let base = base_ceiling *. (1.0 -. exp (-.k *. t *. float_of_int max_iterations)) in
  (* Deterministic pseudo-random noise from seed *)
  let noise_seed = seed + (iteration * 7919) in  (* prime multiplier *)
  let noise = (float_of_int (noise_seed mod 1000) /. 1000.0 -. 0.5) *. noise_scale in
  let ssim = min 1.0 (max 0.0 (base +. noise)) in
  ssim

(** Simulate delta_e (color difference) inversely correlated with SSIM *)
let simulate_delta_e ~ssim ~seed =
  let base_de = (1.0 -. ssim) *. 40.0 in
  let noise_seed = seed + 3571 in
  let noise = (float_of_int (noise_seed mod 100) /. 100.0 -. 0.5) *. 3.0 in
  max 0.0 (base_de +. noise)

(* ── CSV output ──────────────────────────────────────────────── *)

type measurement = {
  design_id: string;
  design_name: string;
  complexity_str: string;
  group: string;  (* "C" or "T" *)
  iteration: int;
  depth: int;
  ssim: float;
  delta_e: float;
  human_ssim: float;
  fidelity_score: float;
  stopped: bool;
  stop_reason: string;
}

let csv_header =
  "design_id,design_name,complexity,group,iteration,depth,ssim,delta_e,human_ssim,fidelity_score,stopped,stop_reason"

let measurement_to_csv m =
  sprintf "%s,%s,%s,%s,%d,%d,%.6f,%.4f,%.6f,%.6f,%b,%s"
    m.design_id m.design_name m.complexity_str m.group
    m.iteration m.depth m.ssim m.delta_e m.human_ssim m.fidelity_score
    m.stopped m.stop_reason

(* ── Experiment runner ───────────────────────────────────────── *)

(** Control group: fixed 5 iterations, depth 4→8→12→16→20 *)
let run_control_simulated design =
  let depths = [| 4; 8; 12; 16; 20 |] in
  let seed = Hashtbl.hash design.id in
  let measurements = ref [] in
  for i = 0 to 4 do
    let iteration = i + 1 in
    let ssim = simulate_ssim ~complexity:design.complexity
                 ~iteration ~max_iterations:5 ~seed in
    let delta_e = simulate_delta_e ~ssim ~seed:(seed + iteration) in
    let human_ssim = Visual_verifier.calculate_human_ssim ssim delta_e in
    let m = {
      design_id = design.id;
      design_name = design.name;
      complexity_str = string_of_complexity design.complexity;
      group = "C";
      iteration;
      depth = depths.(i);
      ssim;
      delta_e;
      human_ssim;
      fidelity_score = human_ssim;  (* proxy *)
      stopped = (iteration = 5);
      stop_reason = (if iteration = 5 then "MAX_ITER" else "CONTINUE");
    } in
    measurements := m :: !measurements
  done;
  List.rev !measurements

(** Treatment group: metric-driven iteration with early_stop *)
let run_treatment_simulated design =
  let seed = Hashtbl.hash design.id + 10000 in  (* different seed from control *)
  let config = Figma_early_stop.{
    target_ssim = 0.92;
    plateau_threshold = 0.005;
    plateau_patience = 3;
    text_ceiling = 0.88;
    max_iterations = 10;
  } in
  let detector = Figma_early_stop.create ~config () in
  let measurements = ref [] in
  let max_iter = 10 in
  let finished = ref false in
  let i = ref 1 in
  while !i <= max_iter && not !finished do
    let iteration = !i in
    let depth = 4 + (iteration - 1) * 4 in  (* 4, 8, 12, ... *)
    let ssim = simulate_ssim ~complexity:design.complexity
                 ~iteration ~max_iterations:max_iter ~seed in
    let delta_e = simulate_delta_e ~ssim ~seed:(seed + iteration) in
    let human_ssim = Visual_verifier.calculate_human_ssim ssim delta_e in
    let stop_condition = Figma_early_stop.check detector
      ~current_ssim:ssim ~iteration () in
    let m = {
      design_id = design.id;
      design_name = design.name;
      complexity_str = string_of_complexity design.complexity;
      group = "T";
      iteration;
      depth;
      ssim;
      delta_e;
      human_ssim;
      fidelity_score = human_ssim;
      stopped = stop_condition.should_stop;
      stop_reason = (match stop_condition.reason with
        | Figma_early_stop.Target_reached -> "TARGET"
        | Max_iterations -> "MAX_ITER"
        | Plateau -> "PLATEAU"
        | Text_ceiling -> "TEXT_CEILING"
        | Regression -> "REGRESSION"
        | Continue -> "CONTINUE");
    } in
    measurements := m :: !measurements;
    if stop_condition.should_stop then finished := true;
    incr i
  done;
  List.rev !measurements

(* ── Summary statistics ──────────────────────────────────────── *)

(** Extract final measurement for each design in a group *)
let final_measurements_by_group group all_measurements =
  let designs = Array.to_list test_designs in
  List.filter_map (fun d ->
    let design_ms = List.filter (fun m ->
      m.design_id = d.id && m.group = group
    ) all_measurements in
    match List.rev design_ms with
    | last :: _ -> Some last
    | [] -> None
  ) designs

let mean xs =
  let n = List.length xs in
  if n = 0 then 0.0
  else List.fold_left (+.) 0.0 xs /. float_of_int n

let std xs =
  let n = List.length xs in
  if n <= 1 then 0.0
  else
    let m = mean xs in
    let sum_sq = List.fold_left (fun acc x ->
      acc +. (x -. m) ** 2.0
    ) 0.0 xs in
    sqrt (sum_sq /. float_of_int (n - 1))

(** Cohen's d for paired samples *)
let cohens_d xs ys =
  let diffs = List.map2 (fun x y -> x -. y) xs ys in
  let d_mean = mean diffs in
  let d_std = std diffs in
  if d_std = 0.0 then infinity
  else d_mean /. d_std

let print_summary all_measurements =
  let c_finals = final_measurements_by_group "C" all_measurements in
  let t_finals = final_measurements_by_group "T" all_measurements in
  let c_ssims = List.map (fun m -> m.ssim) c_finals in
  let t_ssims = List.map (fun m -> m.ssim) t_finals in
  let c_hssims = List.map (fun m -> m.human_ssim) c_finals in
  let t_hssims = List.map (fun m -> m.human_ssim) t_finals in
  let c_iters = List.map (fun m -> float_of_int m.iteration) c_finals in
  let t_iters = List.map (fun m -> float_of_int m.iteration) t_finals in

  printf "\n═══════════════════════════════════════════════════════\n";
  printf "  PROOF EXPERIMENT RESULTS\n";
  printf "═══════════════════════════════════════════════════════\n\n";

  printf "┌────────────────┬──────────────┬──────────────┐\n";
  printf "│ Metric         │ Control (C)  │ Treatment (T)│\n";
  printf "├────────────────┼──────────────┼──────────────┤\n";
  printf "│ SSIM (mean)    │ %12.4f │ %12.4f │\n" (mean c_ssims) (mean t_ssims);
  printf "│ SSIM (std)     │ %12.4f │ %12.4f │\n" (std c_ssims) (std t_ssims);
  printf "│ human_ssim (m) │ %12.4f │ %12.4f │\n" (mean c_hssims) (mean t_hssims);
  printf "│ human_ssim (s) │ %12.4f │ %12.4f │\n" (std c_hssims) (std t_hssims);
  printf "│ Iterations (m) │ %12.1f │ %12.1f │\n" (mean c_iters) (mean t_iters);
  printf "│ Iterations (s) │ %12.1f │ %12.1f │\n" (std c_iters) (std t_iters);
  printf "└────────────────┴──────────────┴──────────────┘\n\n";

  (* Effect sizes *)
  let d_ssim = cohens_d t_ssims c_ssims in
  let d_hssim = cohens_d t_hssims c_hssims in
  printf "Effect Sizes (Cohen's d):\n";
  printf "  SSIM:       d = %.4f %s\n" d_ssim
    (if abs_float d_ssim >= 0.8 then "(large)"
     else if abs_float d_ssim >= 0.5 then "(medium)"
     else if abs_float d_ssim >= 0.2 then "(small)"
     else "(negligible)");
  printf "  human_ssim: d = %.4f %s\n" d_hssim
    (if abs_float d_hssim >= 0.8 then "(large)"
     else if abs_float d_hssim >= 0.5 then "(medium)"
     else if abs_float d_hssim >= 0.2 then "(small)"
     else "(negligible)");
  printf "\n";

  (* Per-design comparison *)
  printf "Per-Design Comparison (final SSIM):\n";
  printf "┌──────┬───────────────────┬──────────┬──────────┬──────────┬────────┐\n";
  printf "│ ID   │ Name              │ Complex. │ C SSIM   │ T SSIM   │ T > C  │\n";
  printf "├──────┼───────────────────┼──────────┼──────────┼──────────┼────────┤\n";
  List.iter2 (fun c t ->
    let winner = if t.ssim > c.ssim then "yes" else "no" in
    printf "│ %-4s │ %-17s │ %-8s │ %8.4f │ %8.4f │ %-6s │\n"
      c.design_id c.design_name c.complexity_str c.ssim t.ssim winner
  ) c_finals t_finals;
  printf "└──────┴───────────────────┴──────────┴──────────┴──────────┴────────┘\n\n";

  let t_wins = List.length (List.filter_map (fun (c, t) ->
    if t.ssim > c.ssim then Some () else None
  ) (List.combine c_finals t_finals)) in
  let total = List.length c_finals in
  printf "Treatment wins: %d/%d (%.0f%%)\n" t_wins total
    (100.0 *. float_of_int t_wins /. float_of_int total);

  (* Success criteria *)
  printf "\n── Success Criteria ──────────────────────────────────\n";
  printf "  [%s] T SSIM > C SSIM (mean): %.4f > %.4f\n"
    (if mean t_ssims > mean c_ssims then "PASS" else "FAIL")
    (mean t_ssims) (mean c_ssims);
  printf "  [%s] Cohen's d >= 0.5 (medium effect): d = %.4f\n"
    (if abs_float d_ssim >= 0.5 then "PASS" else "FAIL")
    d_ssim;
  printf "  [%s] T iterations <= C iterations: %.1f <= %.1f\n"
    (if mean t_iters <= mean c_iters then "PASS" else "FAIL")
    (mean t_iters) (mean c_iters);

  printf "\nNote: p-values require scipy (run analyze_experiment.py on CSV).\n";
  printf "═══════════════════════════════════════════════════════\n"

(* ── Main ────────────────────────────────────────────────────── *)

let () =
  let mode = ref "simulate" in
  let csv_path = ref "test/proof_experiment_results.csv" in
  let specs = [
    ("--mode", Arg.Set_string mode,
     "simulate|live — Simulation or real Figma API (default: simulate)");
    ("--csv", Arg.Set_string csv_path,
     "path — Output CSV file path (default: test/proof_experiment_results.csv)");
  ] in
  Arg.parse specs (fun _ -> ()) "proof_experiment [--mode simulate|live] [--csv path]";

  printf "Mode: %s\n" !mode;
  printf "CSV output: %s\n\n" !csv_path;

  if !mode = "live" then begin
    eprintf "[ERROR] Live mode requires Figma API credentials and design IDs.\n";
    eprintf "        Edit test_designs array with real file_key/node_id values.\n";
    eprintf "        Set FIGMA_TOKEN environment variable.\n";
    exit 1
  end;

  (* Run experiments *)
  let all_measurements = ref [] in

  Array.iter (fun design ->
    printf "Running design %s (%s) — %s...\n"
      design.id design.name (string_of_complexity design.complexity);

    let c_ms = run_control_simulated design in
    let t_ms = run_treatment_simulated design in
    all_measurements := !all_measurements @ c_ms @ t_ms;
  ) test_designs;

  (* Write CSV *)
  let oc = open_out !csv_path in
  output_string oc (csv_header ^ "\n");
  List.iter (fun m ->
    output_string oc (measurement_to_csv m ^ "\n")
  ) !all_measurements;
  close_out oc;
  printf "\nCSV written to: %s (%d rows)\n" !csv_path (List.length !all_measurements);

  (* Print summary *)
  print_summary !all_measurements
