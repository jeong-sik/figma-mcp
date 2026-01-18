(** Early Stop Detector - 다중 조건 종료 감지기

    목표: 22회 반복 → 10회로 감소 (55% 절감)
    - TARGET: SSIM ≥ 90% 달성
    - PLATEAU: 3회 연속 <0.5% 개선
    - TEXT_CEILING: 텍스트 UI 88% 천장
    - REGRESSION: SSIM 하락 감지
*)

open Printf

(** 종료 조건 타입 *)
type stop_reason =
  | Target_reached      (** 목표 SSIM 달성 *)
  | Max_iterations      (** 최대 반복 횟수 도달 *)
  | Plateau             (** 개선 정체 *)
  | Text_ceiling        (** 텍스트 UI 천장 *)
  | Regression          (** 퇴행 감지 (종료하진 않음) *)
  | Continue            (** 계속 진행 *)

(** 종료 조건 결과 *)
type stop_condition = {
  should_stop: bool;
  reason: stop_reason;
  message: string;
  confidence: float;  (** 0.0 - 1.0 *)
}

(** 감지기 설정 *)
type config = {
  target_ssim: float;         (** 목표 SSIM (기본: 0.90) *)
  plateau_threshold: float;   (** 정체 임계값 (기본: 0.005 = 0.5%) *)
  plateau_patience: int;      (** 정체 인내심 (기본: 3회) *)
  text_ceiling: float;        (** 텍스트 UI 천장 (기본: 0.88) *)
  max_iterations: int;        (** 최대 반복 (기본: 10) *)
}

let default_config = {
  target_ssim = 0.90;
  plateau_threshold = 0.005;
  plateau_patience = 3;
  text_ceiling = 0.88;
  max_iterations = 10;
}

(** 감지기 상태 *)
type t = {
  config: config;
  mutable ssim_history: float list;
  mutable improvement_history: float list;
  mutable best_ssim: float;
  mutable best_iteration: int;
}

(** 감지기 생성 *)
let create ?(config=default_config) () = {
  config;
  ssim_history = [];
  improvement_history = [];
  best_ssim = 0.0;
  best_iteration = 0;
}

(** 텍스트 밀도 계산 (DSL JSON에서) *)
let calculate_text_density (dsl: Yojson.Safe.t) : float =
  let total = ref 0 in
  let text_nodes = ref 0 in
  let rec count node =
    incr total;
    (match Yojson.Safe.Util.(member "type" node |> to_string_option) with
    | Some "TEXT" -> incr text_nodes
    | _ -> ());
    let children =
      try Some (Yojson.Safe.Util.(member "children" node |> to_list))
      with _ -> None
    in
    Option.iter (List.iter count) children
  in
  (try count dsl with _ -> ());
  if !total > 0 then float !text_nodes /. float !total else 0.0

(** 종료 조건 체크 *)
let check t ~current_ssim ~iteration ?(text_density=0.0) () : stop_condition =
  (* 히스토리 업데이트 *)
  t.ssim_history <- t.ssim_history @ [current_ssim];

  if current_ssim > t.best_ssim then begin
    t.best_ssim <- current_ssim;
    t.best_iteration <- iteration
  end;

  (* 1. 목표 달성 *)
  if current_ssim >= t.config.target_ssim then
    { should_stop = true;
      reason = Target_reached;
      message = sprintf "✅ TARGET REACHED: %.1f%% ≥ %.1f%%"
        (current_ssim *. 100.) (t.config.target_ssim *. 100.);
      confidence = 1.0 }

  (* 2. 최대 반복 *)
  else if iteration >= t.config.max_iterations then
    { should_stop = true;
      reason = Max_iterations;
      message = sprintf "⏱️ MAX ITERATIONS: %d reached" iteration;
      confidence = 0.9 }

  (* 3. 개선율 분석 *)
  else begin
    let improvement =
      if List.length t.ssim_history >= 2 then
        let prev = List.nth t.ssim_history (List.length t.ssim_history - 2) in
        current_ssim -. prev
      else 0.0
    in

    if List.length t.ssim_history >= 2 then
      t.improvement_history <- t.improvement_history @ [improvement];

    (* 3a. Plateau 감지 *)
    let recent_improvements =
      let n = t.config.plateau_patience in
      let len = List.length t.improvement_history in
      if len >= n then
        List.filteri (fun i _ -> i >= len - n) t.improvement_history
      else []
    in

    if List.length recent_improvements >= t.config.plateau_patience &&
       List.for_all (fun imp -> imp < t.config.plateau_threshold) recent_improvements then
      { should_stop = true;
        reason = Plateau;
        message = sprintf "🏔️ PLATEAU: %d회 연속 <%.1f%% 개선"
          t.config.plateau_patience (t.config.plateau_threshold *. 100.);
        confidence = 0.85 }

    (* 3b. 퇴행 감지 *)
    else if improvement < 0.0 then
      { should_stop = false;  (* 아직 종료 안함, 경고만 *)
        reason = Regression;
        message = sprintf "⚠️ REGRESSION: %.2f%% 악화 → 이전 버전 유지"
          (improvement *. 100.);
        confidence = 0.7 }

    (* 4. 텍스트 UI 천장 *)
    else if text_density > 0.3 && current_ssim >= t.config.text_ceiling then
      { should_stop = true;
        reason = Text_ceiling;
        message = sprintf "📝 TEXT CEILING: %.1f%% (텍스트 밀도 %.0f%%)"
          (current_ssim *. 100.) (text_density *. 100.);
        confidence = 0.8 }

    (* 5. 계속 진행 *)
    else
      { should_stop = false;
        reason = Continue;
        message = sprintf "🔄 CONTINUE: %.1f%% → target %.1f%%"
          (current_ssim *. 100.) (t.config.target_ssim *. 100.);
        confidence = 0.0 }
  end

(** 진행 상황 요약 *)
let summary t =
  let iterations = List.length t.ssim_history in
  let avg_improvement =
    if List.length t.improvement_history > 0 then
      List.fold_left (+.) 0.0 t.improvement_history
      /. float (List.length t.improvement_history)
    else 0.0
  in
  sprintf "Iterations: %d, Best: %.1f%% (iter %d), Last: %.1f%%, Avg Δ: %.2f%%"
    iterations
    (t.best_ssim *. 100.)
    t.best_iteration
    (if iterations > 0 then List.nth t.ssim_history (iterations - 1) *. 100. else 0.)
    (avg_improvement *. 100.)

(** JSON으로 변환 *)
let to_json t condition =
  `Assoc [
    ("should_stop", `Bool condition.should_stop);
    ("reason", `String (match condition.reason with
      | Target_reached -> "TARGET"
      | Max_iterations -> "MAX_ITER"
      | Plateau -> "PLATEAU"
      | Text_ceiling -> "TEXT_CEILING"
      | Regression -> "REGRESSION"
      | Continue -> "CONTINUE"));
    ("message", `String condition.message);
    ("confidence", `Float condition.confidence);
    ("summary", `String (summary t));
    ("stats", `Assoc [
      ("iterations", `Int (List.length t.ssim_history));
      ("best_ssim", `Float t.best_ssim);
      ("best_iteration", `Int t.best_iteration);
      ("ssim_history", `List (List.map (fun f -> `Float f) t.ssim_history));
    ]);
  ]

(** 상태 리셋 *)
let reset t =
  t.ssim_history <- [];
  t.improvement_history <- [];
  t.best_ssim <- 0.0;
  t.best_iteration <- 0
