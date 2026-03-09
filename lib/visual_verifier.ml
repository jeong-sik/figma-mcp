(** Visual Verifier - HTML → PNG 렌더링 및 시각적 비교

    99%+ Figma-to-Code 정확도 달성을 위한 Visual Feedback Loop 핵심 모듈.

    Pipeline:
    1. HTML → Playwright → PNG (render_html_to_png)
    2. Figma PNG ↔ HTML PNG → SSIM 비교 (compare_renders)
    3. SSIM < threshold → 자동 조정 힌트 생성 (suggest_corrections)
*)

open Printf

[@@@coverage off]

(* ============== 설정 (Figma_config에서 가져옴) ============== *)

let temp_dir = Figma_config.Visual.temp_dir

let render_script_path =
  match Figma_config.Visual.render_script with
  | Some path -> path
  | None ->
      (* 상대 경로로 scripts/render-html.js 찾기 *)
      let candidates = [
        Filename.concat (Sys.getcwd ()) "scripts/render-html.js";
        Filename.concat (Filename.dirname Sys.executable_name) "../scripts/render-html.js";
        "/Users/dancer/me/.worktrees/figma-mcp-streaming/features/figma-mcp/scripts/render-html.js";
      ] in
      List.find_opt Sys.file_exists candidates
      |> Option.value ~default:"scripts/render-html.js"

let default_target_ssim = 0.99
let default_max_iterations = 5

(** ============== 유틸리티 ============== *)

(** 디렉토리 생성 (존재하지 않으면) *)
let ensure_dir path =
  if Sys.file_exists path then ()
  else
    (* Best-effort: in concurrent runs another fiber/process may have created it. *)
    try Unix.mkdir path 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** 고유 파일명 생성 *)
let generate_temp_filename ~prefix ~ext =
  let timestamp = Unix.gettimeofday () in
  let random = Random.int 10000 in
  sprintf "%s_%d_%04d.%s" prefix (int_of_float timestamp) random ext

(** 임시 파일 경로 생성 *)
let temp_file ~prefix ~ext =
  ensure_dir temp_dir;
  Filename.concat temp_dir (generate_temp_filename ~prefix ~ext)

let run_ok ~cmd ~args =
  match Safe_exec.run ~timeout_ms:20000 ~output_limit:(256 * 1024) cmd args with
  | Error e -> Error e
  | Ok out ->
      if out.timed_out then Error "Process timed out"
      else
        match out.status with
        | Unix.WEXITED 0 -> Ok out
        | Unix.WEXITED code -> Error (sprintf "Exit code %d: %s" code out.stderr)
        | Unix.WSIGNALED sig_num -> Error (sprintf "Killed by signal %d" sig_num)
        | Unix.WSTOPPED _ -> Error "Process stopped"

let mkdir_p path =
  let rec loop p =
    if p = Filename.dirname p then ()
    else if Sys.file_exists p then ()
    else begin
      loop (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  loop path

let copy_file ~src ~dst =
  let buf = Bytes.create 65536 in
  In_channel.with_open_bin src (fun ic ->
    Out_channel.with_open_bin dst (fun oc ->
      let rec pump () =
        match In_channel.input ic buf 0 (Bytes.length buf) with
        | 0 -> ()
        | n -> Out_channel.output oc buf 0 n; pump ()
      in
      pump ()
    )
  )

(** ============== 핵심 기능 ============== *)

(** HTML을 PNG로 렌더링

    @param html HTML 문자열 또는 파일 경로
    @param width 뷰포트 너비 (기본: 375)
    @param height 뷰포트 높이 (기본: 812)
    @return PNG 파일 경로 (Ok) 또는 에러 메시지 (Error)
*)
let render_html_to_png ?(width=375) ?(height=812) html =
  (* HTML을 임시 파일로 저장 (문자열인 경우) *)
  let html_path =
    if Sys.file_exists html then html
    else begin
      let path = temp_file ~prefix:"html" ~ext:"html" in
      Out_channel.with_open_bin path (fun oc ->
        output_string oc html
      );
      path
    end
  in

  let output_path = temp_file ~prefix:"render" ~ext:"png" in

  (* Node.js Playwright 스크립트 실행 *)
  let args = [|
    "node";
    render_script_path;
    html_path;
    output_path;
    string_of_int width;
    string_of_int height;
  |] in

  match run_ok ~cmd:"node" ~args with
  | Ok output ->
    (* JSON 응답 파싱 *)
    (try
      let json = Yojson.Safe.from_string (String.trim output.stdout) in
      let open Yojson.Safe.Util in
      if json |> member "success" |> to_bool then
        Ok output_path
      else
        Error (json |> member "error" |> to_string_option |> Option.value ~default:"Unknown error")
    with Yojson.Json_error _ ->
      if Sys.file_exists output_path then Ok output_path
      else Error ("Failed to parse render output: " ^ output.stdout))
  | Error e -> Error e

(** 두 PNG 이미지의 SSIM 및 Delta E 비교

    @param figma_png Figma에서 내보낸 PNG 경로
    @param html_png HTML 렌더링 결과 PNG 경로
    @return (SSIM, Delta E) 또는 에러 메시지
*)
let compare_renders ~figma_png ~html_png =
  match Figma_image_similarity.compare_paths_auto ~path_a:figma_png ~path_b:html_png with
  | Ok metrics -> Ok (metrics.ssim, metrics.delta_e)
  | Error e -> Error e

[@@@coverage on]

(** Human-Eye SSIM 계산
    SSIM(구조)과 Delta E(색상)를 결합하여 인간의 인지적 유사도를 도출합니다.
    Delta E 1.0 = JND (Just Noticeable Difference).
    Delta E > 10.0 = Significant difference.
*)
let calculate_human_ssim ssim delta_e =
  let color_penalty = min 1.0 (delta_e /. 50.0) in (* 50 이상이면 색상 점수 0 *)
  ssim *. (1.0 -. color_penalty)

(** 전체 비교 메트릭 반환 *)
let compare_renders_full ~figma_png ~html_png =
  Figma_image_similarity.compare_paths_auto ~path_a:figma_png ~path_b:html_png

(** 영역별 diff 정보 (ssim-compare.js의 regions 출력) *)
type diff_quadrants = {
  top_left: float;
  top_right: float;
  bottom_left: float;
  bottom_right: float;
}

type diff_strips = {
  strip_top: float;
  strip_middle: float;
  strip_bottom: float;
}

type diff_edges = {
  edge_top: float;
  edge_bottom: float;
  edge_left: float;
  edge_right: float;
}

type diff_regions = {
  quadrants: diff_quadrants;
  strips: diff_strips;
  edges: diff_edges;
}

(** 빈 diff_regions (fallback용) *)
let empty_diff_regions = {
  quadrants = { top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0 };
  strips = { strip_top = 0.0; strip_middle = 0.0; strip_bottom = 0.0 };
  edges = { edge_top = 0.0; edge_bottom = 0.0; edge_left = 0.0; edge_right = 0.0 };
}

(** 🆕 고급 메트릭 (논문 기반) *)
type advanced_metrics = {
  true_ssim: float;       (** Wang et al. 2004 - IEEE TIP *)
  ms_ssim: float;         (** Wang et al. 2003 - Multi-Scale *)
  pixel_match: float;     (** Legacy pixelmatch-based *)
  lpips: float option;    (** Zhang et al. 2018 - CVPR (optional) *)
}

(** SSIM + 영역 분석 결과 (Node.js ssim-compare.js 직접 호출) *)
type comparison_with_regions = {
  ssim: float;
  psnr: float;
  mse: float;
  delta_e: float;
  diff_pixels: int;
  total_pixels: int;
  width: int;
  height: int;
  regions: diff_regions;
  advanced: advanced_metrics option;  (** 🆕 논문 기반 메트릭 *)
}

(** ssim-compare.js 스크립트 경로 찾기 *)
let ssim_script_path =
  let candidates = [
    Filename.concat (Sys.getcwd ()) "scripts/ssim-compare.js";
    Filename.concat (Filename.dirname Sys.executable_name) "../scripts/ssim-compare.js";
    "/Users/dancer/me/.worktrees/figma-mcp-streaming/features/figma-mcp/scripts/ssim-compare.js";
  ] in
  List.find_opt Sys.file_exists candidates
  |> Option.value ~default:"scripts/ssim-compare.js"

(** JSON에서 diff_regions 파싱 *)
let parse_diff_regions json =
  let open Yojson.Safe.Util in
  try
    let regions = json |> member "regions" in
    if regions = `Null then empty_diff_regions
    else
      let quads = regions |> member "quadrants" in
      let strips = regions |> member "strips" in
      let edges = regions |> member "edges" in
      {
        quadrants = {
          top_left = quads |> member "topLeft" |> to_float;
          top_right = quads |> member "topRight" |> to_float;
          bottom_left = quads |> member "bottomLeft" |> to_float;
          bottom_right = quads |> member "bottomRight" |> to_float;
        };
        strips = {
          strip_top = strips |> member "top" |> to_float;
          strip_middle = strips |> member "middle" |> to_float;
          strip_bottom = strips |> member "bottom" |> to_float;
        };
        edges = {
          edge_top = edges |> member "top" |> to_float;
          edge_bottom = edges |> member "bottom" |> to_float;
          edge_left = edges |> member "left" |> to_float;
          edge_right = edges |> member "right" |> to_float;
        };
      }
  with exn ->
    Printf.eprintf "[visual_verifier] Warning: diff regions parse failed: %s\n%!" (Printexc.to_string exn);
    empty_diff_regions

(** Node.js ssim-compare.js로 비교 (영역 정보 포함) *)
let compare_renders_with_regions ~figma_png ~html_png : (comparison_with_regions, string) result =
  let args = [| "node"; ssim_script_path; figma_png; html_png |] in
  let output =
    match run_ok ~cmd:"node" ~args with
    | Ok out -> out.stdout
    | Error e -> e
  in
  try
    let json = Yojson.Safe.from_string (String.trim output) in
    let open Yojson.Safe.Util in
    match json |> member "error" with
    | `Null ->
        let ssim = json |> member "ssim" |> to_float in
        let psnr = json |> member "psnr" |> to_float in
        let mse = json |> member "mse" |> to_float in
        let delta_e = json |> member "deltaE" |> to_float_option |> Option.value ~default:0.0 in
        let diff_pixels = json |> member "diffPixels" |> to_int in
        let total_pixels = json |> member "totalPixels" |> to_int in
        let width = json |> member "width" |> to_int in
        let height = json |> member "height" |> to_int in
        let regions = parse_diff_regions json in
        (* 🆕 Parse advanced metrics (논문 기반) *)
        let advanced =
          let adv = json |> member "advanced" in
          if adv = `Null then None
          else Some {
            true_ssim = adv |> member "trueSSIM" |> to_float_option |> Option.value ~default:ssim;
            ms_ssim = adv |> member "msSSIM" |> to_float_option |> Option.value ~default:0.0;
            pixel_match = adv |> member "pixelMatch" |> to_float_option |> Option.value ~default:0.0;
            lpips = adv |> member "lpips" |> to_float_option;
          }
        in
        Ok { ssim; psnr; mse; delta_e; diff_pixels; total_pixels; width; height; regions; advanced }
    | error_msg -> Error (to_string error_msg)
  with e -> Error (sprintf "JSON parse error: %s\nOutput: %s" (Printexc.to_string e) output)

(** ============== 자동 조정 힌트 ============== *)

(** 조정 힌트 타입 *)
type correction_hint =
  | AdjustPadding of float * float * float * float  (** top, right, bottom, left *)
  | AdjustGap of float
  | AdjustColor of string * Figma_types.rgba  (** selector, target color *)
  | AdjustSize of float * float  (** width, height delta *)
  | AdjustFontSize of float
  | AdjustBorderRadius of float

(** 힌트를 JSON으로 변환 *)
let hint_to_json = function
  | AdjustPadding (t, r, b, l) ->
    `Assoc [
      ("type", `String "padding");
      ("top", `Float t); ("right", `Float r);
      ("bottom", `Float b); ("left", `Float l);
    ]
  | AdjustGap g ->
    `Assoc [("type", `String "gap"); ("value", `Float g)]
  | AdjustColor (sel, rgba) ->
    `Assoc [
      ("type", `String "color");
      ("selector", `String sel);
      ("r", `Float rgba.r); ("g", `Float rgba.g);
      ("b", `Float rgba.b); ("a", `Float rgba.a);
    ]
  | AdjustSize (w, h) ->
    `Assoc [("type", `String "size"); ("width", `Float w); ("height", `Float h)]
  | AdjustFontSize s ->
    `Assoc [("type", `String "font_size"); ("value", `Float s)]
  | AdjustBorderRadius r ->
    `Assoc [("type", `String "border_radius"); ("value", `Float r)]

(** 힌트를 자연어 설명으로 변환 *)
let hint_to_description = function
  | AdjustPadding (t, r, b, l) ->
    let parts = [] in
    let parts = if t > 0. then (sprintf "상단 %.0fpx" t) :: parts else parts in
    let parts = if r > 0. then (sprintf "우측 %.0fpx" r) :: parts else parts in
    let parts = if b > 0. then (sprintf "하단 %.0fpx" b) :: parts else parts in
    let parts = if l > 0. then (sprintf "좌측 %.0fpx" l) :: parts else parts in
    if parts = [] then "여백 미세 조정 필요"
    else sprintf "여백(padding) 추가 필요: %s" (String.concat ", " (List.rev parts))
  | AdjustGap g ->
    sprintf "요소 간격(gap) %.0fpx 조정 필요" g
  | AdjustColor (sel, rgba) ->
    sprintf "'%s' 색상을 rgba(%.0f,%.0f,%.0f,%.2f)로 변경 필요"
      sel (rgba.r *. 255.) (rgba.g *. 255.) (rgba.b *. 255.) rgba.a
  | AdjustSize (w, h) ->
    sprintf "크기 조정 필요: 너비 %+.0fpx, 높이 %+.0fpx" w h
  | AdjustFontSize s ->
    sprintf "폰트 크기 %+.0fpx 조정 필요" s
  | AdjustBorderRadius r ->
    sprintf "모서리 둥글기(border-radius) %.0fpx로 조정 필요" r

(** 힌트 목록을 자연어 요약으로 변환 *)
let hints_to_summary hints =
  if hints = [] then
    "✅ 조정 불필요 - 시각적 일치도가 충분합니다."
  else
    let descriptions = List.map hint_to_description hints in
    sprintf "📋 %d개 조정 제안:\n%s"
      (List.length hints)
      (String.concat "\n" (List.mapi (fun i d -> sprintf "  %d. %s" (i+1) d) descriptions))

(** 힌트 목록을 JSON 배열로 변환 (MCP 클라이언트용)

    @param hints correction_hint list
    @return JSON array: [{"type": "padding", ...}, ...]

    Usage:
      let json = correction_hints_to_json [AdjustPadding (10., 0., 0., 0.)]
      (* Returns: [{"type":"padding","top":10,"right":0,"bottom":0,"left":0}] *)
*)
let correction_hints_to_json hints =
  `List (List.map hint_to_json hints)

(** SSIM 점수와 영역별 diff 분석 기반 조정 힌트 생성
    diff_regions의 영역별 차이 비율을 분석하여 타겟 조정을 제안합니다.

    전략:
    - edges 높음 (>5%) → padding 조정 필요
    - bottom strip 높음 → 하단 영역 (버튼 등) 문제
    - quadrants 불균형 → 레이아웃/gap 문제
    - 전체적으로 높지만 edges 낮음 → 콘텐츠/아이콘 문제 (수동 확인 필요)
*)
let suggest_corrections ~ssim ~diff_regions =
  if ssim >= 0.99 then
    (* 이미 99% 이상: 조정 불필요 *)
    []
  else
    let hints = ref [] in
    let regions = diff_regions in

    (* 1. Edge 분석: padding 문제 감지 *)
    let edge_threshold = 0.05 in (* 5% 이상이면 문제 *)
    let top_edge = regions.edges.edge_top in
    let bottom_edge = regions.edges.edge_bottom in
    let left_edge = regions.edges.edge_left in
    let right_edge = regions.edges.edge_right in

    if top_edge > edge_threshold || bottom_edge > edge_threshold ||
       left_edge > edge_threshold || right_edge > edge_threshold then begin
      (* 가장자리에 차이가 많음 → padding 조정 *)
      let top_delta = if top_edge > edge_threshold then 1.0 else 0.0 in
      let bottom_delta = if bottom_edge > edge_threshold then 1.0 else 0.0 in
      let left_delta = if left_edge > edge_threshold then 1.0 else 0.0 in
      let right_delta = if right_edge > edge_threshold then 1.0 else 0.0 in
      hints := AdjustPadding (top_delta, right_delta, bottom_delta, left_delta) :: !hints
    end;

    (* 2. Strip 분석: 영역별 문제 감지 *)
    let strip_threshold = 0.08 in (* 8% 이상이면 문제 *)
    let bottom_strip = regions.strips.strip_bottom in
    let top_strip = regions.strips.strip_top in

    if bottom_strip > strip_threshold then begin
      (* 하단 영역 문제 (버튼, 푸터) → 하단 padding 추가 조정 *)
      hints := AdjustPadding (0.0, 0.0, 1.0, 0.0) :: !hints
    end;

    if top_strip > strip_threshold then begin
      (* 상단 영역 문제 (헤더) → 상단 padding 조정 *)
      hints := AdjustPadding (1.0, 0.0, 0.0, 0.0) :: !hints
    end;

    (* 3. Quadrant 분석: 레이아웃/gap 불균형 감지 *)
    let quad_threshold = 0.10 in (* 10% 이상이면 문제 *)
    let max_quad = max
      (max regions.quadrants.top_left regions.quadrants.top_right)
      (max regions.quadrants.bottom_left regions.quadrants.bottom_right) in
    let min_quad = min
      (min regions.quadrants.top_left regions.quadrants.top_right)
      (min regions.quadrants.bottom_left regions.quadrants.bottom_right) in

    if max_quad -. min_quad > 0.05 then begin
      (* Quadrant 간 불균형 → gap 조정 *)
      hints := AdjustGap 1.0 :: !hints
    end;

    if max_quad > quad_threshold then begin
      (* 특정 quadrant가 매우 높음 → 크기 조정 *)
      hints := AdjustSize (1.0, 1.0) :: !hints
    end;

    (* 4. SSIM 기반 fallback: 영역 분석이 충분하지 않을 때 *)
    if !hints = [] then begin
      if ssim < 0.90 then
        hints := [AdjustPadding (1.0, 1.0, 1.0, 1.0); AdjustGap 1.0]
      else if ssim < 0.95 then
        hints := [AdjustPadding (0.5, 0.5, 0.5, 0.5)]
      else if ssim < 0.99 then
        hints := [AdjustPadding (0.2, 0.2, 0.2, 0.2)]
    end;

    (* 중복 제거 및 반환 *)
    List.sort_uniq compare !hints

(** ============== HTML 조정 적용 ============== *)

(** CSS 값 조정 헬퍼 - global_substitute로 한 번에 모든 매치 처리 *)
let adjust_css_value pattern delta html =
  (* POSIX regex: [ \t]* for whitespace (Str module doesn't support \s) *)
  let re = Str.regexp (pattern ^ ":[ \t]*\\([0-9.]+\\)px") in
  Str.global_substitute re (fun s ->
    let original = float_of_string (Str.matched_group 1 s) in
    let adjusted = max 0.0 (original +. delta) in
    sprintf "%s: %.1fpx" pattern adjusted
  ) html

(** 패딩 조정 적용 *)
let apply_padding_adjustment (top, right, bottom, left) html =
  html
  |> adjust_css_value "padding-top" top
  |> adjust_css_value "padding-right" right
  |> adjust_css_value "padding-bottom" bottom
  |> adjust_css_value "padding-left" left
  (* padding 축약형도 처리 - 단순화 버전 *)
  |> (fun h ->
    let re = Str.regexp "padding:[ \t]*\\([0-9.]+\\)px" in
    try
      let _ = Str.search_forward re h 0 in
      let original = float_of_string (Str.matched_group 1 h) in
      let avg_delta = (top +. right +. bottom +. left) /. 4.0 in
      let adjusted = max 0.0 (original +. avg_delta) in
      Str.replace_first re (sprintf "padding: %.1fpx" adjusted) h
    with Not_found -> h)

(** gap 조정 적용 *)
let apply_gap_adjustment delta html =
  adjust_css_value "gap" delta html

(** 크기 조정 적용 *)
let apply_size_adjustment (width_delta, height_delta) html =
  html
  |> adjust_css_value "width" width_delta
  |> adjust_css_value "height" height_delta

(** font-size 조정 적용 *)
let apply_font_size_adjustment delta html =
  adjust_css_value "font-size" delta html

(** border-radius 조정 적용 *)
let apply_border_radius_adjustment delta html =
  adjust_css_value "border-radius" delta html

(** 색상 조정 적용 (CSS selector 기반) *)
let apply_color_adjustment selector rgba html =
  (* CSS 변수나 특정 selector의 색상 교체 *)
  let color_hex = sprintf "#%02x%02x%02x"
    (int_of_float (rgba.Figma_types.r *. 255.0))
    (int_of_float (rgba.Figma_types.g *. 255.0))
    (int_of_float (rgba.Figma_types.b *. 255.0))
  in
  let escape_regex s =
    let buf = Buffer.create (String.length s * 2) in
    String.iter (fun c ->
      match c with
      | '\\' | '.' | '+' | '*' | '?' | '[' | ']' | '^' | '$' | '(' | ')' | '{' | '}' | '|' ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
      | _ -> Buffer.add_char buf c
    ) s;
    Buffer.contents buf
  in
  let replace_color_in_rule rule =
    let bg_re = Str.regexp "background-color:[ \t]*#[0-9a-fA-F]+" in
    let fg_re = Str.regexp "color:[ \t]*#[0-9a-fA-F]+" in
    try Str.replace_first bg_re (sprintf "background-color: %s" color_hex) rule
    with Not_found ->
      (try Str.replace_first fg_re (sprintf "color: %s" color_hex) rule
       with Not_found -> rule)
  in
  (* 간단 구현: selector가 없으면 첫 번째 color 속성 교체 *)
  if selector = "" then
    let re = Str.regexp "\\(background-\\)?color:[ \t]*#[0-9a-fA-F]+" in
    Str.replace_first re (sprintf "color: %s" color_hex) html
  else
    (* 제한적 구현: selector 블록 내 color 또는 background-color 교체 *)
    let sel = escape_regex selector in
    let rule_re = Str.regexp (sprintf "%s[^{]*{[^}]*}" sel) in
    try
      let _ = Str.search_forward rule_re html 0 in
      let rule = Str.matched_string html in
      let updated = replace_color_in_rule rule in
      Str.replace_first rule_re updated html
    with Not_found ->
      html

(** 단일 힌트를 HTML에 적용 *)
let apply_single_hint hint html =
  match hint with
  | AdjustPadding (t, r, b, l) -> apply_padding_adjustment (t, r, b, l) html
  | AdjustGap g -> apply_gap_adjustment g html
  | AdjustSize (w, h) -> apply_size_adjustment (w, h) html
  | AdjustFontSize s -> apply_font_size_adjustment s html
  | AdjustBorderRadius r -> apply_border_radius_adjustment r html
  | AdjustColor (sel, rgba) -> apply_color_adjustment sel rgba html

(** 모든 힌트를 HTML에 적용 *)
let apply_corrections hints html =
  List.fold_left (fun h hint -> apply_single_hint hint h) html hints

(** ============== Visual Feedback Loop ============== *)

(** 진화 과정의 단일 스텝 *)
type evolution_step = {
  step_num: int;
  step_ssim: float;
  step_delta_e: float;
  step_human_ssim: float;
  step_html_path: string;
  step_png_path: string;
  corrections_this_step: correction_hint list;
}

type verification_result = {
  ssim: float;
  delta_e: float;
  human_ssim: float;
  passed: bool;
  iterations: int;
  figma_png: string;
  html_png: string;
  corrections_applied: correction_hint list;
  final_html: string option;
  evolution_history: evolution_step list;  (* 진화 과정 기록 *)
  evolution_dir: string;                    (* 진화 저장 디렉토리 *)
  errors: string list;                      (* 실패/부분 실패 원인 (진단용) *)
}

(** evolution_step을 JSON으로 변환 *)
let step_to_json step =
  `Assoc [
    ("step", `Int step.step_num);
    ("ssim", `Float step.step_ssim);
    ("delta_e", `Float step.step_delta_e);
    ("human_ssim", `Float step.step_human_ssim);
    ("html_path", `String step.step_html_path);
    ("png_path", `String step.step_png_path);
    ("corrections", `List (List.map hint_to_json step.corrections_this_step));
  ]

(** 결과를 JSON으로 변환 *)
let result_to_json result =
  `Assoc [
    ("ssim", `Float result.ssim);
    ("delta_e", `Float result.delta_e);
    ("human_ssim", `Float result.human_ssim);
    ("passed", `Bool result.passed);
    ("iterations", `Int result.iterations);
    ("figma_png", `String result.figma_png);
    ("html_png", `String result.html_png);
    ("corrections_applied", `List (List.map hint_to_json result.corrections_applied));
    ("final_html", match result.final_html with Some h -> `String h | None -> `Null);
    ("evolution_history", `List (List.map step_to_json result.evolution_history));
    ("evolution_dir", `String result.evolution_dir);
    ("errors", `List (List.map (fun e -> `String e) result.errors));
  ]

(** 진화 디렉토리 생성 *)
let create_evolution_dir () =
  let base_dir = "/tmp/figma-evolution" in
  ensure_dir base_dir;
  let pid = Unix.getpid () in
  let rec attempt tries =
    let timestamp_ms = int_of_float (Unix.gettimeofday () *. 1000.0) in
    let nonce = Random.int 1_000_000 in
    let dir = sprintf "%s/run_%d_%d_%06d" base_dir timestamp_ms pid nonce in
    try
      Unix.mkdir dir 0o755;
      ensure_dir (Filename.concat dir "html");
      dir
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) when tries < 25 -> attempt (tries + 1)
  in
  attempt 0

(** HTML을 파일로 저장 *)
let save_html ~dir ~step html =
  let path = sprintf "%s/html/step%d.html" dir step in
  Out_channel.with_open_bin path (fun oc ->
    output_string oc html
  );
  path

(** PNG를 evolution 디렉토리로 복사 *)
let save_png ~dir ~step ~src_png =
  let dst = sprintf "%s/step%d_render.png" dir step in
  copy_file ~src:src_png ~dst;
  dst

(** Visual Feedback Loop 실행

    @param figma_png Figma 원본 PNG 경로
    @param html 초기 HTML 문자열
    @param target_ssim 목표 SSIM (기본: 0.95)
    @param max_iterations 최대 반복 횟수 (기본: 3)
    @param width 뷰포트 너비
    @param height 뷰포트 높이
    @param save_evolution 진화 과정 저장 여부 (기본: true)
    @param html_png_provided 외부에서 렌더링된 HTML PNG 경로 (Chrome MCP 등)
    @return 검증 결과
*)
let verify_visual
    ?(target_ssim=default_target_ssim)
    ?(max_iterations=default_max_iterations)
    ?(width=375)
    ?(height=812)
    ?(save_evolution=true)
    ?html_png_provided
    ~figma_png
    html
  =
  let evo_dir = if save_evolution then create_evolution_dir () else temp_dir in

  (* Figma 원본도 evolution 디렉토리에 복사 *)
  let _ =
    if save_evolution then
      copy_file ~src:figma_png ~dst:(Filename.concat evo_dir "figma_original.png")
  in

  (* 외부 렌더링 이미지가 제공된 경우: Playwright 스킵, 단일 비교만 수행 *)
  match html_png_provided with
  | Some external_png when Sys.file_exists external_png ->
    let saved_png = if save_evolution then save_png ~dir:evo_dir ~step:1 ~src_png:external_png else external_png in
    let saved_html = if save_evolution then save_html ~dir:evo_dir ~step:1 html else "" in
    (match compare_renders_with_regions ~figma_png ~html_png:external_png with
     | Ok result ->
       let hssim = calculate_human_ssim result.ssim result.delta_e in
       let step = { step_num = 1; step_ssim = result.ssim;
                    step_delta_e = result.delta_e; step_human_ssim = hssim;
                    step_html_path = saved_html; step_png_path = saved_png;
                    corrections_this_step = [] } in
       { ssim = result.ssim; delta_e = result.delta_e; human_ssim = hssim;
         passed = hssim >= target_ssim; iterations = 1;
         figma_png; html_png = saved_png; corrections_applied = [];
         final_html = Some html;
         evolution_history = [step];
         evolution_dir = evo_dir;
         errors = [] }
     | Error e ->
       { ssim = 0.0; delta_e = 0.0; human_ssim = 0.0; passed = false; iterations = 0;
         figma_png; html_png = external_png; corrections_applied = [];
         final_html = Some html;
         evolution_history = [];
         evolution_dir = evo_dir;
         errors = [sprintf "compare failed: %s" e] })
  | Some _missing_png ->
    { ssim = 0.0; delta_e = 0.0; human_ssim = 0.0; passed = false; iterations = 0;
      figma_png; html_png = ""; corrections_applied = [];
      final_html = Some html;
      evolution_history = [];
      evolution_dir = evo_dir;
      errors = ["html_png_provided path does not exist"] }
  | None ->
    (* 기본 모드: Playwright 렌더링 + 반복 개선 *)
    let rec loop iteration current_html corrections history =
    if iteration > max_iterations then
      (* 최대 반복 도달 - 마지막 결과 반환 *)
      match render_html_to_png ~width ~height current_html with
      | Ok html_png ->
        let saved_png = if save_evolution then save_png ~dir:evo_dir ~step:iteration ~src_png:html_png else html_png in
        let saved_html = if save_evolution then save_html ~dir:evo_dir ~step:iteration current_html else "" in
        (match compare_renders_with_regions ~figma_png ~html_png with
        | Ok result ->
          let hssim = calculate_human_ssim result.ssim result.delta_e in
          let step = { step_num = iteration; step_ssim = result.ssim;
                       step_delta_e = result.delta_e; step_human_ssim = hssim;
                       step_html_path = saved_html; step_png_path = saved_png;
                       corrections_this_step = [] } in
          { ssim = result.ssim; delta_e = result.delta_e; human_ssim = hssim;
            passed = hssim >= target_ssim; iterations = iteration - 1;
            figma_png; html_png = saved_png; corrections_applied = corrections;
            final_html = Some current_html;
            evolution_history = List.rev (step :: history);
            evolution_dir = evo_dir;
            errors = [] }
        | Error e ->
          { ssim = 0.0; delta_e = 0.0; human_ssim = 0.0; passed = false; iterations = iteration - 1;
            figma_png; html_png = ""; corrections_applied = corrections;
            final_html = Some current_html;
            evolution_history = List.rev history;
            evolution_dir = evo_dir;
            errors = [sprintf "compare failed: %s" e] })
      | Error e ->
        { ssim = 0.0; delta_e = 0.0; human_ssim = 0.0; passed = false; iterations = iteration - 1;
          figma_png; html_png = ""; corrections_applied = corrections;
          final_html = Some current_html;
          evolution_history = List.rev history;
          evolution_dir = evo_dir;
          errors = [sprintf "render failed: %s" e] }
    else
      match render_html_to_png ~width ~height current_html with
      | Error e ->
        { ssim = 0.0; delta_e = 0.0; human_ssim = 0.0; passed = false; iterations = iteration;
          figma_png; html_png = ""; corrections_applied = corrections;
          final_html = None;
          evolution_history = List.rev history;
          evolution_dir = evo_dir;
          errors = [sprintf "render failed: %s" e] }
      | Ok html_png ->
        let saved_png = if save_evolution then save_png ~dir:evo_dir ~step:iteration ~src_png:html_png else html_png in
        let saved_html = if save_evolution then save_html ~dir:evo_dir ~step:iteration current_html else "" in
        match compare_renders_with_regions ~figma_png ~html_png with
        | Error e ->
          { ssim = 0.0; delta_e = 0.0; human_ssim = 0.0; passed = false; iterations = iteration;
            figma_png; html_png = saved_png; corrections_applied = corrections;
            final_html = Some current_html;
            evolution_history = List.rev history;
            evolution_dir = evo_dir;
            errors = [sprintf "compare failed: %s" e] }
        | Ok result ->
          let hssim = calculate_human_ssim result.ssim result.delta_e in
          let step = { step_num = iteration; step_ssim = result.ssim;
                       step_delta_e = result.delta_e; step_human_ssim = hssim;
                       step_html_path = saved_html; step_png_path = saved_png;
                       corrections_this_step = [] } in
          if hssim >= target_ssim then
            (* 목표 달성! *)
            { ssim = result.ssim; delta_e = result.delta_e; human_ssim = hssim;
              passed = true; iterations = iteration;
              figma_png; html_png = saved_png; corrections_applied = corrections;
              final_html = Some current_html;
              evolution_history = List.rev (step :: history);
              evolution_dir = evo_dir;
              errors = [] }
          else
            (* 조정 힌트 생성 및 적용 - 영역별 diff 분석 활용 *)
            let hints = suggest_corrections ~ssim:result.ssim ~diff_regions:result.regions in
            let adjusted_html = apply_corrections hints current_html in
            let step_with_hints = { step with corrections_this_step = hints } in
            loop (iteration + 1) adjusted_html (corrections @ hints) (step_with_hints :: history)
    in
    loop 1 html [] []

(** ============== Diff 시각화 ============== *)

(** Quadrant SSIM 결과 (SSIM, Delta E) *)
type quadrant_result = {
  top_left: float * float;
  top_right: float * float;
  bottom_left: float * float;
  bottom_right: float * float;
  overall: float * float;
}

(** 4분면별 SSIM 분석

    이미지를 4등분하여 각 영역의 유사도를 측정합니다.
    어느 부분이 가장 차이나는지 파악하는데 유용합니다.
*)
let quadrant_analysis ~figma_png ~html_png =
  (* ImageMagick를 사용해 4분면으로 자르고 각각 비교 *)
  let crop_and_compare ~crop_spec ~suffix =
    let figma_crop = temp_file ~prefix:("figma_" ^ suffix) ~ext:"png" in
    let html_crop = temp_file ~prefix:("html_" ^ suffix) ~ext:"png" in
    let args_a = [| "magick"; figma_png; "-crop"; crop_spec; "+repage"; figma_crop |] in
    let args_b = [| "magick"; html_png; "-crop"; crop_spec; "+repage"; html_crop |] in
    match (run_ok ~cmd:"magick" ~args:args_a,
           run_ok ~cmd:"magick" ~args:args_b) with
    | (Ok _, Ok _) ->
        (match compare_renders ~figma_png:figma_crop ~html_png:html_crop with
         | Ok (ssim, de) -> Some (ssim, de)
         | Error _ -> None)
    | _ -> None
  in

  match compare_renders_full ~figma_png ~html_png with
  | Error e -> Error e
  | Ok metrics ->
      let w = metrics.Figma_image_similarity.overlap_width in
      let h = metrics.Figma_image_similarity.overlap_height in
      let hw = w / 2 in
      let hh = h / 2 in

      (* 각 분면 crop spec: WxH+X+Y *)
      let tl = crop_and_compare ~crop_spec:(sprintf "%dx%d+0+0" hw hh) ~suffix:"tl" in
      let tr = crop_and_compare ~crop_spec:(sprintf "%dx%d+%d+0" hw hh hw) ~suffix:"tr" in
      let bl = crop_and_compare ~crop_spec:(sprintf "%dx%d+0+%d" hw hh hh) ~suffix:"bl" in
      let br = crop_and_compare ~crop_spec:(sprintf "%dx%d+%d+%d" hw hh hw hh) ~suffix:"br" in

      Ok {
        top_left = Option.value tl ~default:(0.0, 0.0);
        top_right = Option.value tr ~default:(0.0, 0.0);
        bottom_left = Option.value bl ~default:(0.0, 0.0);
        bottom_right = Option.value br ~default:(0.0, 0.0);
        overall = (metrics.ssim, metrics.delta_e);
      }

(** Diff 이미지 생성 결과 *)
type diff_image_result = {
  side_by_side: string;    (** 나란히 비교 이미지 경로 *)
  diff_overlay: string;    (** 차이 하이라이트 이미지 경로 *)
  diff_percent: float;     (** 다른 픽셀 비율 (0-100) *)
}

(** Diff 이미지 생성

    두 이미지를 시각적으로 비교할 수 있는 결과물 생성:
    1. Side-by-side 비교
    2. 차이 영역 하이라이트 (빨간색 오버레이)
*)
let generate_diff_images ~figma_png ~html_png =
  let side_by_side = temp_file ~prefix:"diff_sidebyside" ~ext:"png" in
  let diff_overlay = temp_file ~prefix:"diff_overlay" ~ext:"png" in

  (* Side-by-side 이미지 생성 *)
  let args_side = [| "magick"; figma_png; html_png; "+append"; side_by_side |] in
  let args_diff = [| "magick"; "compare"; "-metric"; "AE"; "-highlight-color"; "red"; "-lowlight-color"; "none"; figma_png; html_png; diff_overlay |] in

  match run_ok ~cmd:"magick" ~args:args_side with
  | Error e -> Error (sprintf "Failed to create side-by-side: %s" e)
  | Ok _ ->
      (* compare 명령은 stderr에 pixel count를 출력함 *)
      (* 출력 형식: "10000 (1)" 또는 "10000" - 첫 번째 숫자만 추출 *)
      let diff_pixels_str =
        match Safe_exec.run ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_diff with
        | Ok out ->
            (match out.status with
             | Unix.WEXITED 0 | Unix.WEXITED 1 -> out.stderr
             | Unix.WEXITED code -> out.stderr ^ Printf.sprintf " (exit %d)" code
             | _ -> out.stderr)
        | Error e -> e
      in
          let diff_count =
            let trimmed = String.trim diff_pixels_str in
            (* 공백이나 '('로 분리하여 첫 번째 숫자만 추출 *)
            (* ImageMagick 출력 형식: "10000 (1)" 또는 "10000" *)
            let first_token =
              try
                let space_pos = String.index trimmed ' ' in
                String.sub trimmed 0 space_pos
              with Not_found ->
                try
                  let paren_pos = String.index trimmed '(' in
                  String.sub trimmed 0 paren_pos
                with Not_found -> trimmed
            in
            try float_of_string first_token
            with Failure _ -> 0.0
          in
          (* 이미지 크기로 비율 계산 *)
          let get_image_size path =
            (* identify -format "%w %h" 명령으로 크기 획득 *)
            let args = [| "magick"; "identify"; "-format"; "%w %h"; path |] in
            match run_ok ~cmd:"magick" ~args with
            | Ok size_str ->
                (try
                  let parts = String.split_on_char ' ' (String.trim size_str.stdout) in
                  match parts with
                  | [w; h] -> Some (int_of_string w * int_of_string h)
                  | _ -> None
                with Failure _ -> None)
            | Error _ -> None
          in
          let total_pixels =
            match compare_renders_full ~figma_png ~html_png with
            | Ok metrics -> float_of_int (metrics.overlap_width * metrics.overlap_height)
            | Error _ ->
                (* fallback: 직접 이미지 크기 획득 *)
                match get_image_size figma_png with
                | Some px -> float_of_int px
                | None -> 0.0
          in
          let diff_percent = if total_pixels > 0.0 then (diff_count /. total_pixels) *. 100.0 else 0.0 in
          Ok { side_by_side; diff_overlay; diff_percent }

(** Diff 결과를 JSON으로 변환 *)
let quadrant_result_to_json result =
  let json_field (ssim, de) =
    `Assoc [
      ("ssim", `Float ssim);
      ("delta_e", `Float de);
      ("human_ssim", `Float (calculate_human_ssim ssim de));
    ]
  in
  `Assoc [
    ("top_left", json_field result.top_left);
    ("top_right", json_field result.top_right);
    ("bottom_left", json_field result.bottom_left);
    ("bottom_right", json_field result.bottom_right);
    ("overall", json_field result.overall);
    ("worst_quadrant", `String (
      let hssim (s, d) = calculate_human_ssim s d in
      let tl_h = hssim result.top_left in
      let tr_h = hssim result.top_right in
      let bl_h = hssim result.bottom_left in
      let br_h = hssim result.bottom_right in
      let min_h = min (min tl_h tr_h) (min bl_h br_h) in
      if min_h = tl_h then "top_left"
      else if min_h = tr_h then "top_right"
      else if min_h = bl_h then "bottom_left"
      else "bottom_right"
    ));
  ]

let diff_image_result_to_json result =
  `Assoc [
    ("side_by_side", `String result.side_by_side);
    ("diff_overlay", `String result.diff_overlay);
    ("diff_percent", `Float result.diff_percent);
  ]

(** ============== 임시 파일 정리 ============== *)

(** 오래된 임시 파일 정리 (1시간 이상) *)
let cleanup_temp_files () =
  if Sys.file_exists temp_dir then begin
    let now = Unix.time () in
    let files = Sys.readdir temp_dir in
    Array.iter (fun filename ->
      let filepath = Filename.concat temp_dir filename in
      try
        let stats = Unix.stat filepath in
        if now -. stats.Unix.st_mtime > 3600.0 then
          Unix.unlink filepath
      with Unix.Unix_error _ -> ()
    ) files
  end

(** ============== SSIM 개선 로그 (Feedback Loop) ============== *)

(** SSIM 검증 로그 파일 경로 *)
let ssim_log_path = ".figma-ssim.log"

(** 검증 결과를 로컬 파일에 append

    @param node_id Figma 노드 ID
    @param ssim 측정된 SSIM 값
    @param notes 변경 설명 (optional)
*)
let log_verification ~node_id ~ssim ?(notes="") () =
  let timestamp = Unix.gettimeofday () in
  let iso_time =
  let tm = Unix.localtime timestamp in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  let line = Printf.sprintf "%s|%s|%.4f|%s\n" iso_time node_id ssim notes in
  let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 ssim_log_path in
  Common.protect
    ~module_name:"visual_verifier"
    ~finally_label:"close_out"
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc line)

(** SSIM 개선 기록 (before/after 비교)

    @param node_id Figma 노드 ID
    @param before_ssim 이전 SSIM 값
    @param after_ssim 이후 SSIM 값
    @param change_description 변경 설명
    @return SSIM 개선율 (percentage)
*)
let log_improvement ~node_id ~before_ssim ~after_ssim ~change_description =
  let improvement = (after_ssim -. before_ssim) *. 100.0 in
  let notes = Printf.sprintf "%s (%.1f%% improvement)" change_description improvement in
  log_verification ~node_id ~ssim:after_ssim ~notes ();
  improvement

(** 힌트 적용 결과를 SSIM 로그에 기록 *)
let log_hint_application ~node_id ~before_ssim ~after_ssim ~hints =
  let hint_names = List.map (function
    | AdjustPadding _ -> "padding"
    | AdjustGap _ -> "gap"
    | AdjustColor _ -> "color"
    | AdjustSize _ -> "size"
    | AdjustFontSize _ -> "font_size"
    | AdjustBorderRadius _ -> "border_radius"
  ) hints in
  let hints_str = String.concat "+" hint_names in
  let improvement = (after_ssim -. before_ssim) *. 100. in
  let notes = Printf.sprintf "hints:%s (%.1f%% %s)"
    hints_str
    (abs_float improvement)
    (if improvement >= 0. then "↑" else "↓")
  in
  log_verification ~node_id ~ssim:after_ssim ~notes ()

(** 최근 SSIM 로그 조회

    @param count 조회할 개수 (기본: 20)
    @return (timestamp, node_id, ssim, notes) 리스트
*)
let get_recent_logs ?(count=20) () =
  if Sys.file_exists ssim_log_path then begin
    let lines =
      In_channel.with_open_bin ssim_log_path (fun ic ->
        let acc = ref [] in
        (try while true do acc := input_line ic :: !acc done
         with End_of_file -> ());
        !acc)
    in
    let recent = List.filteri (fun i _ -> i < count) lines in
    List.filter_map (fun line ->
      match String.split_on_char '|' line with
      | [ts; node_id; ssim_str; notes] ->
          (try Some (ts, node_id, float_of_string ssim_str, notes)
           with Failure _ -> None)
      | _ -> None
    ) recent
  end else []

(** 노드별 SSIM 추세 조회

    @param node_id Figma 노드 ID
    @return SSIM 값 리스트 (시간순)
*)
let get_ssim_trend ~node_id =
  get_recent_logs ~count:1000 ()
  |> List.filter (fun (_, nid, _, _) -> nid = node_id)
  |> List.map (fun (_, _, ssim, _) -> ssim)
  |> List.rev
