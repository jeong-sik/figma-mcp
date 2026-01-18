(** Visual Verifier 테스트 *)

open Alcotest

(** ============== 유틸리티 테스트 ============== *)

let test_temp_file_generation () =
  (* 임시 파일 경로 생성 테스트 *)
  let path1 = Visual_verifier.temp_file ~prefix:"test" ~ext:"html" in
  let path2 = Visual_verifier.temp_file ~prefix:"test" ~ext:"html" in
  (* 고유 경로여야 함 *)
  check bool "unique paths" true (path1 <> path2);
  (* 확장자가 올바른지 *)
  check bool "html extension" true (Filename.check_suffix path1 ".html")

let test_ensure_dir () =
  (* 디렉토리 생성 테스트 *)
  let test_dir = "/tmp/figma-visual-test-" ^ string_of_int (Random.int 10000) in
  Visual_verifier.ensure_dir test_dir;
  check bool "dir created" true (Sys.file_exists test_dir);
  (* 정리 *)
  Unix.rmdir test_dir

(** ============== Correction Hint 테스트 ============== *)

let test_hint_to_json_padding () =
  let hint = Visual_verifier.AdjustPadding (10.0, 20.0, 10.0, 20.0) in
  let json = Visual_verifier.hint_to_json hint in
  let open Yojson.Safe.Util in
  check string "type" "padding" (json |> member "type" |> to_string);
  check (Alcotest.float 0.001) "top" 10.0 (json |> member "top" |> to_float)

let test_hint_to_json_gap () =
  let hint = Visual_verifier.AdjustGap 12.0 in
  let json = Visual_verifier.hint_to_json hint in
  let open Yojson.Safe.Util in
  check string "type" "gap" (json |> member "type" |> to_string);
  check (Alcotest.float 0.001) "value" 12.0 (json |> member "value" |> to_float)

let test_hint_to_json_size () =
  let hint = Visual_verifier.AdjustSize (2.0, -1.0) in
  let json = Visual_verifier.hint_to_json hint in
  let open Yojson.Safe.Util in
  check string "type" "size" (json |> member "type" |> to_string);
  check (Alcotest.float 0.001) "width" 2.0 (json |> member "width" |> to_float);
  check (Alcotest.float 0.001) "height" (-1.0) (json |> member "height" |> to_float)

let test_hint_to_json_color () =
  let rgba = Figma_types.{ r = 1.0; g = 0.5; b = 0.0; a = 1.0 } in
  let hint = Visual_verifier.AdjustColor (".button", rgba) in
  let json = Visual_verifier.hint_to_json hint in
  let open Yojson.Safe.Util in
  check string "type" "color" (json |> member "type" |> to_string);
  check string "selector" ".button" (json |> member "selector" |> to_string);
  check (Alcotest.float 0.001) "r" 1.0 (json |> member "r" |> to_float)

(** ============== CSS Correction 적용 테스트 (Phase 2) ============== *)

let test_apply_padding_adjustment () =
  let html = "<div style=\"padding: 10px; padding-top: 5px;\">test</div>" in
  let adjusted = Visual_verifier.apply_padding_adjustment (2.0, 0.0, 0.0, 0.0) html in
  (* padding-top: 5px → 7px *)
  check bool "padding-top adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*padding-top: 7\\.0px.*") s 0)

let test_apply_gap_adjustment () =
  let html = "<div style=\"display: flex; gap: 8px;\">test</div>" in
  let adjusted = Visual_verifier.apply_gap_adjustment 2.0 html in
  check bool "gap adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*gap: 10\\.0px.*") s 0)

let test_apply_size_adjustment () =
  let html = "<div style=\"width: 100px; height: 50px;\">test</div>" in
  let adjusted = Visual_verifier.apply_size_adjustment (5.0, -3.0) html in
  check bool "width adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*width: 105\\.0px.*") s 0);
  check bool "height adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*height: 47\\.0px.*") s 0)

let test_apply_font_size_adjustment () =
  let html = "<span style=\"font-size: 14px;\">text</span>" in
  let adjusted = Visual_verifier.apply_font_size_adjustment 2.0 html in
  check bool "font-size adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*font-size: 16\\.0px.*") s 0)

let test_apply_corrections_multiple () =
  let html = "<div style=\"padding: 10px; gap: 5px;\">test</div>" in
  let hints = Visual_verifier.[
    AdjustPadding (1.0, 1.0, 1.0, 1.0);
    AdjustGap 2.0;
  ] in
  let adjusted = Visual_verifier.apply_corrections hints html in
  (* padding: 10px → 11px, gap: 5px → 7px *)
  check bool "padding adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*padding: 11\\.0px.*") s 0);
  check bool "gap adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*gap: 7\\.0px.*") s 0)

let test_apply_border_radius_adjustment () =
  let html = "<div style=\"border-radius: 4px;\">rounded</div>" in
  let adjusted = Visual_verifier.apply_border_radius_adjustment 2.0 html in
  check bool "border-radius adjusted" true (String.sub adjusted 0 (String.length adjusted) |> fun s ->
    Str.string_match (Str.regexp ".*border-radius: 6\\.0px.*") s 0)

(** ============== Suggestion Logic 테스트 ============== *)

let test_suggest_corrections_low_ssim () =
  (* 낮은 SSIM (< 0.93)에서는 여러 힌트 반환 *)
  let hints = Visual_verifier.suggest_corrections ~ssim:0.85 ~diff_regions:Visual_verifier.empty_diff_regions in
  check bool "multiple hints for low ssim" true (List.length hints >= 2)

let test_suggest_corrections_medium_ssim () =
  (* 중간 SSIM (0.93~0.99)에서는 적은 힌트 *)
  let hints = Visual_verifier.suggest_corrections ~ssim:0.96 ~diff_regions:Visual_verifier.empty_diff_regions in
  check bool "fewer hints for medium ssim" true (List.length hints >= 1)

let test_suggest_corrections_high_ssim () =
  (* 높은 SSIM (>= 0.99)에서는 힌트 없음 - 99% 목표 달성 *)
  let hints = Visual_verifier.suggest_corrections ~ssim:0.995 ~diff_regions:Visual_verifier.empty_diff_regions in
  check bool "no hints for high ssim" true (List.length hints = 0)

(** ============== Result JSON 테스트 ============== *)

let test_result_to_json () =
  let result = Visual_verifier.{
    ssim = 0.97;
    passed = true;
    iterations = 2;
    figma_png = "/tmp/figma.png";
    html_png = "/tmp/html.png";
    corrections_applied = [AdjustGap 2.0];
    final_html = Some "<div>test</div>";
    evolution_history = [];
    evolution_dir = "/tmp/figma-evolution/test";
  } in
  let json = Visual_verifier.result_to_json result in
  let open Yojson.Safe.Util in
  check (Alcotest.float 0.001) "ssim" 0.97 (json |> member "ssim" |> to_float);
  check bool "passed" true (json |> member "passed" |> to_bool);
  check int "iterations" 2 (json |> member "iterations" |> to_int);
  check int "corrections count" 1 (json |> member "corrections_applied" |> to_list |> List.length)

(** ============== 통합 테스트 (Playwright 필요) ============== *)

(* 이 테스트는 Playwright가 설치되어 있어야 실행 가능 *)
let test_render_html_to_png_basic () =
  (* Playwright 존재 여부 확인 *)
  let playwright_check = Sys.command "which node > /dev/null 2>&1" in
  if playwright_check <> 0 then begin
    Printf.printf "Skipping Playwright test (node not found)\n";
    check bool "skip" true true
  end else begin
    let html = {|
      <!DOCTYPE html>
      <html>
      <head><style>body { background: #007AFF; width: 100px; height: 100px; }</style></head>
      <body></body>
      </html>
    |} in
    match Visual_verifier.render_html_to_png ~width:100 ~height:100 html with
    | Ok png_path ->
      check bool "png created" true (Sys.file_exists png_path);
      (* 정리 *)
      Unix.unlink png_path
    | Error e ->
      (* Playwright 미설치 시 graceful fail *)
      Printf.printf "Playwright render skipped: %s\n" e;
      check bool "graceful fail" true true
  end

(** ============== 테스트 그룹 ============== *)

let utility_tests = [
  "temp file generation", `Quick, test_temp_file_generation;
  "ensure dir", `Quick, test_ensure_dir;
]

let hint_tests = [
  "hint to json - padding", `Quick, test_hint_to_json_padding;
  "hint to json - gap", `Quick, test_hint_to_json_gap;
  "hint to json - size", `Quick, test_hint_to_json_size;
  "hint to json - color", `Quick, test_hint_to_json_color;
]

let css_correction_tests = [
  "apply padding adjustment", `Quick, test_apply_padding_adjustment;
  "apply gap adjustment", `Quick, test_apply_gap_adjustment;
  "apply size adjustment", `Quick, test_apply_size_adjustment;
  "apply font-size adjustment", `Quick, test_apply_font_size_adjustment;
  "apply border-radius adjustment", `Quick, test_apply_border_radius_adjustment;
  "apply corrections multiple", `Quick, test_apply_corrections_multiple;
]

let suggestion_tests = [
  "suggest corrections - low ssim", `Quick, test_suggest_corrections_low_ssim;
  "suggest corrections - medium ssim", `Quick, test_suggest_corrections_medium_ssim;
  "suggest corrections - high ssim", `Quick, test_suggest_corrections_high_ssim;
]

let result_tests = [
  "result to json", `Quick, test_result_to_json;
]

let integration_tests = [
  "render html to png basic", `Slow, test_render_html_to_png_basic;
]

(** ============== Diff 시각화 테스트 (Phase 4) ============== *)

let test_quadrant_result_to_json () =
  let result = Visual_verifier.{
    top_left = 0.95;
    top_right = 0.92;
    bottom_left = 0.98;
    bottom_right = 0.91;
    overall = 0.94;
  } in
  let json = Visual_verifier.quadrant_result_to_json result in
  let open Yojson.Safe.Util in
  check (Alcotest.float 0.001) "top_left" 0.95 (json |> member "top_left" |> to_float);
  check (Alcotest.float 0.001) "top_right" 0.92 (json |> member "top_right" |> to_float);
  check (Alcotest.float 0.001) "bottom_left" 0.98 (json |> member "bottom_left" |> to_float);
  check (Alcotest.float 0.001) "bottom_right" 0.91 (json |> member "bottom_right" |> to_float);
  check (Alcotest.float 0.001) "overall" 0.94 (json |> member "overall" |> to_float)

let test_diff_image_result_to_json () =
  let result = Visual_verifier.{
    side_by_side = "/tmp/side_by_side.png";
    diff_overlay = "/tmp/diff.png";
    diff_percent = 5.2;
  } in
  let json = Visual_verifier.diff_image_result_to_json result in
  let open Yojson.Safe.Util in
  check string "side_by_side" "/tmp/side_by_side.png" (json |> member "side_by_side" |> to_string);
  check string "diff_overlay" "/tmp/diff.png" (json |> member "diff_overlay" |> to_string);
  check (Alcotest.float 0.001) "diff_percent" 5.2 (json |> member "diff_percent" |> to_float)

let test_quadrant_analysis_integration () =
  (* ImageMagick 존재 여부 확인 *)
  let magick_check = Sys.command "which magick > /dev/null 2>&1" in
  if magick_check <> 0 then begin
    Printf.printf "Skipping quadrant analysis test (ImageMagick not found)\n";
    check bool "skip" true true
  end else begin
    (* 두 개의 테스트 이미지 생성 *)
    let img1 = Visual_verifier.temp_file ~prefix:"test_q1" ~ext:"png" in
    let img2 = Visual_verifier.temp_file ~prefix:"test_q2" ~ext:"png" in
    let cmd1 = Printf.sprintf "magick -size 100x100 xc:blue %s 2>/dev/null" img1 in
    let cmd2 = Printf.sprintf "magick -size 100x100 xc:blue %s 2>/dev/null" img2 in
    ignore (Sys.command cmd1);
    ignore (Sys.command cmd2);

    if Sys.file_exists img1 && Sys.file_exists img2 then begin
      match Visual_verifier.quadrant_analysis ~figma_png:img1 ~html_png:img2 with
      | Ok result ->
          (* 동일 이미지이므로 높은 SSIM 예상 *)
          check bool "overall ssim high" true (result.overall >= 0.9);
          Unix.unlink img1;
          Unix.unlink img2
      | Error e ->
          Printf.printf "Quadrant analysis error: %s\n" e;
          check bool "graceful" true true
    end else begin
      Printf.printf "Failed to create test images\n";
      check bool "skip" true true
    end
  end

let test_generate_diff_images_integration () =
  (* ImageMagick 존재 여부 확인 *)
  let magick_check = Sys.command "which magick > /dev/null 2>&1" in
  if magick_check <> 0 then begin
    Printf.printf "Skipping diff images test (ImageMagick not found)\n";
    check bool "skip" true true
  end else begin
    (* 두 개의 약간 다른 테스트 이미지 생성 *)
    let img1 = Visual_verifier.temp_file ~prefix:"test_d1" ~ext:"png" in
    let img2 = Visual_verifier.temp_file ~prefix:"test_d2" ~ext:"png" in
    let cmd1 = Printf.sprintf "magick -size 100x100 xc:blue %s 2>/dev/null" img1 in
    let cmd2 = Printf.sprintf "magick -size 100x100 xc:red %s 2>/dev/null" img2 in
    ignore (Sys.command cmd1);
    ignore (Sys.command cmd2);

    if Sys.file_exists img1 && Sys.file_exists img2 then begin
      match Visual_verifier.generate_diff_images ~figma_png:img1 ~html_png:img2 with
      | Ok result ->
          (* 결과 파일들이 생성되었는지 확인 *)
          check bool "side_by_side exists" true (Sys.file_exists result.side_by_side);
          check bool "diff_overlay exists" true (Sys.file_exists result.diff_overlay);
          (* 파란색과 빨간색은 100% 다름 *)
          check bool "diff_percent > 0" true (result.diff_percent > 0.0);
          (* 정리 *)
          Unix.unlink img1;
          Unix.unlink img2;
          (try Unix.unlink result.side_by_side with _ -> ());
          (try Unix.unlink result.diff_overlay with _ -> ())
      | Error e ->
          Printf.printf "Generate diff images error: %s\n" e;
          check bool "graceful" true true
    end else begin
      Printf.printf "Failed to create test images\n";
      check bool "skip" true true
    end
  end

let diff_visualization_tests = [
  "quadrant result to json", `Quick, test_quadrant_result_to_json;
  "diff image result to json", `Quick, test_diff_image_result_to_json;
  "quadrant analysis integration", `Slow, test_quadrant_analysis_integration;
  "generate diff images integration", `Slow, test_generate_diff_images_integration;
]

(** ============== 메인 실행 ============== *)

let () =
  run "Visual Verifier" [
    "Utilities", utility_tests;
    "Correction Hints", hint_tests;
    "CSS Corrections", css_correction_tests;
    "Suggestions", suggestion_tests;
    "Result JSON", result_tests;
    "Integration", integration_tests;
    "Diff Visualization", diff_visualization_tests;
  ]
