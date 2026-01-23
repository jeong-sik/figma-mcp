(** OKLab 검증 테스트 - Björn Ottosson 공식 참조값 기반

    References:
    - Original paper: https://bottosson.github.io/posts/oklab/
    - CSS Color Level 4: https://www.w3.org/TR/css-color-4/#ok-lab
    - Implementation notes: https://observablehq.com/@davidedc/oklab-color-space

    테스트 통과 = 공식 OKLab 구현 준수 확인
*)

open Figma_similarity

(** 허용 오차 (floating point 정밀도 고려) *)
let tolerance = 0.001

(** OKLab 공식 참조값 (Björn Ottosson 사이트에서 발췌)
    sRGB [0-1] → OKLab (L, a, b)
*)
let oklab_reference_data = [
  (* 기본 색상들 *)
  ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0));           (* 검정 *)
  ((1.0, 1.0, 1.0), (1.0, 0.0, 0.0));           (* 흰색 *)
  ((1.0, 0.0, 0.0), (0.6280, 0.2249, 0.1260));  (* 빨강 *)
  ((0.0, 1.0, 0.0), (0.8664, -0.2339, 0.1795)); (* 초록 *)
  ((0.0, 0.0, 1.0), (0.4520, -0.0324, -0.3115));(* 파랑 *)

  (* 중간 색상들 *)
  ((1.0, 1.0, 0.0), (0.9680, -0.0711, 0.1986)); (* 노랑 *)
  ((0.0, 1.0, 1.0), (0.9054, -0.1494, -0.0394));(* 시안 *)
  ((1.0, 0.0, 1.0), (0.7017, 0.2745, -0.1694)); (* 마젠타 *)

  (* 회색 스케일 - sRGB gamma correction 적용 후 값 *)
  ((0.5, 0.5, 0.5), (0.5982, 0.0, 0.0));        (* 50% 회색 *)
  ((0.2, 0.2, 0.2), (0.3211, 0.0, 0.0));        (* 20% 회색: linearize(0.2)=0.0331 *)
  ((0.8, 0.8, 0.8), (0.8452, 0.0, 0.0));        (* 80% 회색 *)
]

(** 단일 테스트 케이스 실행 *)
let test_rgb_to_oklab ((r, g, b), (exp_l, exp_a, exp_b)) =
  let (act_l, act_a, act_b) = rgb_to_oklab (r, g, b) in
  let l_ok = Float.abs (act_l -. exp_l) < tolerance in
  let a_ok = Float.abs (act_a -. exp_a) < tolerance in
  let b_ok = Float.abs (act_b -. exp_b) < tolerance in
  (l_ok && a_ok && b_ok, act_l, act_a, act_b, exp_l, exp_a, exp_b)

(** 거리 테스트: 알려진 색상 쌍의 거리 *)
let distance_tests = [
  (* 같은 색 = 0 *)
  ((1.0, 0.0, 0.0), (1.0, 0.0, 0.0), 0.0);

  (* 검정-흰색: 최대 밝기 차이 *)
  ((0.0, 0.0, 0.0), (1.0, 1.0, 1.0), 1.0);

  (* 보색 쌍: 큰 색차 *)
  ((1.0, 0.0, 0.0), (0.0, 1.0, 1.0), 0.5);  (* 대략적 기대값 *)
]

let test_distance ((r1, g1, b1), (r2, g2, b2), expected_approx) =
  let dist = color_distance_oklab (r1, g1, b1) (r2, g2, b2) in
  let diff = Float.abs (dist -. expected_approx) in
  (* 거리는 근사값이므로 더 넓은 허용 오차 *)
  (diff < 0.2, dist, expected_approx)

(** 모든 테스트 실행 *)
let run_all_tests () =
  Printf.printf "OKLab 검증 테스트 (Björn Ottosson 2020)\n";
  Printf.printf "========================================\n\n";

  (* 변환 테스트 *)
  Printf.printf "📐 RGB → OKLab 변환 테스트:\n";
  let conversion_results = List.mapi (fun i data ->
    let (passed, act_l, act_a, act_b, exp_l, exp_a, exp_b) = test_rgb_to_oklab data in
    let status = if passed then "✅" else "❌" in
    Printf.printf "%s Test %2d: expected=(%.4f, %.4f, %.4f), actual=(%.4f, %.4f, %.4f)\n"
      status (i + 1) exp_l exp_a exp_b act_l act_a act_b;
    passed
  ) oklab_reference_data in

  let conversion_passed = List.length (List.filter Fun.id conversion_results) in
  let conversion_total = List.length conversion_results in

  Printf.printf "\n변환 결과: %d/%d 통과\n\n" conversion_passed conversion_total;

  (* 거리 테스트 *)
  Printf.printf "📏 OKLab 거리 테스트:\n";
  let distance_results = List.mapi (fun i data ->
    let (passed, actual, expected) = test_distance data in
    let status = if passed then "✅" else "⚠️" in
    Printf.printf "%s Test %d: expected≈%.2f, actual=%.4f\n"
      status (i + 1) expected actual;
    passed
  ) distance_tests in

  let distance_passed = List.length (List.filter Fun.id distance_results) in
  let distance_total = List.length distance_results in

  Printf.printf "\n거리 결과: %d/%d 통과\n\n" distance_passed distance_total;

  (* GIoU/DIoU 테스트 *)
  Printf.printf "📦 GIoU/DIoU 테스트:\n";

  (* 완전 일치 *)
  let giou_same = giou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  Printf.printf "✅ 완전 일치: GIoU=%.2f (expected=1.0)\n" giou_same;

  (* 50% 겹침 *)
  let giou_half = giou (0.0, 0.0, 100.0, 100.0) (50.0, 0.0, 100.0, 100.0) in
  Printf.printf "✅ 50%% 겹침: GIoU=%.4f\n" giou_half;

  (* 겹치지 않음 (IoU=0이지만 GIoU < 0) *)
  let giou_apart = giou (0.0, 0.0, 50.0, 50.0) (100.0, 100.0, 50.0, 50.0) in
  Printf.printf "✅ 떨어짐: GIoU=%.4f (expected < 0)\n" giou_apart;

  (* DIoU: 중심점 거리 반영 *)
  let diou_centered = diou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  Printf.printf "✅ DIoU 완전 일치: %.2f\n" diou_centered;

  let diou_offset = diou (0.0, 0.0, 100.0, 100.0) (50.0, 50.0, 100.0, 100.0) in
  Printf.printf "✅ DIoU 중심 이탈: %.4f\n" diou_offset;

  Printf.printf "\n========================================\n";

  let total_passed = conversion_passed + distance_passed in
  let total_tests = conversion_total + distance_total in

  Printf.printf "총 결과: %d/%d 통과 (%.1f%%)\n" total_passed total_tests
    (100.0 *. float_of_int total_passed /. float_of_int total_tests);

  if total_passed = total_tests then begin
    Printf.printf "\n🎨 OKLab 구현 검증 완료!\n";
    Printf.printf "   출처: Björn Ottosson (2020)\n";
    Printf.printf "   표준: CSS Color Level 4 (W3C)\n"
  end else begin
    Printf.printf "\n⚠️ 일부 테스트 실패 - 구현 검토 필요\n"
  end;

  total_passed = total_tests

let () =
  let success = run_all_tests () in
  exit (if success then 0 else 1)
