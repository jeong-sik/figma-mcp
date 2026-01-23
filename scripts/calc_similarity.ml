(** 유사도 계산 CLI 도구

    Usage:
      # 색상 비교 (hex 또는 rgb)
      dune exec ./scripts/calc_similarity.exe -- color "#FF0000" "#00FF00"
      dune exec ./scripts/calc_similarity.exe -- color "rgb(255,0,0)" "rgb(0,255,0)"

      # 박스 비교 (x,y,w,h)
      dune exec ./scripts/calc_similarity.exe -- box "0,0,100,100" "50,50,100,100"

      # 전체 비교
      dune exec ./scripts/calc_similarity.exe -- full "#FF0000" "#00FF00" "0,0,100,100" "50,50,100,100"
*)

open Figma_similarity

(** Hex 색상 파싱: "#RRGGBB" → (r, g, b) [0-1] *)
let parse_hex hex =
  let hex = if String.get hex 0 = '#' then String.sub hex 1 (String.length hex - 1) else hex in
  let r = int_of_string ("0x" ^ String.sub hex 0 2) in
  let g = int_of_string ("0x" ^ String.sub hex 2 2) in
  let b = int_of_string ("0x" ^ String.sub hex 4 2) in
  (float_of_int r /. 255.0, float_of_int g /. 255.0, float_of_int b /. 255.0)

(** RGB 문자열 파싱: "rgb(r,g,b)" → (r, g, b) [0-1] *)
let parse_rgb str =
  let re = Str.regexp "rgb(\\([0-9]+\\),[ ]*\\([0-9]+\\),[ ]*\\([0-9]+\\))" in
  if Str.string_match re str 0 then
    let r = int_of_string (Str.matched_group 1 str) in
    let g = int_of_string (Str.matched_group 2 str) in
    let b = int_of_string (Str.matched_group 3 str) in
    (float_of_int r /. 255.0, float_of_int g /. 255.0, float_of_int b /. 255.0)
  else
    failwith ("Invalid RGB format: " ^ str)

(** 색상 문자열 파싱 (hex 또는 rgb) *)
let parse_color str =
  if String.get str 0 = '#' then parse_hex str
  else if String.length str >= 4 && String.sub str 0 3 = "rgb" then parse_rgb str
  else failwith ("Unknown color format: " ^ str ^ " (use #RRGGBB or rgb(r,g,b))")

(** 박스 문자열 파싱: "x,y,w,h" *)
let parse_box str =
  match String.split_on_char ',' str with
  | [x; y; w; h] ->
      (float_of_string (String.trim x),
       float_of_string (String.trim y),
       float_of_string (String.trim w),
       float_of_string (String.trim h))
  | _ -> failwith ("Invalid box format: " ^ str ^ " (use x,y,w,h)")

(** 색상 비교 출력 *)
let compare_colors c1 c2 =
  let (r1, g1, b1) = c1 in
  let (r2, g2, b2) = c2 in

  Printf.printf "\n🎨 색상 비교\n";
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
  Printf.printf "Color 1: rgb(%.0f, %.0f, %.0f)\n" (r1 *. 255.) (g1 *. 255.) (b1 *. 255.);
  Printf.printf "Color 2: rgb(%.0f, %.0f, %.0f)\n" (r2 *. 255.) (g2 *. 255.) (b2 *. 255.);
  Printf.printf "\n";

  (* OKLab *)
  let oklab1 = rgb_to_oklab c1 in
  let oklab2 = rgb_to_oklab c2 in
  let oklab_dist = oklab_distance oklab1 oklab2 in
  let oklab_sim = oklab_to_similarity oklab_dist in

  Printf.printf "┌────────────────┬──────────┬──────────┬─────────────────────────┐\n";
  Printf.printf "│ 메트릭         │ 거리     │ 유사도   │ 해석                    │\n";
  Printf.printf "├────────────────┼──────────┼──────────┼─────────────────────────┤\n";

  let oklab_note =
    if oklab_dist < 0.02 then "JND 이하, 구분 불가"
    else if oklab_dist < 0.05 then "미세한 차이"
    else if oklab_dist < 0.1 then "눈에 띄는 차이"
    else "명확히 다름"
  in
  Printf.printf "│ OKLab          │ %8.4f │ %6.1f%% │ %-23s │\n"
    oklab_dist oklab_sim oklab_note;

  (* CIEDE2000 *)
  let ciede_dist = color_distance_ciede2000 c1 c2 in
  let ciede_sim = delta_e_to_similarity ciede_dist in
  let ciede_note =
    if ciede_dist < 1.0 then "거의 동일"
    else if ciede_dist < 2.3 then "JND 이하"
    else if ciede_dist < 5.0 then "미세한 차이"
    else "명확히 다름"
  in
  Printf.printf "│ CIEDE2000      │ %8.4f │ %6.1f%% │ %-23s │\n"
    ciede_dist ciede_sim ciede_note;

  (* 단순 유클리드 (참고용) *)
  let euclidean = Float.sqrt ((r2-.r1)**2. +. (g2-.g1)**2. +. (b2-.b1)**2.) in
  let euclidean_sim = 100.0 *. (1.0 -. euclidean /. Float.sqrt 3.0) in
  Printf.printf "│ RGB Euclidean  │ %8.4f │ %6.1f%% │ (참고용, 비인지적)      │\n"
    euclidean euclidean_sim;

  Printf.printf "└────────────────┴──────────┴──────────┴─────────────────────────┘\n";

  Printf.printf "\n📊 OKLab 좌표:\n";
  let (l1, a1, b1) = oklab1 in
  let (l2, a2, b2) = oklab2 in
  Printf.printf "   Color 1: L=%.4f, a=%.4f, b=%.4f\n" l1 a1 b1;
  Printf.printf "   Color 2: L=%.4f, a=%.4f, b=%.4f\n" l2 a2 b2

(** 박스 비교 출력 *)
let compare_boxes box1 box2 =
  let (x1, y1, w1, h1) = box1 in
  let (x2, y2, w2, h2) = box2 in

  Printf.printf "\n📦 박스 비교\n";
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
  Printf.printf "Box 1: (%.0f, %.0f) %.0f×%.0f\n" x1 y1 w1 h1;
  Printf.printf "Box 2: (%.0f, %.0f) %.0f×%.0f\n" x2 y2 w2 h2;
  Printf.printf "\n";

  let iou_val = iou box1 box2 in
  let giou_val = giou box1 box2 in
  let diou_val = diou box1 box2 in

  Printf.printf "┌────────────────┬──────────┬──────────┬─────────────────────────┐\n";
  Printf.printf "│ 메트릭         │ 값       │ 유사도   │ 특징                    │\n";
  Printf.printf "├────────────────┼──────────┼──────────┼─────────────────────────┤\n";

  Printf.printf "│ IoU            │ %8.4f │ %6.1f%% │ 겹침 비율 [0,1]         │\n"
    iou_val (iou_val *. 100.);

  Printf.printf "│ GIoU           │ %8.4f │ %6.1f%% │ 떨어진 정도 [-1,1]      │\n"
    giou_val (giou_to_similarity giou_val);

  Printf.printf "│ DIoU           │ %8.4f │ %6.1f%% │ 중심점 거리 [-1,1]      │\n"
    diou_val (diou_to_similarity diou_val);

  Printf.printf "└────────────────┴──────────┴──────────┴─────────────────────────┘\n";

  (* 추가 정보 *)
  let area1 = w1 *. h1 in
  let area2 = w2 *. h2 in
  let cx1, cy1 = x1 +. w1 /. 2., y1 +. h1 /. 2. in
  let cx2, cy2 = x2 +. w2 /. 2., y2 +. h2 /. 2. in
  let center_dist = Float.sqrt ((cx2 -. cx1) ** 2. +. (cy2 -. cy1) ** 2.) in

  Printf.printf "\n📊 상세 정보:\n";
  Printf.printf "   면적: %.0f vs %.0f (비율 %.2f)\n" area1 area2 (area1 /. area2);
  Printf.printf "   중심점: (%.1f, %.1f) vs (%.1f, %.1f)\n" cx1 cy1 cx2 cy2;
  Printf.printf "   중심점 거리: %.2f px\n" center_dist

(** 사용법 출력 *)
let print_usage () =
  Printf.printf {|
유사도 계산 CLI 도구
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Usage:
  dune exec ./scripts/calc_similarity.exe -- <command> <args>

Commands:
  color <color1> <color2>
    색상 비교 (OKLab, CIEDE2000)
    색상 형식: #RRGGBB 또는 rgb(r,g,b)

  box <box1> <box2>
    박스 비교 (IoU, GIoU, DIoU)
    박스 형식: x,y,w,h

  full <color1> <color2> <box1> <box2>
    전체 비교

Examples:
  # 빨강 vs 초록 비교
  ... -- color "#FF0000" "#00FF00"

  # rgb 형식
  ... -- color "rgb(255,0,0)" "rgb(0,128,0)"

  # 박스 비교
  ... -- box "0,0,100,100" "50,50,100,100"

  # 전체
  ... -- full "#FF0000" "#00FF00" "0,0,100,100" "50,50,100,100"

References:
  - OKLab: Björn Ottosson (2020), CSS Color Level 4
  - CIEDE2000: CIE Technical Report 142-2001
  - GIoU: Rezatofighi et al., CVPR 2019
  - DIoU: Zheng et al., AAAI 2020
|}

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | ["color"; c1; c2] ->
      let color1 = parse_color c1 in
      let color2 = parse_color c2 in
      compare_colors color1 color2

  | ["box"; b1; b2] ->
      let box1 = parse_box b1 in
      let box2 = parse_box b2 in
      compare_boxes box1 box2

  | ["full"; c1; c2; b1; b2] ->
      let color1 = parse_color c1 in
      let color2 = parse_color c2 in
      let box1 = parse_box b1 in
      let box2 = parse_box b2 in
      compare_colors color1 color2;
      compare_boxes box1 box2

  | _ -> print_usage ()
