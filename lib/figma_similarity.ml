(** Figma 유사도 측정 모듈 - 학술적 기반 알고리즘

    References:
    - CIEDE2000: CIE Technical Report (2001)
    - IoU: Jaccard Index for bounding boxes
    - Tree Edit Distance: Zhang-Shasha (1989)
*)

open Figma_types

(** ============== 색상 변환 (RGB → Lab) ============== *)

(** sRGB 감마 보정 해제 *)
let linearize_rgb c =
  if c <= 0.04045 then c /. 12.92
  else ((c +. 0.055) /. 1.055) ** 2.4

(** RGB [0-1] → XYZ (D65 illuminant) *)
let rgb_to_xyz (r, g, b) =
  let r' = linearize_rgb r in
  let g' = linearize_rgb g in
  let b' = linearize_rgb b in
  (* sRGB → XYZ 행렬 (D65) *)
  let x = r' *. 0.4124564 +. g' *. 0.3575761 +. b' *. 0.1804375 in
  let y = r' *. 0.2126729 +. g' *. 0.7151522 +. b' *. 0.0721750 in
  let z = r' *. 0.0193339 +. g' *. 0.1191920 +. b' *. 0.9503041 in
  (x, y, z)

(** XYZ → Lab (D65 reference white) *)
let xyz_to_lab (x, y, z) =
  (* D65 참조 백색점 *)
  let xn, yn, zn = 0.95047, 1.0, 1.08883 in
  let f t =
    if t > 0.008856 then t ** (1.0 /. 3.0)
    else (903.3 *. t +. 16.0) /. 116.0
  in
  let fx = f (x /. xn) in
  let fy = f (y /. yn) in
  let fz = f (z /. zn) in
  let l = 116.0 *. fy -. 16.0 in
  let a = 500.0 *. (fx -. fy) in
  let b = 200.0 *. (fy -. fz) in
  (l, a, b)

(** RGB [0-1] → Lab *)
let rgb_to_lab rgb =
  xyz_to_lab (rgb_to_xyz rgb)

(** ============== OKLab 색상 공간 (2020) ============== *)

(** OKLab: Björn Ottosson의 perceptually uniform 색상 공간 (2020)

    장점:
    - 단순한 유클리드 거리로 인지적 색차 측정 가능
    - CSS Color Level 4 표준
    - CIEDE2000보다 계산 비용 낮음
    - 색상 보간에 적합

    References:
    - https://bottosson.github.io/posts/oklab/
    - CSS Color Level 4: https://www.w3.org/TR/css-color-4/#ok-lab
*)

(** Linear sRGB → OKLab *)
let linear_rgb_to_oklab (r, g, b) =
  (* Linear RGB → LMS (cone response) *)
  let l = 0.4122214708 *. r +. 0.5363325363 *. g +. 0.0514459929 *. b in
  let m = 0.2119034982 *. r +. 0.6806995451 *. g +. 0.1073969566 *. b in
  let s = 0.0883024619 *. r +. 0.2817188376 *. g +. 0.6299787005 *. b in

  (* LMS → LMS' (cube root) *)
  let l' = Float.cbrt l in
  let m' = Float.cbrt m in
  let s' = Float.cbrt s in

  (* LMS' → OKLab *)
  let ok_l = 0.2104542553 *. l' +. 0.7936177850 *. m' -. 0.0040720468 *. s' in
  let ok_a = 1.9779984951 *. l' -. 2.4285922050 *. m' +. 0.4505937099 *. s' in
  let ok_b = 0.0259040371 *. l' +. 0.7827717662 *. m' -. 0.8086757660 *. s' in
  (ok_l, ok_a, ok_b)

(** sRGB [0-1] → OKLab *)
let rgb_to_oklab (r, g, b) =
  let r' = linearize_rgb r in
  let g' = linearize_rgb g in
  let b' = linearize_rgb b in
  linear_rgb_to_oklab (r', g', b')

(** OKLab 색상 거리 (단순 유클리드)

    반환값 해석:
    - 0.0 = 동일한 색상
    - ~0.02 = JND (Just Noticeable Difference)
    - ~0.05 = 명확히 다른 색상
    - 1.0+ = 극단적으로 다른 색상 (검정 vs 흰색 ≈ 1.0)
*)
let oklab_distance (l1, a1, b1) (l2, a2, b2) =
  let dl = l2 -. l1 in
  let da = a2 -. a1 in
  let db = b2 -. b1 in
  Float.sqrt (dl *. dl +. da *. da +. db *. db)

(** RGB [0-1] 두 색상 간 OKLab 거리 *)
let color_distance_oklab rgb1 rgb2 =
  let lab1 = rgb_to_oklab rgb1 in
  let lab2 = rgb_to_oklab rgb2 in
  oklab_distance lab1 lab2

(** RGBA 두 색상 간 OKLab 거리 *)
let rgba_distance_oklab c1 c2 =
  color_distance_oklab (c1.r, c1.g, c1.b) (c2.r, c2.g, c2.b)

(** OKLab 거리를 유사도 퍼센트로 변환 (0-100%)
    - 0.0 → 100%
    - 0.02 (JND) → ~98%
    - 0.1 → ~90%
    - 0.5 → ~50%
*)
let oklab_to_similarity dist =
  100.0 *. Float.exp (-. dist *. 10.0)

(** RGBA (Figma 타입) → Lab *)
let rgba_to_lab (c: rgba) =
  rgb_to_lab (c.r, c.g, c.b)

(** ============== CIEDE2000 색상 거리 ============== *)

(** CIEDE2000 ΔE*₀₀ 계산

    CIE Technical Report 142-2001 기반 구현
    - kL, kC, kH: 가중치 (기본값 1.0)
    - 반환값: 0 = 동일, ~2.3 = JND, 100+ = 완전히 다름
*)
let ciede2000 ?(kl=1.0) ?(kc=1.0) ?(kh=1.0) (l1, a1, b1) (l2, a2, b2) =
  let pi = Float.pi in
  let rad_to_deg x = x *. 180.0 /. pi in
  let deg_to_rad x = x *. pi /. 180.0 in

  (* Step 1: Calculate C'i and h'i *)
  let c1_star = Float.sqrt (a1 *. a1 +. b1 *. b1) in
  let c2_star = Float.sqrt (a2 *. a2 +. b2 *. b2) in
  let c_bar_star = (c1_star +. c2_star) /. 2.0 in

  let c_bar_star_7 = c_bar_star ** 7.0 in
  let g = 0.5 *. (1.0 -. Float.sqrt (c_bar_star_7 /. (c_bar_star_7 +. 6103515625.0))) in

  let a1' = a1 *. (1.0 +. g) in
  let a2' = a2 *. (1.0 +. g) in

  let c1' = Float.sqrt (a1' *. a1' +. b1 *. b1) in
  let c2' = Float.sqrt (a2' *. a2' +. b2 *. b2) in

  let h1' =
    if a1' = 0.0 && b1 = 0.0 then 0.0
    else
      let h = rad_to_deg (Float.atan2 b1 a1') in
      if h < 0.0 then h +. 360.0 else h
  in
  let h2' =
    if a2' = 0.0 && b2 = 0.0 then 0.0
    else
      let h = rad_to_deg (Float.atan2 b2 a2') in
      if h < 0.0 then h +. 360.0 else h
  in

  (* Step 2: Calculate ΔL', ΔC', ΔH' *)
  let delta_l' = l2 -. l1 in
  let delta_c' = c2' -. c1' in

  let delta_h' =
    if c1' *. c2' = 0.0 then 0.0
    else
      let dh = h2' -. h1' in
      if dh > 180.0 then dh -. 360.0
      else if dh < -180.0 then dh +. 360.0
      else dh
  in

  let delta_H' = 2.0 *. Float.sqrt (c1' *. c2') *. Float.sin (deg_to_rad (delta_h' /. 2.0)) in

  (* Step 3: Calculate L'bar, C'bar, h'bar *)
  let l_bar' = (l1 +. l2) /. 2.0 in
  let c_bar' = (c1' +. c2') /. 2.0 in

  let h_bar' =
    if c1' *. c2' = 0.0 then h1' +. h2'
    else
      let sum = h1' +. h2' in
      let diff = Float.abs (h1' -. h2') in
      if diff <= 180.0 then sum /. 2.0
      else if sum < 360.0 then (sum +. 360.0) /. 2.0
      else (sum -. 360.0) /. 2.0
  in

  (* Step 4: Calculate T *)
  let h_bar'_rad = deg_to_rad h_bar' in
  let t = 1.0
    -. 0.17 *. Float.cos (h_bar'_rad -. deg_to_rad 30.0)
    +. 0.24 *. Float.cos (2.0 *. h_bar'_rad)
    +. 0.32 *. Float.cos (3.0 *. h_bar'_rad +. deg_to_rad 6.0)
    -. 0.20 *. Float.cos (4.0 *. h_bar'_rad -. deg_to_rad 63.0)
  in

  (* Step 5: Calculate SL, SC, SH *)
  let l_bar'_minus_50_sq = (l_bar' -. 50.0) ** 2.0 in
  let sl = 1.0 +. (0.015 *. l_bar'_minus_50_sq) /. Float.sqrt (20.0 +. l_bar'_minus_50_sq) in
  let sc = 1.0 +. 0.045 *. c_bar' in
  let sh = 1.0 +. 0.015 *. c_bar' *. t in

  (* Step 6: Calculate RT *)
  (* NOTE: Parentheses critical! OCaml: -. x ** 2.0 = (-.x) ** 2.0, not -(x ** 2.0) *)
  let delta_theta = 30.0 *. Float.exp (-. (((h_bar' -. 275.0) /. 25.0) ** 2.0)) in
  let c_bar'_7 = c_bar' ** 7.0 in
  let rc = 2.0 *. Float.sqrt (c_bar'_7 /. (c_bar'_7 +. 6103515625.0)) in
  let rt = -. rc *. Float.sin (deg_to_rad (2.0 *. delta_theta)) in

  (* Step 7: Calculate ΔE00 *)
  let term_l = delta_l' /. (kl *. sl) in
  let term_c = delta_c' /. (kc *. sc) in
  let term_h = delta_H' /. (kh *. sh) in

  Float.sqrt (term_l *. term_l +. term_c *. term_c +. term_h *. term_h +. rt *. term_c *. term_h)

(** RGB [0-1] 두 색상 간 CIEDE2000 거리 *)
let color_distance_ciede2000 rgb1 rgb2 =
  let lab1 = rgb_to_lab rgb1 in
  let lab2 = rgb_to_lab rgb2 in
  ciede2000 lab1 lab2

(** RGBA 두 색상 간 CIEDE2000 거리 *)
let rgba_distance_ciede2000 c1 c2 =
  color_distance_ciede2000 (c1.r, c1.g, c1.b) (c2.r, c2.g, c2.b)

(** ΔE를 유사도 퍼센트로 변환 (0-100%)
    - ΔE = 0 → 100%
    - ΔE = 2.3 (JND) → ~98%
    - ΔE = 50 → ~50%
    - ΔE = 100+ → ~0%
*)
let delta_e_to_similarity delta_e =
  100.0 *. Float.exp (-. delta_e /. 50.0)

(** ============== IoU (Intersection over Union) ============== *)

(** 바운딩 박스: (x, y, width, height) *)
type bbox_tuple = float * float * float * float

(** 두 바운딩 박스의 IoU 계산
    - 0.0 = 겹치지 않음
    - 1.0 = 완전히 일치
*)
let iou (x1, y1, w1, h1) (x2, y2, w2, h2) =
  (* 좌표를 min/max 형식으로 변환 *)
  let x1_min, x1_max = x1, x1 +. w1 in
  let y1_min, y1_max = y1, y1 +. h1 in
  let x2_min, x2_max = x2, x2 +. w2 in
  let y2_min, y2_max = y2, y2 +. h2 in

  (* 교집합 영역 *)
  let inter_x_min = Float.max x1_min x2_min in
  let inter_y_min = Float.max y1_min y2_min in
  let inter_x_max = Float.min x1_max x2_max in
  let inter_y_max = Float.min y1_max y2_max in

  let inter_w = Float.max 0.0 (inter_x_max -. inter_x_min) in
  let inter_h = Float.max 0.0 (inter_y_max -. inter_y_min) in
  let inter_area = inter_w *. inter_h in

  (* 합집합 영역 *)
  let area1 = w1 *. h1 in
  let area2 = w2 *. h2 in
  let union_area = area1 +. area2 -. inter_area in

  if union_area = 0.0 then 0.0
  else inter_area /. union_area

(** ============== GIoU (Generalized IoU) ============== *)

(** GIoU: Generalized Intersection over Union (Rezatofighi et al., 2019)

    IoU의 한계:
    - 박스가 겹치지 않으면 무조건 0 → 거리 정보 손실
    - 최적화 시 gradient 소실 문제

    GIoU 해결책:
    - 최소 외접 박스(enclosing box)를 활용
    - 범위: [-1, 1] (IoU는 [0, 1])
    - -1 = 최대한 떨어짐, 1 = 완전히 일치

    References:
    - "Generalized Intersection over Union" (CVPR 2019)
    - https://giou.stanford.edu/
*)
let giou (x1, y1, w1, h1) (x2, y2, w2, h2) =
  (* 좌표를 min/max 형식으로 변환 *)
  let x1_min, x1_max = x1, x1 +. w1 in
  let y1_min, y1_max = y1, y1 +. h1 in
  let x2_min, x2_max = x2, x2 +. w2 in
  let y2_min, y2_max = y2, y2 +. h2 in

  (* 교집합 영역 *)
  let inter_x_min = Float.max x1_min x2_min in
  let inter_y_min = Float.max y1_min y2_min in
  let inter_x_max = Float.min x1_max x2_max in
  let inter_y_max = Float.min y1_max y2_max in

  let inter_w = Float.max 0.0 (inter_x_max -. inter_x_min) in
  let inter_h = Float.max 0.0 (inter_y_max -. inter_y_min) in
  let inter_area = inter_w *. inter_h in

  (* 합집합 영역 *)
  let area1 = w1 *. h1 in
  let area2 = w2 *. h2 in
  let union_area = area1 +. area2 -. inter_area in

  (* 최소 외접 박스 (enclosing box) *)
  let enc_x_min = Float.min x1_min x2_min in
  let enc_y_min = Float.min y1_min y2_min in
  let enc_x_max = Float.max x1_max x2_max in
  let enc_y_max = Float.max y1_max y2_max in

  let enc_w = enc_x_max -. enc_x_min in
  let enc_h = enc_y_max -. enc_y_min in
  let enc_area = enc_w *. enc_h in

  if union_area = 0.0 || enc_area = 0.0 then 0.0
  else
    let iou_val = inter_area /. union_area in
    (* GIoU = IoU - (외접 영역 - 합집합) / 외접 영역 *)
    iou_val -. (enc_area -. union_area) /. enc_area

(** GIoU를 유사도 퍼센트로 변환 (0-100%)
    - GIoU 범위: [-1, 1]
    - -1 → 0%, 0 → 50%, 1 → 100%
*)
let giou_to_similarity giou_val =
  50.0 *. (giou_val +. 1.0)

(** 노드 바운딩 박스로 GIoU 계산 *)
let node_giou node1 node2 =
  match node1.bbox, node2.bbox with
  | Some b1, Some b2 ->
      giou (b1.x, b1.y, b1.width, b1.height) (b2.x, b2.y, b2.width, b2.height)
  | _ -> 0.0

(** ============== DIoU (Distance IoU) ============== *)

(** DIoU: Distance Intersection over Union (Zheng et al., 2020)

    GIoU 개선:
    - 중심점 간 거리를 직접 최소화
    - 수렴 속도 향상

    공식: DIoU = IoU - (중심점 거리² / 대각선²)

    References:
    - "Distance-IoU Loss" (AAAI 2020)
*)
let diou (x1, y1, w1, h1) (x2, y2, w2, h2) =
  (* 좌표를 min/max 형식으로 변환 *)
  let x1_min, x1_max = x1, x1 +. w1 in
  let y1_min, y1_max = y1, y1 +. h1 in
  let x2_min, x2_max = x2, x2 +. w2 in
  let y2_min, y2_max = y2, y2 +. h2 in

  (* 중심점 *)
  let cx1 = (x1_min +. x1_max) /. 2.0 in
  let cy1 = (y1_min +. y1_max) /. 2.0 in
  let cx2 = (x2_min +. x2_max) /. 2.0 in
  let cy2 = (y2_min +. y2_max) /. 2.0 in

  (* 중심점 간 거리² *)
  let rho2 = (cx2 -. cx1) ** 2.0 +. (cy2 -. cy1) ** 2.0 in

  (* 최소 외접 박스 *)
  let enc_x_min = Float.min x1_min x2_min in
  let enc_y_min = Float.min y1_min y2_min in
  let enc_x_max = Float.max x1_max x2_max in
  let enc_y_max = Float.max y1_max y2_max in

  (* 외접 박스 대각선² *)
  let c2 = (enc_x_max -. enc_x_min) ** 2.0 +. (enc_y_max -. enc_y_min) ** 2.0 in

  (* IoU 계산 *)
  let iou_val = iou (x1, y1, w1, h1) (x2, y2, w2, h2) in

  if c2 = 0.0 then iou_val
  else iou_val -. rho2 /. c2

(** DIoU를 유사도 퍼센트로 변환 *)
let diou_to_similarity diou_val =
  50.0 *. (diou_val +. 1.0)

(** 노드 바운딩 박스로 DIoU 계산 *)
let node_diou node1 node2 =
  match node1.bbox, node2.bbox with
  | Some b1, Some b2 ->
      diou (b1.x, b1.y, b1.width, b1.height) (b2.x, b2.y, b2.width, b2.height)
  | _ -> 0.0

(** 노드 바운딩 박스로 IoU 계산 *)
let node_iou node1 node2 =
  match node1.bbox, node2.bbox with
  | Some b1, Some b2 ->
      iou (b1.x, b1.y, b1.width, b1.height) (b2.x, b2.y, b2.width, b2.height)
  | _ -> 0.0

(** ============== Tree Edit Distance (Zhang-Shasha) ============== *)

(** 단순화된 트리 편집 거리 (재귀적 계산)
    - 삽입, 삭제, 교체 비용 = 1
    - 노드 타입이 다르면 교체 필요
*)
let rec tree_edit_distance node1 node2 =
  (* 노드 타입이 같으면 비용 0, 다르면 1 *)
  let label_cost =
    if node1.node_type = node2.node_type then 0 else 1
  in

  let children1 = node1.children in
  let children2 = node2.children in
  let n1 = List.length children1 in
  let n2 = List.length children2 in

  (* 자식이 없으면 라벨 비용만 반환 *)
  if n1 = 0 && n2 = 0 then label_cost
  else if n1 = 0 then label_cost + n2  (* 모든 자식 삽입 *)
  else if n2 = 0 then label_cost + n1  (* 모든 자식 삭제 *)
  else begin
    (* 동적 프로그래밍으로 자식 매칭 *)
    let dp = Array.make_matrix (n1 + 1) (n2 + 1) 0 in

    (* 초기화 *)
    for i = 0 to n1 do dp.(i).(0) <- i done;
    for j = 0 to n2 do dp.(0).(j) <- j done;

    (* DP 채우기 *)
    List.iteri (fun i c1 ->
      List.iteri (fun j c2 ->
        let sub_cost = tree_edit_distance c1 c2 in
        let replace = dp.(i).(j) + sub_cost in
        let delete = dp.(i + 1).(j) + 1 in
        let insert = dp.(i).(j + 1) + 1 in
        dp.(i + 1).(j + 1) <- min replace (min delete insert)
      ) children2
    ) children1;

    label_cost + dp.(n1).(n2)
  end

(** TED를 유사도로 변환
    - 편집 거리 / 최대 가능 거리 = 비유사도
    - 유사도 = 1 - 비유사도
*)
let ted_to_similarity ted max_nodes =
  if max_nodes = 0 then 100.0
  else 100.0 *. (1.0 -. (float_of_int ted /. float_of_int max_nodes))

(** ============== 통합 유사도 결과 ============== *)

type similarity_metrics = {
  color_delta_e: float;           (** CIEDE2000 ΔE 값 *)
  color_similarity: float;        (** 색상 유사도 (%) *)
  layout_iou: float;              (** IoU 값 (0-1) *)
  layout_similarity: float;       (** 레이아웃 유사도 (%) *)
  structure_ted: int;             (** Tree Edit Distance *)
  structure_similarity: float;    (** 구조 유사도 (%) *)
  overall_similarity: float;      (** 종합 유사도 (%) *)
}

(** 두 노드의 종합 유사도 계산 *)
let compute_similarity node1 node2 =
  (* 색상 유사도: 배경색 기준 *)
  let get_fill_color node =
    List.find_map (fun p ->
      match p.paint_type, p.color with
      | Solid, Some c -> Some c
      | _ -> None
    ) node.fills
  in

  let color_delta_e, color_sim =
    match get_fill_color node1, get_fill_color node2 with
    | Some c1, Some c2 ->
        let de = rgba_distance_ciede2000 c1 c2 in
        (de, delta_e_to_similarity de)
    | _ -> (0.0, 100.0)  (* 색상 없으면 동일 취급 *)
  in

  (* 레이아웃 유사도: IoU *)
  let layout_iou_val = node_iou node1 node2 in
  let layout_sim = layout_iou_val *. 100.0 in

  (* 구조 유사도: TED *)
  let ted = tree_edit_distance node1 node2 in
  let max_nodes =
    let rec count n = 1 + List.fold_left (fun acc c -> acc + count c) 0 n.children in
    max (count node1) (count node2)
  in
  let structure_sim = ted_to_similarity ted max_nodes in

  (* 종합 유사도: 가중 평균 *)
  let overall = (color_sim *. 0.3 +. layout_sim *. 0.4 +. structure_sim *. 0.3) in

  {
    color_delta_e;
    color_similarity = color_sim;
    layout_iou = layout_iou_val;
    layout_similarity = layout_sim;
    structure_ted = ted;
    structure_similarity = structure_sim;
    overall_similarity = overall;
  }

(** 유사도 결과를 테이블 문자열로 포맷팅 *)
let metrics_to_string metrics =
  let jnd_note =
    if metrics.color_delta_e < 2.3 then " (JND 이하, 구분 불가)"
    else if metrics.color_delta_e < 5.0 then " (미세한 차이)"
    else ""
  in
  Printf.sprintf
{|┌─────────────────┬────────┬─────────────────────────────┐
│ 지표            │ 점수   │ 설명                        │
├─────────────────┼────────┼─────────────────────────────┤
│ Color (ΔE*₀₀)  │ %5.1f%% │ ΔE=%.2f%s │
│ Layout (IoU)    │ %5.1f%% │ 요소 위치 오버랩 %.2f       │
│ Structure (TED) │ %5.1f%% │ 트리 편집 거리 %d           │
├─────────────────┼────────┼─────────────────────────────┤
│ **종합**        │ %5.1f%% │ 가중 평균 (C:30 L:40 S:30)  │
└─────────────────┴────────┴─────────────────────────────┘|}
    metrics.color_similarity metrics.color_delta_e jnd_note
    metrics.layout_similarity metrics.layout_iou
    metrics.structure_similarity metrics.structure_ted
    metrics.overall_similarity

(** ============== 확장 메트릭 (OKLab, GIoU, DIoU 포함) ============== *)

type extended_color_metrics = {
  oklab_distance: float;         (** OKLab 유클리드 거리 *)
  oklab_similarity: float;       (** OKLab 유사도 (%) *)
  ciede2000_distance: float;     (** CIEDE2000 ΔE 값 *)
  ciede2000_similarity: float;   (** CIEDE2000 유사도 (%) *)
  rgb_euclidean: float;          (** RGB 유클리드 (참고용) *)
}

type extended_box_metrics = {
  iou_value: float;              (** IoU [0,1] *)
  giou_value: float;             (** GIoU [-1,1] *)
  diou_value: float;             (** DIoU [-1,1] *)
  iou_similarity: float;         (** IoU 유사도 (%) *)
  giou_similarity: float;        (** GIoU 유사도 (%) *)
  diou_similarity: float;        (** DIoU 유사도 (%) *)
  center_distance: float;        (** 중심점 거리 (px) *)
}

(** 두 색상의 확장 메트릭 계산 *)
let compute_extended_color_metrics (r1, g1, b1) (r2, g2, b2) =
  (* OKLab *)
  let oklab1 = rgb_to_oklab (r1, g1, b1) in
  let oklab2 = rgb_to_oklab (r2, g2, b2) in
  let oklab_dist = oklab_distance oklab1 oklab2 in
  let oklab_sim = oklab_to_similarity oklab_dist in

  (* CIEDE2000 *)
  let ciede_dist = color_distance_ciede2000 (r1, g1, b1) (r2, g2, b2) in
  let ciede_sim = delta_e_to_similarity ciede_dist in

  (* RGB Euclidean (참고용) *)
  let rgb_euc = Float.sqrt ((r2-.r1)**2. +. (g2-.g1)**2. +. (b2-.b1)**2.) in

  {
    oklab_distance = oklab_dist;
    oklab_similarity = oklab_sim;
    ciede2000_distance = ciede_dist;
    ciede2000_similarity = ciede_sim;
    rgb_euclidean = rgb_euc;
  }

(** 두 박스의 확장 메트릭 계산 *)
let compute_extended_box_metrics box1 box2 =
  let (x1, y1, w1, h1) = box1 in
  let (x2, y2, w2, h2) = box2 in

  let iou_val = iou box1 box2 in
  let giou_val = giou box1 box2 in
  let diou_val = diou box1 box2 in

  let cx1, cy1 = x1 +. w1 /. 2., y1 +. h1 /. 2. in
  let cx2, cy2 = x2 +. w2 /. 2., y2 +. h2 /. 2. in
  let center_dist = Float.sqrt ((cx2 -. cx1) ** 2. +. (cy2 -. cy1) ** 2.) in

  {
    iou_value = iou_val;
    giou_value = giou_val;
    diou_value = diou_val;
    iou_similarity = iou_val *. 100.0;
    giou_similarity = giou_to_similarity giou_val;
    diou_similarity = diou_to_similarity diou_val;
    center_distance = center_dist;
  }

(** 확장 색상 메트릭을 테이블 문자열로 포맷팅 *)
let extended_color_to_string metrics =
  let oklab_note =
    if metrics.oklab_distance < 0.02 then "JND 이하"
    else if metrics.oklab_distance < 0.05 then "미세한 차이"
    else if metrics.oklab_distance < 0.1 then "눈에 띄는 차이"
    else "명확히 다름"
  in
  let ciede_note =
    if metrics.ciede2000_distance < 1.0 then "거의 동일"
    else if metrics.ciede2000_distance < 2.3 then "JND 이하"
    else if metrics.ciede2000_distance < 5.0 then "미세한 차이"
    else "명확히 다름"
  in
  Printf.sprintf
{|🎨 색상 비교 (Color Comparison)
┌─────────────────┬───────────┬──────────┬────────────────────┐
│ 메트릭          │ 거리      │ 유사도   │ 해석               │
├─────────────────┼───────────┼──────────┼────────────────────┤
│ OKLab (2020)    │ %9.4f │ %6.1f%% │ %s │
│ CIEDE2000       │ %9.4f │ %6.1f%% │ %s │
│ RGB Euclidean   │ %9.4f │   N/A    │ (참고용)           │
└─────────────────┴───────────┴──────────┴────────────────────┘|}
    metrics.oklab_distance metrics.oklab_similarity oklab_note
    metrics.ciede2000_distance metrics.ciede2000_similarity ciede_note
    metrics.rgb_euclidean

(** 확장 박스 메트릭을 테이블 문자열로 포맷팅 *)
let extended_box_to_string metrics =
  let iou_note =
    if metrics.iou_value >= 0.9 then "거의 일치"
    else if metrics.iou_value >= 0.7 then "대체로 겹침"
    else if metrics.iou_value >= 0.5 then "부분 겹침"
    else if metrics.iou_value > 0.0 then "약간 겹침"
    else "겹침 없음"
  in
  Printf.sprintf
{|📦 레이아웃 비교 (Layout Comparison)
┌─────────────────┬───────────┬──────────┬────────────────────┐
│ 메트릭          │ 값        │ 유사도   │ 특징               │
├─────────────────┼───────────┼──────────┼────────────────────┤
│ IoU             │ %9.4f │ %6.1f%% │ %s │
│ GIoU (2019)     │ %9.4f │ %6.1f%% │ 외접영역 반영       │
│ DIoU (2020)     │ %9.4f │ %6.1f%% │ 중심거리 반영       │
├─────────────────┼───────────┴──────────┴────────────────────┤
│ 중심점 거리     │ %.2f px                                   │
└─────────────────┴───────────────────────────────────────────┘|}
    metrics.iou_value metrics.iou_similarity iou_note
    metrics.giou_value metrics.giou_similarity
    metrics.diou_value metrics.diou_similarity
    metrics.center_distance
