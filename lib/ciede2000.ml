(** CIEDE2000 Color Difference Formula Implementation in OCaml
    Based on the "IsThisColourSimilar" JS implementation and Bruce Lindbloom's references.
*)

(** Reference white (D65) *)
let ref_x = 95.047
let ref_y = 100.0
let ref_z = 108.883

type lab = { l: float; a: float; b: float }
type rgb = { r: float; g: float; b: float }

(** sRGB to XYZ conversion *)
let rgb_to_xyz {r; g; b} =
  let linearize c =
    let c = c /. 255.0 in
    if c > 0.04045 then ((c +. 0.055) /. 1.055) ** 2.4
    else c /. 12.92
  in
  let lr = linearize r *. 100.0 in
  let lg = linearize g *. 100.0 in
  let lb = linearize b *. 100.0 in
  let x = lr *. 0.4124564 +. lg *. 0.3575761 +. lb *. 0.1804375 in
  let y = lr *. 0.2126729 +. lg *. 0.7151522 +. lb *. 0.0721750 in
  let z = lr *. 0.0193339 +. lg *. 0.1191920 +. lb *. 0.9503041 in
  (x, y, z)

(** XYZ to CIELab conversion *)
let xyz_to_lab (x, y, z) =
  let f t =
    if t > 0.008856 then t ** (1.0 /. 3.0)
    else (7.787 *. t) +. (16.0 /. 116.0)
  in
  let fx = f (x /. ref_x) in
  let fy = f (y /. ref_y) in
  let fz = f (z /. ref_z) in
  let l = (116.0 *. fy) -. 16.0 in
  let a = 500.0 *. (fx -. fy) in
  let b = 200.0 *. (fy -. fz) in
  { l; a; b }

(** RGB to CIELab *)
let rgb_to_lab rgb =
  xyz_to_lab (rgb_to_xyz rgb)

(** Helper: radians to degrees *)
let rad2deg rad = rad *. 180.0 /. Float.pi

(** Helper: degrees to radians *)
let deg2rad deg = deg *. Float.pi /. 180.0

(** CIEDE2000 Delta E calculation *)
let delta_e lab1 lab2 =
  let l1, a1, b1 = lab1.l, lab1.a, lab1.b in
  let l2, a2, b2 = lab2.l, lab2.a, lab2.b in

  (* Weighting factors *)
  let kl, kc, kh = 1.0, 1.0, 1.0 in

  (* Step 1 *)
  let c1 = sqrt (a1 ** 2.0 +. b1 ** 2.0) in
  let c2 = sqrt (a2 ** 2.0 +. b2 ** 2.0) in
  let avg_c = (c1 +. c2) /. 2.0 in
  let g = 0.5 *. (1.0 -. sqrt (avg_c ** 7.0 /. (avg_c ** 7.0 +. 25.0 ** 7.0))) in

  (* Step 2 *)
  let a1p = a1 *. (1.0 +. g) in
  let a2p = a2 *. (1.0 +. g) in
  let c1p = sqrt (a1p ** 2.0 +. b1 ** 2.0) in
  let c2p = sqrt (a2p ** 2.0 +. b2 ** 2.0) in

  let h1p =
    if b1 = 0.0 && a1p = 0.0 then 0.0
    else
      let h = rad2deg (atan2 b1 a1p) in
      if h < 0.0 then h +. 360.0 else h
  in
  let h2p =
    if b2 = 0.0 && a2p = 0.0 then 0.0
    else
      let h = rad2deg (atan2 b2 a2p) in
      if h < 0.0 then h +. 360.0 else h
  in

  (* Step 3 *)
  let lp = l2 -. l1 in
  let cp = c2p -. c1p in
  let hp =
    if c1p *. c2p = 0.0 then 0.0
    else
      let diff = h2p -. h1p in
      if abs_float diff <= 180.0 then diff
      else if diff > 180.0 then diff -. 360.0
      else diff +. 360.0
  in
  let h_prime = 2.0 *. sqrt (c1p *. c2p) *. sin (deg2rad (hp /. 2.0)) in

  (* Step 4 *)
  let avg_lp = (l1 +. l2) /. 2.0 in
  let avg_cp = (c1p +. c2p) /. 2.0 in
  let avg_hp =
    if c1p *. c2p = 0.0 then h1p +. h2p
    else
      let diff = abs_float (h1p -. h2p) in
      if diff <= 180.0 then (h1p +. h2p) /. 2.0
      else if h1p +. h2p < 360.0 then (h1p +. h2p +. 360.0) /. 2.0
      else (h1p +. h2p -. 360.0) /. 2.0
  in

  let t = 1.0 -. 0.17 *. cos (deg2rad (avg_hp -. 30.0))
          +. 0.24 *. cos (deg2rad (2.0 *. avg_hp))
          +. 0.32 *. cos (deg2rad (3.0 *. avg_hp +. 6.0))
          -. 0.20 *. cos (deg2rad (4.0 *. avg_hp -. 63.0))
  in

  let sl = 1.0 +. (0.015 *. ((avg_lp -. 50.0) ** 2.0)) /. sqrt (20.0 +. ((avg_lp -. 50.0) ** 2.0)) in
  let sc = 1.0 +. 0.045 *. avg_cp in
  let sh = 1.0 +. 0.015 *. avg_cp *. t in

  (* Step 5 *)
  let delta_theta = 30.0 *. exp (-. (((avg_hp -. 275.0) /. 25.0) ** 2.0)) in
  let rc = 2.0 *. sqrt (avg_cp ** 7.0 /. (avg_cp ** 7.0 +. 25.0 ** 7.0)) in
  let rt = -. rc *. sin (deg2rad (2.0 *. delta_theta)) in

  (* Final calculation *)
  sqrt ((lp /. (kl *. sl)) ** 2.0 +.
        (cp /. (kc *. sc)) ** 2.0 +.
        (h_prime /. (kh *. sh)) ** 2.0 +.
        rt *. (cp /. (kc *. sc)) *. (h_prime /. (kh *. sh)))
