(** Semantic Verifier - "Meaning-first" design verification.

    Goal: catch obvious layout/text/style regressions with deterministic checks
    before running pixel-based SSIM verification (which can be flaky due to fonts/AA).

    Inputs:
    - Figma DSL node tree (Figma_types.ui_node) with absoluteBoundingBox
    - HTML metrics extracted from Playwright (Html_metrics.t)

    Outputs:
    - passed/score + mismatch list with concrete deltas
*)

open Figma_types

type config = {
  score_threshold: float;
  root_size_tol_px: float;
  root_bg_tol_rgb: float;
  root_radius_tol_px: float;
  text_bbox_tol_px: float;
  font_size_tol_px: float;
  font_weight_tol: int;
  text_color_tol_rgb: float;
}

let default_config = {
  score_threshold = 0.90;
  root_size_tol_px = 2.5;
  root_bg_tol_rgb = 0.10;      (* normalized RGB distance (0..1) *)
  root_radius_tol_px = 2.5;
  text_bbox_tol_px = 4.0;
  font_size_tol_px = 1.5;
  font_weight_tol = 150;
  text_color_tol_rgb = 0.15;
}

type mismatch =
  | RootSizeMismatch of { expected_w: float; expected_h: float; actual_w: float; actual_h: float; dw: float; dh: float }
  | RootBackgroundMismatch of { expected: rgba; actual: rgba; dist: float }
  | RootBorderRadiusMismatch of { expected_px: float; actual_px: float; delta_px: float }
  | MissingText of { text: string }
  | TextBBoxMismatch of { text: string; expected: bbox; actual: bbox; dx: float; dy: float; dw: float; dh: float }
  | TextFontSizeMismatch of { text: string; expected_px: float; actual_px: float; delta_px: float }
  | TextFontWeightMismatch of { text: string; expected: int; actual: int; delta: int }
  | TextColorMismatch of { text: string; expected: rgba; actual: rgba; dist: float }

type t = {
  passed: bool;
  score: float;
  root_expected: bbox;
  root_actual: bbox;
  total_texts: int;
  matched_texts: int;
  mismatches: mismatch list;
}

let re_whitespace = Str.regexp "[ \t\n\r]+"

let normalize_text s =
  let s = Str.global_replace re_whitespace " " s in
  String.trim s

let rgba_dist_rgb (a: rgba) (b: rgba) : float =
  let dr = a.r -. b.r in
  let dg = a.g -. b.g in
  let db = a.b -. b.b in
  sqrt (dr *. dr +. dg *. dg +. db *. db)

let bbox_area (b: bbox) = b.width *. b.height

let bbox_center (b: bbox) = (b.x +. (b.width /. 2.0), b.y +. (b.height /. 2.0))

let bbox_delta ~expected ~actual =
  (actual.x -. expected.x, actual.y -. expected.y, actual.width -. expected.width, actual.height -. expected.height)

let absf = Float.abs

let bbox_within ~tol_px ~expected ~actual =
  let dx, dy, dw, dh = bbox_delta ~expected ~actual in
  absf dx <= tol_px && absf dy <= tol_px && absf dw <= tol_px && absf dh <= tol_px

let normalize_bbox ~origin (b: bbox) : bbox =
  { b with x = b.x -. origin.x; y = b.y -. origin.y }

let pick_root_bbox (m: Html_metrics.t) : bbox =
  match m.root with
  | Some r -> r.bbox
  | None -> { x = 0.; y = 0.; width = float m.viewport_width; height = float m.viewport_height }

let solid_color_of_paints paints =
  let rec loop = function
    | [] -> None
    | (p: paint) :: rest ->
        if p.visible && p.paint_type = Solid then p.color else loop rest
  in
  loop paints

type expected_text = {
  text: string;
  bbox: bbox;  (* root-relative *)
  font_size_px: float option;
  font_weight: int option;
  color: rgba option;
}

let extract_expected_texts ~(root_bbox: bbox) (node: ui_node) : expected_text list =
  let rec loop acc (n: ui_node) =
    let acc =
      match n.node_type, n.characters, n.bbox with
      | Text, Some chars, Some b when String.trim chars <> "" ->
          let rel = normalize_bbox ~origin:root_bbox b in
          let font_size_px = Option.map (fun (t: typography) -> t.font_size) n.typography in
          let font_weight = Option.map (fun (t: typography) -> t.font_weight) n.typography in
          let color = solid_color_of_paints n.fills in
          { text = chars; bbox = rel; font_size_px; font_weight; color } :: acc
      | _ -> acc
    in
    List.fold_left loop acc n.children
  in
  loop [] node |> List.rev

let choose_best_text_candidate ~(expected: expected_text) ~(origin: bbox) (cands: Html_metrics.text_node list) : Html_metrics.text_node option =
  let exp_norm = normalize_text expected.text in
  let score_text (cand: Html_metrics.text_node) =
    let act_norm = normalize_text cand.text in
    if act_norm = exp_norm then 2
    else if String.length exp_norm > 2 then
      (try ignore (Str.search_forward (Str.regexp_string exp_norm) act_norm 0); 1 with Not_found -> 0)
    else 0
  in
  let exp_center = bbox_center expected.bbox in
  let dist (cand: Html_metrics.text_node) =
    let rel = normalize_bbox ~origin cand.Html_metrics.bbox in
    let cx, cy = bbox_center rel in
    let ex, ey = exp_center in
    absf (cx -. ex) +. absf (cy -. ey)
  in
  cands
  |> List.filter (fun c -> score_text c > 0)
  |> (fun xs ->
      match xs with
      | [] -> None
      | _ ->
          let best =
            xs
            |> List.sort (fun a b ->
                let sa = score_text a and sb = score_text b in
                if sa <> sb then compare sb sa
                else compare (dist a) (dist b))
            |> List.hd
          in
          Some best)

let verify ?(config=default_config) ~(dsl_node: ui_node) ~(html: Html_metrics.t) () : (t, string) result =
  match dsl_node.bbox with
  | None -> Error "DSL root node is missing absoluteBoundingBox"
  | Some root_bbox_abs ->
      let expected_root = root_bbox_abs in
      let actual_root = pick_root_bbox html in

      let mismatches = ref [] in

      (* Root: size *)
      let dw = actual_root.width -. expected_root.width in
      let dh = actual_root.height -. expected_root.height in
      if absf dw > config.root_size_tol_px || absf dh > config.root_size_tol_px then
        mismatches :=
          RootSizeMismatch {
            expected_w = expected_root.width; expected_h = expected_root.height;
            actual_w = actual_root.width; actual_h = actual_root.height;
            dw; dh;
          } :: !mismatches;

      (* Root: background + border radius (best-effort) *)
      let expected_bg = solid_color_of_paints dsl_node.fills in
      let actual_bg =
        match html.root with
        | Some r -> r.background
        | None -> None
      in
      (match expected_bg, actual_bg with
       | Some e, Some a ->
           let dist = rgba_dist_rgb e a in
           if dist > config.root_bg_tol_rgb then
             mismatches := RootBackgroundMismatch { expected = e; actual = a; dist } :: !mismatches
       | _ -> ());

      (match html.root with
       | Some r ->
           let expected_r = dsl_node.border_radius in
           (match r.border_radius_px with
            | Some actual_r ->
                let delta = actual_r -. expected_r in
                if absf delta > config.root_radius_tol_px then
                  mismatches := RootBorderRadiusMismatch { expected_px = expected_r; actual_px = actual_r; delta_px = delta } :: !mismatches
            | None -> ())
       | None -> ());

      (* Text checks *)
      let expected_texts = extract_expected_texts ~root_bbox:expected_root dsl_node in
      let total_texts = List.length expected_texts in
      let matched = ref 0 in

      List.iter (fun exp ->
        match choose_best_text_candidate ~expected:exp ~origin:actual_root html.text_nodes with
        | None ->
            mismatches := MissingText { text = exp.text } :: !mismatches
        | Some cand ->
            incr matched;
            let actual_rel = normalize_bbox ~origin:actual_root cand.bbox in

            if not (bbox_within ~tol_px:config.text_bbox_tol_px ~expected:exp.bbox ~actual:actual_rel) then begin
              let dx, dy, dw, dh = bbox_delta ~expected:exp.bbox ~actual:actual_rel in
              mismatches := TextBBoxMismatch { text = exp.text; expected = exp.bbox; actual = actual_rel; dx; dy; dw; dh } :: !mismatches
            end;

            (match exp.font_size_px, cand.font_size_px with
             | Some e, Some a ->
                 let delta = a -. e in
                 if absf delta > config.font_size_tol_px then
                   mismatches := TextFontSizeMismatch { text = exp.text; expected_px = e; actual_px = a; delta_px = delta } :: !mismatches
             | _ -> ());

            (match exp.font_weight, cand.font_weight with
             | Some e, Some a ->
                 let delta = a - e in
                 if abs delta > config.font_weight_tol then
                   mismatches := TextFontWeightMismatch { text = exp.text; expected = e; actual = a; delta } :: !mismatches
             | _ -> ());

            (match exp.color, cand.color with
             | Some e, Some a ->
                 let dist = rgba_dist_rgb e a in
                 if dist > config.text_color_tol_rgb then
                   mismatches := TextColorMismatch { text = exp.text; expected = e; actual = a; dist } :: !mismatches
             | _ -> ())
      ) expected_texts;

      (* Simple score: penalize mismatches by category *)
      let miss_count = List.length (List.filter (function MissingText _ -> true | _ -> false) !mismatches) in
      let bbox_count = List.length (List.filter (function TextBBoxMismatch _ -> true | _ -> false) !mismatches) in
      let style_count =
        List.length !mismatches - miss_count - bbox_count
      in
      let score =
        1.0
        -. (float miss_count *. 0.20)
        -. (float bbox_count *. 0.05)
        -. (float style_count *. 0.03)
        |> max 0.0
      in
      let passed =
        score >= config.score_threshold && miss_count = 0
      in

      Ok {
        passed;
        score;
        root_expected = expected_root;
        root_actual = actual_root;
        total_texts;
        matched_texts = !matched;
        mismatches = List.rev !mismatches;
      }

let bbox_to_json (b: bbox) =
  `Assoc [
    ("x", `Float b.x);
    ("y", `Float b.y);
    ("width", `Float b.width);
    ("height", `Float b.height);
  ]

let rgba_to_json (c: rgba) =
  `Assoc [
    ("r", `Float c.r);
    ("g", `Float c.g);
    ("b", `Float c.b);
    ("a", `Float c.a);
  ]

let mismatch_to_json = function
  | RootSizeMismatch { expected_w; expected_h; actual_w; actual_h; dw; dh } ->
      `Assoc [
        ("type", `String "root_size");
        ("expected", `Assoc [("w", `Float expected_w); ("h", `Float expected_h)]);
        ("actual", `Assoc [("w", `Float actual_w); ("h", `Float actual_h)]);
        ("delta", `Assoc [("dw", `Float dw); ("dh", `Float dh)]);
      ]
  | RootBackgroundMismatch { expected; actual; dist } ->
      `Assoc [
        ("type", `String "root_background");
        ("expected", rgba_to_json expected);
        ("actual", rgba_to_json actual);
        ("dist", `Float dist);
      ]
  | RootBorderRadiusMismatch { expected_px; actual_px; delta_px } ->
      `Assoc [
        ("type", `String "root_border_radius");
        ("expected_px", `Float expected_px);
        ("actual_px", `Float actual_px);
        ("delta_px", `Float delta_px);
      ]
  | MissingText { text } ->
      `Assoc [("type", `String "missing_text"); ("text", `String text)]
  | TextBBoxMismatch { text; expected; actual; dx; dy; dw; dh } ->
      `Assoc [
        ("type", `String "text_bbox");
        ("text", `String text);
        ("expected", bbox_to_json expected);
        ("actual", bbox_to_json actual);
        ("delta", `Assoc [("dx", `Float dx); ("dy", `Float dy); ("dw", `Float dw); ("dh", `Float dh)]);
      ]
  | TextFontSizeMismatch { text; expected_px; actual_px; delta_px } ->
      `Assoc [
        ("type", `String "text_font_size");
        ("text", `String text);
        ("expected_px", `Float expected_px);
        ("actual_px", `Float actual_px);
        ("delta_px", `Float delta_px);
      ]
  | TextFontWeightMismatch { text; expected; actual; delta } ->
      `Assoc [
        ("type", `String "text_font_weight");
        ("text", `String text);
        ("expected", `Int expected);
        ("actual", `Int actual);
        ("delta", `Int delta);
      ]
  | TextColorMismatch { text; expected; actual; dist } ->
      `Assoc [
        ("type", `String "text_color");
        ("text", `String text);
        ("expected", rgba_to_json expected);
        ("actual", rgba_to_json actual);
        ("dist", `Float dist);
      ]

let result_to_json (r: t) =
  `Assoc [
    ("passed", `Bool r.passed);
    ("score", `Float r.score);
    ("root_expected", bbox_to_json r.root_expected);
    ("root_actual", bbox_to_json r.root_actual);
    ("texts", `Assoc [
      ("total", `Int r.total_texts);
      ("matched", `Int r.matched_texts);
    ]);
    ("mismatches", `List (List.map mismatch_to_json r.mismatches));
  ]
