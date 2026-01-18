(** Figma 디자인 토큰 추출 *)

open Figma_types

(** ============== 토큰 타입 ============== *)

type color_token = {
  name: string;
  hex: string;
  rgba: string;
}

type typography_token = {
  name: string;
  font_family: string;
  font_size: float;
  font_weight: int;
  line_height: float option;
  letter_spacing: float option;
}

type spacing_token = {
  name: string;
  value: float;
}

type radius_token = {
  name: string;
  value: float;
}

type design_tokens = {
  colors: color_token list;
  typography: typography_token list;
  spacing: spacing_token list;
  radii: radius_token list;
}

(** ============== 토큰 추출 ============== *)

let extract_colors nodes =
  let seen = Hashtbl.create 32 in
  List.filter_map (fun node ->
    List.find_map (fun paint ->
      match paint.paint_type, paint.color with
      | Solid, Some c ->
          let hex = Printf.sprintf "#%02X%02X%02X"
            (int_of_float (c.r *. 255.))
            (int_of_float (c.g *. 255.))
            (int_of_float (c.b *. 255.))
          in
          if Hashtbl.mem seen hex then None
          else begin
            Hashtbl.add seen hex true;
            let name = Printf.sprintf "color-%s" (String.sub hex 1 6 |> String.lowercase_ascii) in
            Some {
              name;
              hex;
              rgba = Printf.sprintf "rgba(%.0f, %.0f, %.0f, %.2f)"
                (c.r *. 255.) (c.g *. 255.) (c.b *. 255.) c.a;
            }
          end
      | _ -> None
    ) node.fills
  ) nodes

let extract_typography nodes =
  let seen = Hashtbl.create 16 in
  List.filter_map (fun (node: ui_node) ->
    match node.typography with
    | Some t ->
        let key = Printf.sprintf "%s-%d-%.0f" t.font_family t.font_weight t.font_size in
        if Hashtbl.mem seen key then None
        else begin
          Hashtbl.add seen key true;
          let name = Printf.sprintf "text-%s-%d"
            (String.lowercase_ascii (Str.global_replace (Str.regexp " ") "-" t.font_family))
            (int_of_float t.font_size)
          in
          Some {
            name;
            font_family = t.font_family;
            font_size = t.font_size;
            font_weight = t.font_weight;
            line_height = t.line_height;
            letter_spacing = t.letter_spacing;
          }
        end
    | None -> None
  ) nodes

let extract_spacing nodes : spacing_token list =
  let seen = Hashtbl.create 16 in
  let add_if_new value : spacing_token option =
    if value > 0. && not (Hashtbl.mem seen value) then begin
      Hashtbl.add seen value true;
      Some { name = Printf.sprintf "space-%.0f" value; value }
    end
    else None
  in

  List.filter_map (fun node ->
    let gap_token = add_if_new node.gap in
    let (t, r, b, l) = node.padding in
    let pad_tokens = List.filter_map add_if_new [t; r; b; l] in
    Some (Option.to_list gap_token @ pad_tokens)
  ) nodes
  |> List.concat
  |> List.sort_uniq (fun (a: spacing_token) (b: spacing_token) -> compare a.value b.value)

let extract_radii nodes : radius_token list =
  let seen = Hashtbl.create 16 in
  let add_radius v : radius_token option =
    if v > 0. && not (Hashtbl.mem seen v) then begin
      Hashtbl.add seen v true;
      Some { name = Printf.sprintf "radius-%.0f" v; value = v }
    end
    else None
  in
  List.filter_map (fun node ->
    match node.border_radii with
    | Some (tl, tr, br, bl) ->
        let values = [tl; tr; br; bl] in
        List.filter_map add_radius values
        |> (function [] -> None | l -> Some l)
    | None -> None
  ) nodes
  |> List.concat
  |> List.sort_uniq (fun (a: radius_token) (b: radius_token) -> compare a.value b.value)

let extract_all nodes = {
  colors = extract_colors nodes;
  typography = extract_typography nodes;
  spacing = extract_spacing nodes;
  radii = extract_radii nodes;
}

(** ============== 출력 포맷 ============== *)

(** CSS Custom Properties *)
let to_css tokens =
  let lines = Buffer.create 512 in
  Buffer.add_string lines ":root {\n";
  Buffer.add_string lines "  /* Colors */\n";
  List.iter (fun (c: color_token) ->
    Buffer.add_string lines (Printf.sprintf "  --%s: %s;\n" c.name c.hex)
  ) tokens.colors;

  Buffer.add_string lines "\n  /* Spacing */\n";
  List.iter (fun (s: spacing_token) ->
    Buffer.add_string lines (Printf.sprintf "  --%s: %.0fpx;\n" s.name s.value)
  ) tokens.spacing;

  Buffer.add_string lines "\n  /* Border Radius */\n";
  List.iter (fun (r: radius_token) ->
    Buffer.add_string lines (Printf.sprintf "  --%s: %.0fpx;\n" r.name r.value)
  ) tokens.radii;

  Buffer.add_string lines "}\n\n/* Typography */\n";
  List.iter (fun (t: typography_token) ->
    Buffer.add_string lines (Printf.sprintf ".%s {\n" t.name);
    Buffer.add_string lines (Printf.sprintf "  font-family: '%s';\n" t.font_family);
    Buffer.add_string lines (Printf.sprintf "  font-size: %.0fpx;\n" t.font_size);
    Buffer.add_string lines (Printf.sprintf "  font-weight: %d;\n" t.font_weight);
    (match t.line_height with
     | Some lh -> Buffer.add_string lines (Printf.sprintf "  line-height: %.2f;\n" lh)
     | None -> ());
    (match t.letter_spacing with
     | Some ls -> Buffer.add_string lines (Printf.sprintf "  letter-spacing: %.2fpx;\n" ls)
     | None -> ());
    Buffer.add_string lines "}\n\n"
  ) tokens.typography;

  Buffer.contents lines

(** Tailwind config *)
let to_tailwind tokens =
  let lines = Buffer.create 512 in
  Buffer.add_string lines "// tailwind.config.js\nmodule.exports = {\n  theme: {\n    extend: {\n";

  Buffer.add_string lines "      colors: {\n";
  List.iter (fun (c: color_token) ->
    let name = String.sub c.hex 1 6 in
    Buffer.add_string lines (Printf.sprintf "        '%s': '%s',\n" name c.hex)
  ) tokens.colors;
  Buffer.add_string lines "      },\n";

  Buffer.add_string lines "      spacing: {\n";
  List.iter (fun (s: spacing_token) ->
    Buffer.add_string lines (Printf.sprintf "        '%.0f': '%.0fpx',\n" s.value s.value)
  ) tokens.spacing;
  Buffer.add_string lines "      },\n";

  Buffer.add_string lines "      borderRadius: {\n";
  List.iter (fun (r: radius_token) ->
    Buffer.add_string lines (Printf.sprintf "        '%.0f': '%.0fpx',\n" r.value r.value)
  ) tokens.radii;
  Buffer.add_string lines "      },\n";

  Buffer.add_string lines "    },\n  },\n}\n";
  Buffer.contents lines

(** JSON 포맷 *)
let to_json tokens =
  let color_json = List.map (fun (c: color_token) ->
    `Assoc [("name", `String c.name); ("hex", `String c.hex); ("rgba", `String c.rgba)]
  ) tokens.colors in

  let typo_json = List.map (fun (t: typography_token) ->
    `Assoc [
      ("name", `String t.name);
      ("fontFamily", `String t.font_family);
      ("fontSize", `Float t.font_size);
      ("fontWeight", `Int t.font_weight);
    ]
  ) tokens.typography in

  let spacing_json = List.map (fun (s: spacing_token) ->
    `Assoc [("name", `String s.name); ("value", `Float s.value)]
  ) tokens.spacing in

  let radii_json = List.map (fun (r: radius_token) ->
    `Assoc [("name", `String r.name); ("value", `Float r.value)]
  ) tokens.radii in

  Yojson.Safe.pretty_to_string (`Assoc [
    ("colors", `List color_json);
    ("typography", `List typo_json);
    ("spacing", `List spacing_json);
    ("borderRadius", `List radii_json);
  ])
