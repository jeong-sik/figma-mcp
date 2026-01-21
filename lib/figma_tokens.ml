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

(** SwiftUI (iOS/macOS) *)
let to_swiftui tokens =
  let lines = Buffer.create 1024 in
  Buffer.add_string lines "// DesignTokens.swift - Auto-generated from Figma\nimport SwiftUI\n\n";

  (* Colors *)
  Buffer.add_string lines "// MARK: - Colors\nextension Color {\n";
  List.iter (fun (c: color_token) ->
    let name = Str.global_replace (Str.regexp "-") "_" c.name in
    Buffer.add_string lines (Printf.sprintf "    static let %s = Color(hex: \"%s\")\n" name c.hex)
  ) tokens.colors;
  Buffer.add_string lines "}\n\n";

  (* Spacing *)
  Buffer.add_string lines "// MARK: - Spacing\nenum Spacing {\n";
  List.iter (fun (s: spacing_token) ->
    let name = Printf.sprintf "space%.0f" s.value in
    Buffer.add_string lines (Printf.sprintf "    static let %s: CGFloat = %.0f\n" name s.value)
  ) tokens.spacing;
  Buffer.add_string lines "}\n\n";

  (* Border Radius *)
  Buffer.add_string lines "// MARK: - Corner Radius\nenum CornerRadius {\n";
  List.iter (fun (r: radius_token) ->
    let name = Printf.sprintf "radius%.0f" r.value in
    Buffer.add_string lines (Printf.sprintf "    static let %s: CGFloat = %.0f\n" name r.value)
  ) tokens.radii;
  Buffer.add_string lines "}\n\n";

  (* Typography *)
  Buffer.add_string lines "// MARK: - Typography\nenum Typography {\n";
  List.iter (fun (t: typography_token) ->
    let name = Str.global_replace (Str.regexp "-") "_" t.name in
    let weight = match t.font_weight with
      | w when w >= 700 -> "bold"
      | w when w >= 500 -> "medium"
      | _ -> "regular"
    in
    Buffer.add_string lines (Printf.sprintf "    static let %s = Font.custom(\"%s\", size: %.0f).weight(.%s)\n"
      name t.font_family t.font_size weight)
  ) tokens.typography;
  Buffer.add_string lines "}\n";

  Buffer.contents lines

(** Jetpack Compose (Android) *)
let to_compose tokens =
  let lines = Buffer.create 1024 in
  Buffer.add_string lines "// DesignTokens.kt - Auto-generated from Figma\npackage com.example.design\n\n";
  Buffer.add_string lines "import androidx.compose.ui.graphics.Color\nimport androidx.compose.ui.unit.dp\nimport androidx.compose.ui.unit.sp\nimport androidx.compose.ui.text.font.FontWeight\n\n";

  (* Colors *)
  Buffer.add_string lines "object AppColors {\n";
  List.iter (fun (c: color_token) ->
    let name = Str.global_replace (Str.regexp "-") "" c.name |> String.capitalize_ascii in
    let hex = String.sub c.hex 1 6 in
    Buffer.add_string lines (Printf.sprintf "    val %s = Color(0xFF%s)\n" name (String.uppercase_ascii hex))
  ) tokens.colors;
  Buffer.add_string lines "}\n\n";

  (* Spacing *)
  Buffer.add_string lines "object AppSpacing {\n";
  List.iter (fun (s: spacing_token) ->
    let name = Printf.sprintf "Space%.0f" s.value in
    Buffer.add_string lines (Printf.sprintf "    val %s = %.0f.dp\n" name s.value)
  ) tokens.spacing;
  Buffer.add_string lines "}\n\n";

  (* Border Radius *)
  Buffer.add_string lines "object AppCorners {\n";
  List.iter (fun (r: radius_token) ->
    let name = Printf.sprintf "Radius%.0f" r.value in
    Buffer.add_string lines (Printf.sprintf "    val %s = %.0f.dp\n" name r.value)
  ) tokens.radii;
  Buffer.add_string lines "}\n\n";

  (* Typography *)
  Buffer.add_string lines "object AppTypography {\n";
  List.iter (fun (t: typography_token) ->
    let name = Str.global_replace (Str.regexp "-") "" t.name |> String.capitalize_ascii in
    let weight = match t.font_weight with
      | w when w >= 700 -> "Bold"
      | w when w >= 500 -> "Medium"
      | _ -> "Normal"
    in
    Buffer.add_string lines (Printf.sprintf "    val %s = TextStyle(fontSize = %.0f.sp, fontWeight = FontWeight.%s)\n"
      name t.font_size weight)
  ) tokens.typography;
  Buffer.add_string lines "}\n";

  Buffer.contents lines

(** Flutter (Dart) *)
let to_flutter tokens =
  let lines = Buffer.create 1024 in
  Buffer.add_string lines "// design_tokens.dart - Auto-generated from Figma\nimport 'package:flutter/material.dart';\n\n";

  (* Colors *)
  Buffer.add_string lines "class AppColors {\n  AppColors._();\n\n";
  List.iter (fun (c: color_token) ->
    let name = Str.global_replace (Str.regexp "-") "" c.name in
    let name = String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) name in
    let hex = String.sub c.hex 1 6 in
    Buffer.add_string lines (Printf.sprintf "  static const %s = Color(0xFF%s);\n" name (String.uppercase_ascii hex))
  ) tokens.colors;
  Buffer.add_string lines "}\n\n";

  (* Spacing *)
  Buffer.add_string lines "class AppSpacing {\n  AppSpacing._();\n\n";
  List.iter (fun (s: spacing_token) ->
    let name = Printf.sprintf "space%.0f" s.value in
    Buffer.add_string lines (Printf.sprintf "  static const double %s = %.0f;\n" name s.value)
  ) tokens.spacing;
  Buffer.add_string lines "}\n\n";

  (* Border Radius *)
  Buffer.add_string lines "class AppCorners {\n  AppCorners._();\n\n";
  List.iter (fun (r: radius_token) ->
    let name = Printf.sprintf "radius%.0f" r.value in
    Buffer.add_string lines (Printf.sprintf "  static const double %s = %.0f;\n" name r.value);
    Buffer.add_string lines (Printf.sprintf "  static final %sBorder = BorderRadius.circular(%.0f);\n" name r.value)
  ) tokens.radii;
  Buffer.add_string lines "}\n\n";

  (* Typography *)
  Buffer.add_string lines "class AppTextStyles {\n  AppTextStyles._();\n\n";
  List.iter (fun (t: typography_token) ->
    let name = Str.global_replace (Str.regexp "-") "" t.name in
    let name = String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) name in
    let weight = match t.font_weight with
      | w when w >= 700 -> "w700"
      | w when w >= 500 -> "w500"
      | _ -> "w400"
    in
    Buffer.add_string lines (Printf.sprintf "  static const %s = TextStyle(\n    fontFamily: '%s',\n    fontSize: %.0f,\n    fontWeight: FontWeight.%s,\n  );\n\n"
      name t.font_family t.font_size weight)
  ) tokens.typography;
  Buffer.add_string lines "}\n";

  Buffer.contents lines

(** W3C Design Tokens Format (DTCG) - Community Group Standard *)
let to_w3c_dtcg tokens =
  let color_tokens = List.map (fun (c: color_token) ->
    (c.name, `Assoc [
      ("$type", `String "color");
      ("$value", `String c.hex);
    ])
  ) tokens.colors in

  let spacing_tokens = List.map (fun (s: spacing_token) ->
    (s.name, `Assoc [
      ("$type", `String "dimension");
      ("$value", `String (Printf.sprintf "%.0fpx" s.value));
    ])
  ) tokens.spacing in

  let radius_tokens = List.map (fun (r: radius_token) ->
    (r.name, `Assoc [
      ("$type", `String "dimension");
      ("$value", `String (Printf.sprintf "%.0fpx" r.value));
    ])
  ) tokens.radii in

  let typography_tokens = List.map (fun (t: typography_token) ->
    (t.name, `Assoc [
      ("$type", `String "typography");
      ("$value", `Assoc [
        ("fontFamily", `String t.font_family);
        ("fontSize", `String (Printf.sprintf "%.0fpx" t.font_size));
        ("fontWeight", `Int t.font_weight);
      ]);
    ])
  ) tokens.typography in

  Yojson.Safe.pretty_to_string (`Assoc [
    ("color", `Assoc color_tokens);
    ("spacing", `Assoc spacing_tokens);
    ("borderRadius", `Assoc radius_tokens);
    ("typography", `Assoc typography_tokens);
  ])

(** Output format enum *)
type output_format =
  | CSS
  | Tailwind
  | JSON
  | SwiftUI
  | Compose
  | Flutter
  | W3C_DTCG

let format_of_string = function
  | "css" -> CSS
  | "tailwind" -> Tailwind
  | "json" -> JSON
  | "swiftui" | "swift" | "ios" -> SwiftUI
  | "compose" | "kotlin" | "android" -> Compose
  | "flutter" | "dart" -> Flutter
  | "w3c" | "dtcg" | "tokens" -> W3C_DTCG
  | _ -> JSON

let export_tokens tokens format =
  match format with
  | CSS -> to_css tokens
  | Tailwind -> to_tailwind tokens
  | JSON -> to_json tokens
  | SwiftUI -> to_swiftui tokens
  | Compose -> to_compose tokens
  | Flutter -> to_flutter tokens
  | W3C_DTCG -> to_w3c_dtcg tokens
