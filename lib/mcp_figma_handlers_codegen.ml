(** Code generation handlers: template, plugin codegen/analyze, multi-platform. *)

open Printf
open Mcp_figma_handlers_common

(** Generate code template for a Figma node - with recursive children generation *)
let rec generate_code_template ?(depth=0) node platform =
  let open Yojson.Safe.Util in
  let max_depth = 3 in (* Limit recursion depth *)
  let name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
  let safe_name = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') name in
  let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9' then "_" ^ safe_name else safe_name in
  let to_number json = match json with
    | `Float f -> f | `Int i -> float_of_int i | _ -> 100.0
  in
  let w = member "width" node |> to_number |> int_of_float in
  let h = member "height" node |> to_number |> int_of_float in
  let _node_type = member "type" node |> to_string_option |> Option.value ~default:"FRAME" in

  (* Extract background color from fills *)
  let bg_color = match member "fills" node with
    | `List ((`Assoc fields) :: _) ->
        (match List.assoc_opt "color" fields with
         | Some (`String c) -> Some c
         | _ -> None)
    | _ -> None
  in

  (* Extract children and generate code recursively *)
  let children = match member "children" node with
    | `List kids -> kids
    | _ -> []
  in
  let child_count = List.length children in

  (* Generate child components recursively (up to max_depth) *)
  let generate_child_code child =
    let cname = member "name" child |> to_string_option |> Option.value ~default:"Layer" in
    let ctype = member "type" child |> to_string_option |> Option.value ~default:"FRAME" in
    let cw = member "width" child |> to_number |> int_of_float in
    let ch = member "height" child |> to_number |> int_of_float in
    let safe_cname = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') cname in

    if depth >= max_depth then
      (* At max depth, just generate placeholder *)
      match platform with
      | "react" -> sprintf "      <div style={{width: %d, height: %d}}>{/* %s */}</div>" cw ch cname
      | "swiftui" -> sprintf "            Rectangle().frame(width: %d, height: %d) // %s" cw ch cname
      | "compose" -> sprintf "        Box(Modifier.size(%d.dp, %d.dp)) // %s" cw ch cname
      | "flutter" -> sprintf "        SizedBox(width: %d, height: %d), // %s" cw ch cname
      | _ -> sprintf "  <div style=\"width: %dpx; height: %dpx;\"><!-- %s --></div>" cw ch cname
    else if ctype = "TEXT" then
      (* TEXT nodes become text elements *)
      let text_content = member "characters" child |> to_string_option |> Option.value ~default:cname in
      match platform with
      | "react" -> sprintf "      <span>%s</span>" text_content
      | "swiftui" -> sprintf "            Text(\"%s\")" text_content
      | "compose" -> sprintf "        Text(\"%s\")" text_content
      | "flutter" -> sprintf "        Text('%s')," text_content
      | _ -> sprintf "  <span>%s</span>" text_content
    else
      (* Recursively generate for FRAME/GROUP/COMPONENT *)
      match platform with
      | "react" -> sprintf "      <%s />" safe_cname
      | "swiftui" -> sprintf "            %s()" safe_cname
      | "compose" -> sprintf "        %s()" safe_cname
      | "flutter" -> sprintf "        %s()," safe_cname
      | _ -> sprintf "  <%s></%s>" (String.lowercase_ascii safe_cname) (String.lowercase_ascii safe_cname)
  in

  let children_code =
    if child_count = 0 then ""
    else
      let limited_children = if child_count > 10 then List.filteri (fun i _ -> i < 10) children else children in
      String.concat "\n" (List.map generate_child_code limited_children) ^
      (if child_count > 10 then sprintf "\n      {/* ... and %d more children */}" (child_count - 10) else "")
  in

  let children_comment =
    if child_count = 0 || depth > 0 then ""
    else
      let child_lines = List.mapi (fun _i child ->
        let cname = member "name" child |> to_string_option |> Option.value ~default:"Layer" in
        let ctype = member "type" child |> to_string_option |> Option.value ~default:"" in
        sprintf "//   - %s (%s)" cname ctype
      ) (if child_count > 8 then List.filteri (fun i _ -> i < 8) children else children) in
      let truncated = if child_count > 8 then sprintf "\n//   ... and %d more" (child_count - 8) else "" in
      sprintf "\n// Children (%d):\n%s%s\n" child_count (String.concat "\n" child_lines) truncated
  in

  (* Generate sub-component definitions for deeper children *)
  let sub_components =
    if depth >= max_depth || child_count = 0 then ""
    else
      let sub_defs = List.filter_map (fun child ->
        let ctype = member "type" child |> to_string_option |> Option.value ~default:"" in
        if ctype = "TEXT" then None
        else Some (generate_code_template ~depth:(depth + 1) child platform)
      ) (if child_count > 5 then List.filteri (fun i _ -> i < 5) children else children) in
      if List.length sub_defs = 0 then "" else "\n" ^ String.concat "\n" sub_defs
  in

  (* Extract auto-layout info *)
  let layout_mode = match member "autoLayout" node with
    | `Assoc fields ->
        (match List.assoc_opt "mode" fields with
         | Some (`String m) -> Some m
         | _ -> None)
    | _ -> None
  in
  let layout_spacing = match member "autoLayout" node with
    | `Assoc fields ->
        (match List.assoc_opt "spacing" fields with
         | Some (`Int s) -> Some s
         | Some (`Float s) -> Some (int_of_float s)
         | _ -> None)
    | _ -> None
  in

  (* Build child content or empty placeholder *)
  let empty_children = if child_count = 0 then "{/* Empty */}" else "" in
  let react_children = if children_code = "" then empty_children else "\n" ^ children_code ^ "\n    " in
  let swift_children = if children_code = "" then "EmptyView()" else "\n" ^ children_code ^ "\n        " in
  let compose_children = if children_code = "" then "// Empty" else "\n" ^ children_code ^ "\n    " in
  let flutter_children = if children_code = "" then "// Empty" else "\n" ^ children_code ^ "\n      " in
  let html_children = if children_code = "" then "<!-- Empty -->" else "\n" ^ children_code ^ "\n" in

  match platform with
  | "react" ->
      let bg_style = match bg_color with Some c -> sprintf "backgroundColor: '%s',\n        " c | None -> "" in
      let flex_style = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "display: 'flex', flexDirection: 'row', gap: %d,\n        " sp
        | Some "VERTICAL", Some sp -> sprintf "display: 'flex', flexDirection: 'column', gap: %d,\n        " sp
        | _ -> ""
      in
      sprintf "import React from 'react';\n%s\nexport const %s: React.FC = () => (\n  <div style={{\n        width: %d, height: %d,\n        %s%s}}>%s</div>\n);\n%s"
        children_comment safe_name w h bg_style flex_style react_children sub_components

  | "swiftui" ->
      let bg_mod = match bg_color with Some c -> sprintf "\n        .background(Color(hex: \"%s\"))" c | None -> "" in
      let stack = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "HStack(spacing: %d)" sp
        | Some "VERTICAL", Some sp -> sprintf "VStack(spacing: %d)" sp
        | _ -> "ZStack"
      in
      sprintf "import SwiftUI\n%s\nstruct %s: View {\n    var body: some View {\n        %s {%s}\n        .frame(width: %d, height: %d)%s\n    }\n}\n%s"
        children_comment safe_name stack swift_children w h bg_mod sub_components

  | "compose" ->
      let bg_mod = match bg_color with Some c -> sprintf ".background(Color(0xFF%s))\n            " (String.sub c 1 (String.length c - 1)) | None -> "" in
      let layout = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "Row(horizontalArrangement = Arrangement.spacedBy(%d.dp))" sp
        | Some "VERTICAL", Some sp -> sprintf "Column(verticalArrangement = Arrangement.spacedBy(%d.dp))" sp
        | _ -> "Box"
      in
      sprintf "@Composable\nfun %s(modifier: Modifier = Modifier) {%s\n    %s(modifier.size(%d.dp, %d.dp)%s) {%s}\n}\n%s"
        safe_name children_comment layout w h bg_mod compose_children sub_components

  | "flutter" ->
      let bg_prop = match bg_color with Some c -> sprintf "color: Color(0xFF%s),\n      " (String.sub c 1 (String.length c - 1)) | None -> "" in
      let layout = match layout_mode with Some "HORIZONTAL" -> "Row" | Some "VERTICAL" -> "Column" | _ -> "Stack" in
      sprintf "import 'package:flutter/material.dart';\n%s\nclass %s extends StatelessWidget {\n  const %s({super.key});\n\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      width: %d, height: %d,\n      %schild: %s(children: [%s]),\n    );\n  }\n}\n%s"
        children_comment safe_name safe_name w h bg_prop layout flutter_children sub_components

  | _ ->
      let bg_css = match bg_color with Some c -> sprintf "background-color: %s;\n  " c | None -> "" in
      let flex_css = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "display: flex; flex-direction: row; gap: %dpx;\n  " sp
        | Some "VERTICAL", Some sp -> sprintf "display: flex; flex-direction: column; gap: %dpx;\n  " sp
        | _ -> ""
      in
      sprintf "<!-- %s -->%s\n<div class=\"%s\">%s</div>\n\n<style>\n.%s {\n  width: %dpx; height: %dpx;\n  %s%s}\n</style>\n%s"
        name children_comment (String.lowercase_ascii safe_name) html_children (String.lowercase_ascii safe_name) w h bg_css flex_css sub_components

[@@@coverage off]
(* POST /plugin/template - Direct template generation (for testing) *)
let template_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let code = generate_code_template node platform in
        let result = `Assoc [
          ("code", `String code);
          ("platform", `String platform);
          ("source", `String "template");
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** Codegen HTTP error -- used in Claude/Ollama fallback chain *)
exception Codegen_http_error of int * string

(** Plugin codegen handler - calls llm-mcp for code generation *)
let plugin_codegen_handler ~sw ~eio_ctx _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in

        (* Build LLM request with semantic analysis *)
        let semantic_info = analyze_node_semantic node in
        let platform_instruction = match platform with
          | "react" -> "Generate production-ready React/TypeScript code. Use functional components with proper typing. Include all exact pixel values for width, height, padding, margin, fontSize, borderRadius."
          | "swiftui" -> "Generate production-ready SwiftUI code. Use proper View modifiers with exact pixel values for frame, padding, cornerRadius, fontSize."
          | "flutter" -> "Generate production-ready Flutter/Dart code. Use exact pixel values in SizedBox, Container, EdgeInsets, BorderRadius."
          | "compose" -> "Generate production-ready Jetpack Compose/Kotlin code. Use exact Dp values for size, padding, corner radius."
          | _ -> "Generate production-ready code with exact pixel measurements."
        in
        let full_prompt = if prompt = "" then
          sprintf "Convert this Figma design to %s code.\n\n%s\n\n%s\n\nIMPORTANT: Use EXACT pixel values from the design. Do not approximate." platform semantic_info platform_instruction
        else
          prompt
        in

        (* Fallback to template *)
        let send_template () =
          let template = generate_code_template node platform in
          let result = `Assoc [("code", `String template); ("platform", `String platform); ("fallback", `Bool true)] in
          Response.json (Yojson.Safe.to_string result) reqd
        in

        (* Try Ollama *)
        let try_ollama () =
          let ollama_url = "http://127.0.0.1:11434/api/generate" in
          let ollama_body = `Assoc [
            ("model", `String "qwen3-coder:30b");
            ("prompt", `String full_prompt);
            ("stream", `Bool false);
          ] in
          let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_helpers.client in
          let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
          let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string ollama_body) in
          let uri = Uri.of_string ollama_url in
          let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
          let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status_code < 200 || status_code >= 300 then
            raise (Codegen_http_error (status_code, "Ollama"));
          let ollama_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
          let ollama_resp = Yojson.Safe.from_string ollama_resp_str in
          let gen_code = member "response" ollama_resp |> to_string_option |> Option.value ~default:"" in
          if String.length gen_code > 10 then
            let result_json = `Assoc [("code", `String gen_code); ("platform", `String platform); ("source", `String "ollama")] in
            Response.json (Yojson.Safe.to_string result_json) reqd
          else
            send_template ()
        in

        (* Try Claude API first if key available *)
        let anthropic_key = Sys.getenv_opt "ANTHROPIC_API_KEY" in
        (match anthropic_key with
        | Some key when String.length key > 10 ->
            printf "[Codegen] Trying Claude API...\n%!";
            (try
              let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_helpers.client in
              let claude_body = `Assoc [
                ("model", `String "claude-sonnet-4-20250514");  (* Claude 4 Sonnet *)
                ("max_tokens", `Int 4096);
                ("messages", `List [
                  `Assoc [
                    ("role", `String "user");
                    ("content", `String full_prompt)
                  ]
                ]);
              ] in
              let headers = Cohttp.Header.of_list [
                ("Content-Type", "application/json");
                ("x-api-key", key);
                ("anthropic-version", "2023-06-01");
              ] in
              let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string claude_body) in
              let uri = Uri.of_string "https://api.anthropic.com/v1/messages" in
              let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
              let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
              if status_code < 200 || status_code >= 300 then begin
                let err_body = try Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:4096 with Eio.Buf_read.Buffer_limit_exceeded | Eio.Io _ -> "" in
                printf "[Codegen] Claude HTTP %d: %s\n%!" status_code err_body;
                raise (Codegen_http_error (status_code, "Claude"))
              end;
              let claude_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
              let claude_resp = Yojson.Safe.from_string claude_resp_str in
              let content_blocks = member "content" claude_resp |> to_list in
              let gen_code = List.fold_left (fun acc block ->
                let text = member "text" block |> to_string_option |> Option.value ~default:"" in
                acc ^ text
              ) "" content_blocks in
              if String.length gen_code > 10 then
                let result_json = `Assoc [("code", `String gen_code); ("platform", `String platform); ("source", `String "claude")] in
                Response.json (Yojson.Safe.to_string result_json) reqd
              else begin
                printf "[Codegen] Claude returned empty, fallback to Ollama\n%!";
                try_ollama ()
              end
            with exn ->
              printf "[Codegen] Claude error: %s, fallback to Ollama\n%!" (Printexc.to_string exn);
              try_ollama ())
        | _ ->
            (* No Claude key, use Ollama *)
            (try try_ollama () with exn ->
              Printf.eprintf "[Codegen] Ollama fallback: %s, using template\n%!" (Printexc.to_string exn);
              send_template ()))
  )

(** Plugin analyze handler - analyzes node structure with LLM insights *)
let plugin_analyze_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in

        (* Build analysis from node info *)
        let node_info = Yojson.Safe.to_string node in
        let _full_prompt = if prompt = "" then
          sprintf "Analyze this Figma node and provide insights:\n%s\n\nProvide: 1) Structure overview, 2) Design patterns used, 3) Accessibility considerations, 4) Implementation recommendations." node_info
        else
          prompt
        in

        (* Local analysis - fast and reliable, no LLM dependency *)
        let to_number json = match json with
          | `Float f -> f | `Int i -> float_of_int i | _ -> 0.0
        in
        let name = member "name" node |> to_string_option |> Option.value ~default:"Unnamed" in
        let node_type = member "type" node |> to_string_option |> Option.value ~default:"UNKNOWN" in
        let w = member "width" node |> to_number |> int_of_float in
        let h = member "height" node |> to_number |> int_of_float in
        let children = match member "children" node with `List kids -> List.length kids | _ -> 0 in
        let has_autolayout = match member "autoLayout" node with `Null -> false | _ -> true in
        let fills_count = match member "fills" node with `List f -> List.length f | _ -> 0 in

        (* Build children list *)
        let children_detail = match member "children" node with
          | `List kids ->
              let child_items = List.mapi (fun i c ->
                let cname = member "name" c |> to_string_option |> Option.value ~default:(sprintf "Layer %d" i) in
                let ctype = member "type" c |> to_string_option |> Option.value ~default:"UNKNOWN" in
                sprintf "  - %s (%s)" cname ctype
              ) (List.filteri (fun i _ -> i < 10) kids) in
              if List.length kids > 10 then
                String.concat "\n" child_items ^ sprintf "\n  - ... and %d more" (List.length kids - 10)
              else
                String.concat "\n" child_items
          | _ -> "  (none)"
        in

        let analysis = sprintf "## Analysis: %s\n\n**Type**: %s\n**Dimensions**: %d × %d px\n**Children**: %d layer(s)\n%s\n\n**Auto-layout**: %s\n**Fills**: %d\n\n### Recommendations\n- %s\n- Consider adding semantic naming for accessibility\n- %s"
          name node_type w h children children_detail
          (if has_autolayout then "Yes (responsive)" else "No (fixed)")
          fills_count
          (if children > 5 then "Consider grouping related layers" else "Structure looks manageable")
          (if w > 1200 then "Large width - ensure responsive breakpoints" else "Width suitable for most viewports")
        in
        let result_json = `Assoc [("analysis", `String analysis); ("source", `String "local")] in
        Response.json (Yojson.Safe.to_string result_json) reqd
  )

(** POST /plugin/codegen-multi - Generate code for multiple platforms simultaneously *)
let codegen_multi_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platforms = match member "platforms" json with
          | `List ps -> List.filter_map (fun p -> to_string_option p) ps
          | _ -> ["react"; "swiftui"; "compose"]  (* default all 3 *)
        in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
        let to_int_safe json = int_of_float (to_num json) in

        (* Extract component info *)
        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in
        let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9'
          then "C" ^ safe_name else safe_name in

        let w = member "width" node |> to_int_safe in
        let h = member "height" node |> to_int_safe in
        let radius = member "cornerRadius" node |> to_num in
        let padding_t = member "paddingTop" node |> to_int_safe in
        let padding_r = member "paddingRight" node |> to_int_safe in
        let padding_b = member "paddingBottom" node |> to_int_safe in
        let padding_l = member "paddingLeft" node |> to_int_safe in
        let gap = member "itemSpacing" node |> to_int_safe in

        (* Extract primary colors *)
        let bg_color = match member "fills" node with
          | `List (`Assoc fill :: _) ->
              (match List.assoc_opt "color" fill with
               | Some (`Assoc c) ->
                   let r = List.assoc_opt "r" c |> Option.map to_num |> Option.value ~default:1.0 in
                   let g = List.assoc_opt "g" c |> Option.map to_num |> Option.value ~default:1.0 in
                   let b = List.assoc_opt "b" c |> Option.map to_num |> Option.value ~default:1.0 in
                   (r, g, b)
               | _ -> (1.0, 1.0, 1.0))
          | _ -> (1.0, 1.0, 1.0)
        in
        let (bg_r, bg_g, bg_b) = bg_color in
        let bg_hex = sprintf "#%02x%02x%02x" (int_of_float (bg_r *. 255.0)) (int_of_float (bg_g *. 255.0)) (int_of_float (bg_b *. 255.0)) in

        (* Check layout mode *)
        let layout_mode = member "layoutMode" node |> to_string_option |> Option.value ~default:"NONE" in
        let is_vertical = layout_mode = "VERTICAL" in
        let is_horizontal = layout_mode = "HORIZONTAL" in

        (* Generate code for each platform *)
        let generate_for_platform platform =
          let code = match platform with
            | "react" ->
                let flex_dir = if is_vertical then "column" else if is_horizontal then "row" else "column" in
                sprintf {|import React from 'react';

interface %sProps {
  children?: React.ReactNode;
}

export const %s: React.FC<%sProps> = ({ children }) => {
  return (
    <div
      style={{
        width: %d,
        height: %d,
        backgroundColor: '%s',
        borderRadius: %.0f,
        padding: '%dpx %dpx %dpx %dpx',
        display: 'flex',
        flexDirection: '%s',
        gap: %d,
      }}
    >
      {children}
    </div>
  );
};
|}
                  safe_name safe_name safe_name
                  w h bg_hex radius
                  padding_t padding_r padding_b padding_l
                  flex_dir gap

            | "swiftui" ->
                let stack_type = if is_horizontal then "HStack" else "VStack" in
                sprintf {|import SwiftUI

struct %s: View {
    var body: some View {
        %s(spacing: %d) {
            // Children go here
        }
        .frame(width: %d, height: %d)
        .padding(.top, %d)
        .padding(.trailing, %d)
        .padding(.bottom, %d)
        .padding(.leading, %d)
        .background(Color(red: %.3f, green: %.3f, blue: %.3f))
        .cornerRadius(%.0f)
    }
}

#Preview {
    %s()
}
|}
                  safe_name
                  stack_type gap
                  w h
                  padding_t padding_r padding_b padding_l
                  bg_r bg_g bg_b
                  radius
                  safe_name

            | "compose" ->
                let arrangement = if is_horizontal then "Arrangement.spacedBy" else "Arrangement.spacedBy" in
                let container = if is_horizontal then "Row" else "Column" in
                sprintf {|import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp

@Composable
fun %s(
    modifier: Modifier = Modifier,
    content: @Composable %sScope.() -> Unit = {}
) {
    %s(
        modifier = modifier
            .size(width = %d.dp, height = %d.dp)
            .clip(RoundedCornerShape(%.0f.dp))
            .background(Color(0xFF%s))
            .padding(
                top = %d.dp,
                end = %d.dp,
                bottom = %d.dp,
                start = %d.dp
            ),
        %s(%d.dp),
        content = content
    )
}

@Preview
@Composable
private fun %sPreview() {
    %s()
}
|}
                  safe_name container
                  container
                  w h radius
                  (String.sub bg_hex 1 6)  (* remove # *)
                  padding_t padding_r padding_b padding_l
                  arrangement gap
                  safe_name safe_name

            | "flutter" ->
                let container = if is_horizontal then "Row" else "Column" in
                sprintf {|import 'package:flutter/material.dart';

class %s extends StatelessWidget {
  final List<Widget> children;

  const %s({
    super.key,
    this.children = const [],
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      width: %d,
      height: %d,
      padding: const EdgeInsets.fromLTRB(%d, %d, %d, %d),
      decoration: BoxDecoration(
        color: const Color(0xFF%s),
        borderRadius: BorderRadius.circular(%.0f),
      ),
      child: %s(
        mainAxisSize: MainAxisSize.min,
        spacing: %d,
        children: children,
      ),
    );
  }
}
|}
                  safe_name safe_name
                  w h
                  padding_l padding_t padding_r padding_b
                  (String.sub bg_hex 1 6)
                  radius
                  container gap

            | _ -> sprintf "// Unsupported platform: %s" platform
          in
          (platform, code)
        in

        let results = List.map generate_for_platform platforms in
        let code_assoc = List.map (fun (p, c) -> (p, `String c)) results in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("platforms", `List (List.map (fun p -> `String p) platforms));
          ("code", `Assoc code_assoc);
          ("sharedTokens", `Assoc [
            ("width", `Int w);
            ("height", `Int h);
            ("backgroundColor", `String bg_hex);
            ("borderRadius", `Float radius);
            ("padding", `Assoc [
              ("top", `Int padding_t);
              ("right", `Int padding_r);
              ("bottom", `Int padding_b);
              ("left", `Int padding_l);
            ]);
            ("gap", `Int gap);
            ("layoutMode", `String layout_mode);
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )
