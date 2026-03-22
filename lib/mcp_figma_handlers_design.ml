(** Design handlers: storybook generation, responsive breakpoints,
    accessibility, asset export. *)

open Printf
open Mcp_figma_handlers_common

[@@@coverage off]
(* POST /plugin/generate-story - Generate Storybook story from Figma node *)
let generate_story_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let figma_url = member "figmaUrl" json |> to_string_option |> Option.value ~default:"" in
        let framework = member "framework" json |> to_string_option |> Option.value ~default:"react" in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract component info *)
        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in
        let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9'
          then "C" ^ safe_name else safe_name in

        (* Extract design tokens for controls *)
        let colors = ref [] in
        let spacings = ref [] in
        let radii = ref [] in

        let rec extract_tokens n =
          let _name = member "name" n |> to_string_option |> Option.value ~default:"" in

          (* Colors *)
          (match member "fills" n with
           | `List fills -> List.iter (fun fill ->
               match member "color" fill with
               | `Assoc c ->
                   let r = List.assoc_opt "r" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let g = List.assoc_opt "g" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let b = List.assoc_opt "b" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let hex = sprintf "#%02x%02x%02x" (int_of_float (r *. 255.0)) (int_of_float (g *. 255.0)) (int_of_float (b *. 255.0)) in
                   if not (List.mem hex !colors) then colors := hex :: !colors
               | _ -> ()
             ) fills
           | _ -> ());

          (* Spacing *)
          let padding = member "paddingTop" n |> to_num in
          let gap = member "itemSpacing" n |> to_num in
          if padding > 0.0 && not (List.mem (int_of_float padding) !spacings) then
            spacings := (int_of_float padding) :: !spacings;
          if gap > 0.0 && not (List.mem (int_of_float gap) !spacings) then
            spacings := (int_of_float gap) :: !spacings;

          (* Border radius *)
          let radius = member "cornerRadius" n |> to_num in
          if radius > 0.0 && not (List.mem (int_of_float radius) !radii) then
            radii := (int_of_float radius) :: !radii;

          (* Recurse *)
          (match member "children" n with
           | `List kids -> List.iter extract_tokens kids
           | _ -> ())
        in
        extract_tokens node;

        (* Generate story based on framework *)
        let story_code = match framework with
          | "react" ->
              let color_options = String.concat ", " (List.map (sprintf "'%s'") !colors) in
              let spacing_options = String.concat ", " (List.map string_of_int !spacings) in
              let radius_options = String.concat ", " (List.map string_of_int !radii) in

              sprintf {|import type { Meta, StoryObj } from '@storybook/react';
import { %s } from './%s';

/**
 * %s Component
 *
 * Generated from Figma design.
 * @see %s
 */
const meta: Meta<typeof %s> = {
  title: 'Components/%s',
  component: %s,
  parameters: {
    layout: 'centered',
    design: {
      type: 'figma',
      url: '%s',
    },
  },
  tags: ['autodocs'],
  argTypes: {%s%s%s
  },
};

export default meta;
type Story = StoryObj<typeof meta>;

/**
 * Default state from Figma design
 */
export const Default: Story = {
  args: {},
};

/**
 * Interactive playground with all controls
 */
export const Playground: Story = {
  args: {},
  parameters: {
    docs: {
      canvas: { sourceState: 'shown' },
    },
  },
};
|}
                safe_name safe_name
                component_name
                (if figma_url = "" then "Figma" else figma_url)
                safe_name component_name safe_name
                figma_url
                (if !colors = [] then "" else sprintf "\n    backgroundColor: {\n      control: 'select',\n      options: [%s],\n    }," color_options)
                (if !spacings = [] then "" else sprintf "\n    padding: {\n      control: 'select',\n      options: [%s],\n    }," spacing_options)
                (if !radii = [] then "" else sprintf "\n    borderRadius: {\n      control: 'select',\n      options: [%s],\n    }," radius_options)

          | "vue" ->
              sprintf {|import type { Meta, StoryObj } from '@storybook/vue3';
import %s from './%s.vue';

const meta: Meta<typeof %s> = {
  title: 'Components/%s',
  component: %s,
  parameters: {
    design: {
      type: 'figma',
      url: '%s',
    },
  },
  tags: ['autodocs'],
};

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: {},
};
|}
                safe_name safe_name
                safe_name component_name safe_name
                figma_url

          | _ -> (* generic *)
              sprintf {|// Storybook story for %s
// Figma: %s

export default {
  title: 'Components/%s',
  parameters: {
    design: {
      type: 'figma',
      url: '%s',
    },
  },
};

export const Default = {};
|}
                component_name figma_url component_name figma_url
        in

        (* Generate component code if not provided *)
        let component_code = if code = "" then
          sprintf {|import React from 'react';

interface %sProps {
  backgroundColor?: string;
  padding?: number;
  borderRadius?: number;
  children?: React.ReactNode;
}

export const %s: React.FC<%sProps> = ({
  backgroundColor = '%s',
  padding = %d,
  borderRadius = %d,
  children,
}) => {
  return (
    <div
      style={{
        backgroundColor,
        padding,
        borderRadius,
      }}
    >
      {children}
    </div>
  );
};
|}
            safe_name safe_name safe_name
            (if !colors = [] then "#ffffff" else List.hd !colors)
            (if !spacings = [] then 16 else List.hd !spacings)
            (if !radii = [] then 8 else List.hd !radii)
        else
          code
        in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("storyFile", `String (sprintf "%s.stories.tsx" safe_name));
          ("componentFile", `String (sprintf "%s.tsx" safe_name));
          ("storyCode", `String story_code);
          ("componentCode", `String component_code);
          ("figmaUrl", `String figma_url);
          ("designTokens", `Assoc [
            ("colors", `List (List.map (fun c -> `String c) !colors));
            ("spacings", `List (List.map (fun s -> `Int s) !spacings));
            ("borderRadii", `List (List.map (fun r -> `Int r) !radii));
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

[@@@coverage on]
(* POST /plugin/responsive-breakpoints - Generate responsive code with breakpoints *)
let responsive_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let breakpoints = match member "breakpoints" json with
          | `List bps -> List.filter_map (fun bp ->
              match (member "name" bp |> to_string_option, member "width" bp |> to_int_option) with
              | (Some n, Some w) -> Some (n, w)
              | _ -> None
            ) bps
          | _ -> [("mobile", 375); ("tablet", 768); ("desktop", 1440)]
        in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
        let to_int_safe json = int_of_float (to_num json) in

        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in

        let base_w = member "width" node |> to_int_safe in
        let base_h = member "height" node |> to_int_safe in
        let radius = member "cornerRadius" node |> to_num in
        let padding = member "paddingTop" node |> to_int_safe in

        (* Generate CSS with media queries *)
        let css_code = sprintf {|.%s {
  width: %dpx;
  height: %dpx;
  border-radius: %.0fpx;
  padding: %dpx;
}

%s
|}
          safe_name base_w base_h radius padding
          (String.concat "\n\n" (List.map (fun (bp_name, bp_width) ->
            let scale = float_of_int bp_width /. float_of_int base_w in
            sprintf {|/* %s (%dpx) */
@media (max-width: %dpx) {
  .%s {
    width: %dpx;
    height: %dpx;
    border-radius: %.0fpx;
    padding: %dpx;
  }
}|}
              bp_name bp_width bp_width
              safe_name
              bp_width
              (int_of_float (float_of_int base_h *. scale))
              (radius *. scale)
              (int_of_float (float_of_int padding *. scale))
          ) breakpoints))
        in

        (* Generate Tailwind classes *)
        let tailwind = sprintf "%s w-[%dpx] h-[%dpx] rounded-[%.0fpx] p-[%dpx] %s"
          safe_name base_w base_h radius padding
          (String.concat " " (List.map (fun (bp_name, bp_width) ->
            let scale = float_of_int bp_width /. float_of_int base_w in
            sprintf "%s:w-[%dpx] %s:h-[%dpx]"
              bp_name bp_width
              bp_name (int_of_float (float_of_int base_h *. scale))
          ) breakpoints))
        in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("baseWidth", `Int base_w);
          ("breakpoints", `List (List.map (fun (n, w) -> `Assoc [("name", `String n); ("width", `Int w)]) breakpoints));
          ("css", `String css_code);
          ("tailwind", `String tailwind);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/accessibility - Generate accessibility attributes *)
let accessibility_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in

        let suggestions = ref [] in
        let aria_attrs = ref [] in

        let rec analyze_accessibility n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
          let ln = String.lowercase_ascii name in

          (* Helper to check if name contains keyword (anywhere, not just prefix) *)
          let contains_word s word =
            let re = Str.regexp_string word in
            try ignore (Str.search_forward re s 0); true
            with Not_found -> false
          in

          (* Detect interactive elements - check anywhere in name *)
          let is_button = contains_word ln "button" || contains_word ln "btn" || contains_word ln "cta" ||
                          contains_word ln "submit" || contains_word ln "cancel" || contains_word ln "action" in
          let is_link = contains_word ln "link" || contains_word ln "anchor" in
          let is_input = contains_word ln "input" || contains_word ln "search" || contains_word ln "field" ||
                         contains_word ln "text" || contains_word ln "email" || contains_word ln "password" in
          let is_image = contains_word ln "image" || contains_word ln "icon" || contains_word ln "avatar" ||
                         contains_word ln "photo" || contains_word ln "picture" || contains_word ln "img" in
          let is_nav = contains_word ln "nav" || contains_word ln "menu" || contains_word ln "sidebar" ||
                       contains_word ln "header" || contains_word ln "footer" in
          let is_modal = contains_word ln "modal" || contains_word ln "dialog" || contains_word ln "popup" ||
                         contains_word ln "overlay" || contains_word ln "drawer" in

          if is_button then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "button");
              ("aria-label", `String name);
              ("tabIndex", `Int 0);
            ] :: !aria_attrs;
            suggestions := sprintf "Button '%s': Add aria-label and ensure keyboard accessibility" name :: !suggestions
          end;

          if is_link then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "link");
              ("href", `String "#");
            ] :: !aria_attrs
          end;

          if is_input then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "textbox");
              ("aria-label", `String name);
              ("aria-required", `Bool false);
            ] :: !aria_attrs;
            suggestions := sprintf "Input '%s': Add label element or aria-label" name :: !suggestions
          end;

          if is_image then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "img");
              ("alt", `String name);
            ] :: !aria_attrs;
            suggestions := sprintf "Image '%s': Ensure alt text is descriptive" name :: !suggestions
          end;

          if is_nav then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "navigation");
              ("aria-label", `String name);
            ] :: !aria_attrs
          end;

          if is_modal then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "dialog");
              ("aria-modal", `Bool true);
              ("aria-labelledby", `String (name ^ "-title"));
            ] :: !aria_attrs;
            suggestions := sprintf "Modal '%s': Implement focus trap and escape key handling" name :: !suggestions
          end;

          (* Check for text contrast (simplified) *)
          if node_type = "TEXT" then begin
            let chars = member "text" n |> member "characters" |> to_string_option |> Option.value ~default:"" in
            if String.length chars > 0 then
              suggestions := sprintf "Text '%s': Verify color contrast meets WCAG AA (4.5:1)" name :: !suggestions
          end;

          (* Recurse *)
          (match member "children" n with
           | `List kids -> List.iter analyze_accessibility kids
           | _ -> ())
        in
        analyze_accessibility node;

        let result = `Assoc [
          ("ariaAttributes", `List (List.rev !aria_attrs));
          ("suggestions", `List (List.map (fun s -> `String s) (List.rev !suggestions)));
          ("summary", `Assoc [
            ("interactiveElements", `Int (List.length !aria_attrs));
            ("suggestions", `Int (List.length !suggestions));
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/export-assets - Export SVG/PNG assets from node *)
let export_assets_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let formats = match member "formats" json with
          | `List fs -> List.filter_map to_string_option fs
          | _ -> ["svg"; "png@1x"; "png@2x"]
        in

        (* Collect exportable assets (icons, images, logos) *)
        let assets = ref [] in

        let rec find_assets n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
          let node_id = member "id" n |> to_string_option |> Option.value ~default:"" in
          let ln = String.lowercase_ascii name in

          let is_icon = String.length ln >= 4 && String.sub ln 0 4 = "icon" in
          let is_logo = String.length ln >= 4 && String.sub ln 0 4 = "logo" in
          let is_image = node_type = "VECTOR" || node_type = "ELLIPSE" ||
                         (node_type = "FRAME" && (is_icon || is_logo)) in

          if is_image || is_icon || is_logo then begin
            let export_settings = List.map (fun fmt ->
              let (format, scale) = match String.split_on_char '@' fmt with
                | [f; s] -> (f, (try float_of_string (String.sub s 0 (String.length s - 1)) with Failure _ -> 1.0))
                | _ -> (fmt, 1.0)
              in
              `Assoc [
                ("format", `String format);
                ("scale", `Float scale);
                ("filename", `String (sprintf "%s%s.%s" name (if scale > 1.0 then sprintf "@%.0fx" scale else "") format));
              ]
            ) formats in
            assets := `Assoc [
              ("name", `String name);
              ("nodeId", `String node_id);
              ("type", `String node_type);
              ("exports", `List export_settings);
            ] :: !assets
          end;

          (match member "children" n with
           | `List kids -> List.iter find_assets kids
           | _ -> ())
        in
        find_assets node;

        let result = `Assoc [
          ("assets", `List (List.rev !assets));
          ("formats", `List (List.map (fun f -> `String f) formats));
          ("totalAssets", `Int (List.length !assets));
          ("note", `String "Use Figma API /images endpoint with nodeId to download actual files");
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )
