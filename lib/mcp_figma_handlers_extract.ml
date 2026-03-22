(** Extraction handlers: design tokens, variants, animations. *)

open Printf
open Mcp_figma_handlers_common

(** POST /plugin/extract-tokens - Extract design tokens from Figma node *)
let extract_tokens_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let format = member "format" json |> to_string_option |> Option.value ~default:"tokens-studio" in

        (* Token collection *)
        let colors = Hashtbl.create 32 in
        let typography = Hashtbl.create 16 in
        let spacing = Hashtbl.create 16 in
        let radii = Hashtbl.create 16 in
        let effects = Hashtbl.create 16 in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* RGBA color to hex with alpha *)
        let rgba_to_hex r g b a =
          let ri = int_of_float (r *. 255.0) in
          let gi = int_of_float (g *. 255.0) in
          let bi = int_of_float (b *. 255.0) in
          if a < 1.0 then
            sprintf "rgba(%d, %d, %d, %.2f)" ri gi bi a
          else
            sprintf "#%02x%02x%02x" ri gi bi
        in

        (* Extract color from fill/stroke *)
        let extract_color prefix fill =
          match member "color" fill with
          | `Assoc color_obj ->
              let r = List.assoc_opt "r" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let g = List.assoc_opt "g" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let b = List.assoc_opt "b" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let a = List.assoc_opt "a" color_obj |> Option.map to_num |> Option.value ~default:1.0 in
              let hex = rgba_to_hex r g b a in
              Hashtbl.replace colors (prefix ^ "-" ^ hex) hex
          | _ -> ()
        in

        (* Recursive token extraction *)
        let rec extract_from_node n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in

          (* Colors from fills *)
          (match member "fills" n with
           | `List fills -> List.iteri (fun i fill ->
               let visible = member "visible" fill |> to_bool_option |> Option.value ~default:true in
               if visible then extract_color (sprintf "fill-%s-%d" name i) fill
             ) fills
           | _ -> ());

          (* Colors from strokes *)
          (match member "strokes" n with
           | `List strokes -> List.iteri (fun i stroke ->
               extract_color (sprintf "stroke-%s-%d" name i) stroke
             ) strokes
           | _ -> ());

          (* Typography from TEXT nodes *)
          if node_type = "TEXT" then begin
            let style = member "style" n in
            let font_family = member "fontFamily" style |> to_string_option |> Option.value ~default:"" in
            let font_size = member "fontSize" style |> to_num in
            let font_weight = member "fontWeight" style |> to_num in
            let line_height = member "lineHeightPx" style |> to_num in
            let letter_spacing = member "letterSpacing" style |> to_num in
            if font_family <> "" then
              Hashtbl.replace typography name (`Assoc [
                ("fontFamily", `String font_family);
                ("fontSize", `Float font_size);
                ("fontWeight", `Int (int_of_float font_weight));
                ("lineHeight", `Float line_height);
                ("letterSpacing", `Float letter_spacing);
              ])
          end;

          (* Spacing from auto-layout *)
          let item_spacing = member "itemSpacing" n |> to_num in
          let padding_top = member "paddingTop" n |> to_num in
          let padding_right = member "paddingRight" n |> to_num in
          let padding_bottom = member "paddingBottom" n |> to_num in
          let padding_left = member "paddingLeft" n |> to_num in

          if item_spacing > 0.0 then
            Hashtbl.replace spacing (sprintf "gap-%s" name) (`Float item_spacing);
          if padding_top > 0.0 || padding_right > 0.0 || padding_bottom > 0.0 || padding_left > 0.0 then
            Hashtbl.replace spacing (sprintf "padding-%s" name) (`Assoc [
              ("top", `Float padding_top);
              ("right", `Float padding_right);
              ("bottom", `Float padding_bottom);
              ("left", `Float padding_left);
            ]);

          (* Border radius *)
          let corner_radius = member "cornerRadius" n |> to_num in
          let top_left = member "topLeftRadius" n |> to_num in
          let top_right = member "topRightRadius" n |> to_num in
          let bottom_right = member "bottomRightRadius" n |> to_num in
          let bottom_left = member "bottomLeftRadius" n |> to_num in

          if corner_radius > 0.0 then
            Hashtbl.replace radii (sprintf "radius-%s" name) (`Float corner_radius)
          else if top_left > 0.0 || top_right > 0.0 || bottom_right > 0.0 || bottom_left > 0.0 then
            Hashtbl.replace radii (sprintf "radius-%s" name) (`Assoc [
              ("topLeft", `Float top_left);
              ("topRight", `Float top_right);
              ("bottomRight", `Float bottom_right);
              ("bottomLeft", `Float bottom_left);
            ]);

          (* Effects (shadows, blurs) *)
          (match member "effects" n with
           | `List effs -> List.iteri (fun i eff ->
               let eff_type = member "type" eff |> to_string_option |> Option.value ~default:"" in
               let visible = member "visible" eff |> to_bool_option |> Option.value ~default:true in
               if visible then begin
                 match eff_type with
                 | "DROP_SHADOW" | "INNER_SHADOW" ->
                     let offset_x = member "offset" eff |> member "x" |> to_num in
                     let offset_y = member "offset" eff |> member "y" |> to_num in
                     let blur = member "radius" eff |> to_num in
                     let spread = member "spread" eff |> to_num in
                     let color = member "color" eff in
                     let r = member "r" color |> to_num in
                     let g = member "g" color |> to_num in
                     let b = member "b" color |> to_num in
                     let a = member "a" color |> to_num in
                     Hashtbl.replace effects (sprintf "shadow-%s-%d" name i) (`Assoc [
                       ("type", `String eff_type);
                       ("offsetX", `Float offset_x);
                       ("offsetY", `Float offset_y);
                       ("blur", `Float blur);
                       ("spread", `Float spread);
                       ("color", `String (rgba_to_hex r g b a));
                     ])
                 | "LAYER_BLUR" | "BACKGROUND_BLUR" ->
                     let blur = member "radius" eff |> to_num in
                     Hashtbl.replace effects (sprintf "blur-%s-%d" name i) (`Assoc [
                       ("type", `String eff_type);
                       ("radius", `Float blur);
                     ])
                 | _ -> ()
               end
             ) effs
           | _ -> ());

          (* Recurse into children *)
          (match member "children" n with
           | `List kids -> List.iter extract_from_node kids
           | _ -> ())
        in

        extract_from_node node;

        (* Build output based on format *)
        let color_list = Hashtbl.fold (fun k v acc -> (k, `String v) :: acc) colors [] in
        let typo_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) typography [] in
        let spacing_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) spacing [] in
        let radii_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) radii [] in
        let effects_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) effects [] in

        let result = match format with
          | "css-variables" ->
              (* CSS Custom Properties format *)
              let css_vars = Buffer.create 1024 in
              Buffer.add_string css_vars ":root {\n";
              List.iter (fun (k, v) ->
                match v with `String s -> Buffer.add_string css_vars (sprintf "  --%s: %s;\n" k s) | _ -> ()
              ) color_list;
              List.iter (fun (k, v) ->
                match v with `Float f -> Buffer.add_string css_vars (sprintf "  --%s: %.0fpx;\n" k f) | _ -> ()
              ) spacing_list;
              List.iter (fun (k, v) ->
                match v with `Float f -> Buffer.add_string css_vars (sprintf "  --%s: %.0fpx;\n" k f) | _ -> ()
              ) radii_list;
              Buffer.add_string css_vars "}\n";
              `Assoc [
                ("format", `String "css-variables");
                ("css", `String (Buffer.contents css_vars));
                ("tokenCount", `Int (List.length color_list + List.length typo_list + List.length spacing_list));
              ]
          | _ -> (* tokens-studio format *)
              `Assoc [
                ("format", `String "tokens-studio");
                ("tokens", `Assoc [
                  ("colors", `Assoc color_list);
                  ("typography", `Assoc typo_list);
                  ("spacing", `Assoc spacing_list);
                  ("borderRadius", `Assoc radii_list);
                  ("effects", `Assoc effects_list);
                ]);
                ("stats", `Assoc [
                  ("colors", `Int (List.length color_list));
                  ("typography", `Int (List.length typo_list));
                  ("spacing", `Int (List.length spacing_list));
                  ("borderRadius", `Int (List.length radii_list));
                  ("effects", `Int (List.length effects_list));
                  ("total", `Int (List.length color_list + List.length typo_list +
                                  List.length spacing_list + List.length radii_list + List.length effects_list));
                ]);
              ]
        in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/extract-variants - Extract component variants from Figma component set *)
let extract_variants_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let _to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract variant properties from component set or variants *)
        let variants = ref [] in
        let props = Hashtbl.create 16 in

        let rec extract_variants_from ?(parent_is_set=false) n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in

          (* If parent is COMPONENT_SET and child has no type, treat as COMPONENT *)
          let effective_type = if node_type = "" && parent_is_set then "COMPONENT" else node_type in

          (* Parse variant name like "State=Default, Size=Medium" *)
          if effective_type = "COMPONENT" then begin
            let parts = String.split_on_char ',' name in
            let variant_props = List.filter_map (fun part ->
              match String.split_on_char '=' (String.trim part) with
              | [key; value] ->
                  let k = String.trim key in
                  let v = String.trim value in
                  (* Collect all values for each property *)
                  let existing = try Hashtbl.find props k with Not_found -> [] in
                  if not (List.mem v existing) then Hashtbl.replace props k (v :: existing);
                  Some (k, v)
              | _ -> None
            ) parts in
            if variant_props <> [] then
              variants := (name, variant_props, n) :: !variants
          end;

          (* Recurse into children (for component sets) *)
          let is_set = effective_type = "COMPONENT_SET" in
          (match member "children" n with
           | `List kids -> List.iter (extract_variants_from ~parent_is_set:is_set) kids
           | _ -> ())
        in
        extract_variants_from ~parent_is_set:false node;

        (* Generate TypeScript types from variants *)
        let prop_types = Hashtbl.fold (fun key values acc ->
          let union = String.concat " | " (List.map (sprintf "'%s'") (List.rev values)) in
          sprintf "  %s: %s;" key union :: acc
        ) props [] in

        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in

        let ts_types = sprintf {|interface %sProps {
%s
}

type %sVariant = {
  name: string;
  props: Partial<%sProps>;
};
|} safe_name (String.concat "\n" prop_types) safe_name safe_name in

        (* Build variant mapping *)
        let variant_list = List.map (fun (name, vprops, _n) ->
          let props_json = List.map (fun (k, v) -> (k, `String v)) vprops in
          `Assoc [("name", `String name); ("props", `Assoc props_json)]
        ) !variants in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("variantCount", `Int (List.length !variants));
          ("properties", `Assoc (Hashtbl.fold (fun k v acc ->
            (k, `List (List.map (fun x -> `String x) (List.rev v))) :: acc
          ) props []));
          ("variants", `List variant_list);
          ("typescript", `String ts_types);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

[@@@coverage off]
(* POST /plugin/extract-animations - Extract animations and generate CSS/SwiftUI *)
let extract_animations_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract transition/animation properties from prototype interactions *)
        let animations = ref [] in

        let rec extract_anims n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in

          (* Safe member access for nested JSON *)
          let safe_member_type obj =
            match obj with
            | `Null -> ""
            | _ -> member "type" obj |> to_string_option |> Option.value ~default:""
          in

          (* Check for Figma prototype interactions *)
          (match member "reactions" n with
           | `List reactions -> List.iter (fun reaction ->
               let trigger_obj = member "trigger" reaction in
               let trigger = safe_member_type trigger_obj in
               let action = member "action" reaction in
               let anim_type = safe_member_type action in

               if anim_type = "NODE" then begin
                 let transition = member "transition" action in
                 let trans_type = match transition with
                   | `Null -> "DISSOLVE"
                   | _ -> member "type" transition |> to_string_option |> Option.value ~default:"DISSOLVE"
                 in
                 let duration = match transition with
                   | `Null -> 0.3
                   | _ -> member "duration" transition |> to_num
                 in
                 let easing_obj = match transition with
                   | `Null -> `Null
                   | _ -> member "easing" transition
                 in
                 let easing = safe_member_type easing_obj in
                 let easing = if easing = "" then "EASE_OUT" else easing in

                 animations := `Assoc [
                   ("element", `String name);
                   ("trigger", `String trigger);
                   ("type", `String trans_type);
                   ("duration", `Float duration);
                   ("easing", `String easing);
                 ] :: !animations
               end
             ) reactions
           | _ -> ());

          (* Also check for smart animate properties *)
          let opacity = member "opacity" n |> to_num in
          let rotation = member "rotation" n |> to_num in
          if opacity < 1.0 || rotation <> 0.0 then begin
            animations := `Assoc [
              ("element", `String name);
              ("trigger", `String "state_change");
              ("properties", `Assoc [
                ("opacity", `Float opacity);
                ("rotation", `Float rotation);
              ]);
            ] :: !animations
          end;

          (match member "children" n with
           | `List kids -> List.iter extract_anims kids
           | _ -> ())
        in
        extract_anims node;

        (* Generate CSS keyframes based on actual transition type *)
        let safe_num key obj = match obj with `Null -> 0.0 | _ -> member key obj |> to_num in
        let generate_css_keyframes trans_type props =
          let opacity_from = safe_num "opacity" props in
          let opacity_to = if opacity_from < 1.0 then 1.0 else opacity_from in
          let rotation = safe_num "rotation" props in
          match trans_type with
          | "DISSOLVE" ->
              sprintf "from { opacity: %.2f; }\n  to { opacity: %.2f; }" opacity_from opacity_to
          | "MOVE_IN" | "SLIDE_IN" ->
              sprintf "from { opacity: 0; transform: translateX(-100%%); }\n  to { opacity: 1; transform: translateX(0); }"
          | "MOVE_OUT" | "SLIDE_OUT" ->
              sprintf "from { opacity: 1; transform: translateX(0); }\n  to { opacity: 0; transform: translateX(100%%); }"
          | "PUSH" ->
              sprintf "from { transform: scale(0.8); opacity: 0; }\n  to { transform: scale(1); opacity: 1; }"
          | "SMART_ANIMATE" ->
              let rot_css = if rotation <> 0.0 then sprintf " rotate(%.1fdeg)" rotation else "" in
              sprintf "from { opacity: %.2f; transform: translateY(20px)%s; }\n  to { opacity: 1; transform: translateY(0)%s; }"
                opacity_from rot_css rot_css
          | _ -> (* Default fade-in *)
              sprintf "from { opacity: %.2f; }\n  to { opacity: 1; }" (min opacity_from 0.0)
        in

        let css_animations = String.concat "\n\n" (List.mapi (fun i anim ->
          let name = member "element" anim |> to_string_option |> Option.value ~default:"element" in
          let duration = member "duration" anim |> to_num in
          let dur = if duration > 0.0 then duration else 0.3 in
          let easing = member "easing" anim |> to_string_option |> Option.value ~default:"EASE_OUT" in
          let trans_type = member "type" anim |> to_string_option |> Option.value ~default:"DISSOLVE" in
          let props = member "properties" anim in
          let css_easing = match easing with
            | "EASE_IN" -> "ease-in"
            | "EASE_OUT" -> "ease-out"
            | "EASE_IN_AND_OUT" -> "ease-in-out"
            | "LINEAR" -> "linear"
            | "CUSTOM_BEZIER" -> "cubic-bezier(0.4, 0, 0.2, 1)"
            | _ -> "ease-out"
          in
          let keyframes = generate_css_keyframes trans_type props in
          sprintf {|/* Animation %d: %s (type: %s) */
@keyframes %s_anim {
  %s
}

.%s {
  animation: %s_anim %.2fs %s forwards;
}|} i name trans_type name keyframes name name dur css_easing
        ) !animations) in

        (* Generate SwiftUI animations based on actual transition type *)
        let generate_swift_modifiers trans_type props =
          let opacity_from = safe_num "opacity" props in
          let rotation = safe_num "rotation" props in
          match trans_type with
          | "DISSOLVE" ->
              sprintf ".opacity(isAnimating ? 1 : %.2f)" opacity_from
          | "MOVE_IN" | "SLIDE_IN" ->
              ".opacity(isAnimating ? 1 : 0)\n.offset(x: isAnimating ? 0 : -UIScreen.main.bounds.width)"
          | "MOVE_OUT" | "SLIDE_OUT" ->
              ".opacity(isAnimating ? 0 : 1)\n.offset(x: isAnimating ? UIScreen.main.bounds.width : 0)"
          | "PUSH" ->
              ".scaleEffect(isAnimating ? 1 : 0.8)\n.opacity(isAnimating ? 1 : 0)"
          | "SMART_ANIMATE" ->
              let rot_mod = if rotation <> 0.0 then sprintf "\n.rotationEffect(.degrees(isAnimating ? 0 : %.1f))" rotation else "" in
              sprintf ".opacity(isAnimating ? 1 : %.2f)\n.offset(y: isAnimating ? 0 : 20)%s" opacity_from rot_mod
          | _ ->
              sprintf ".opacity(isAnimating ? 1 : %.2f)" (min opacity_from 0.0)
        in

        let swiftui_animations = String.concat "\n\n" (List.mapi (fun i anim ->
          let name = member "element" anim |> to_string_option |> Option.value ~default:"element" in
          let duration = member "duration" anim |> to_num in
          let dur = if duration > 0.0 then duration else 0.3 in
          let easing = member "easing" anim |> to_string_option |> Option.value ~default:"EASE_OUT" in
          let trans_type = member "type" anim |> to_string_option |> Option.value ~default:"DISSOLVE" in
          let props = member "properties" anim in
          let swift_easing = match easing with
            | "EASE_IN" -> ".easeIn"
            | "EASE_OUT" -> ".easeOut"
            | "EASE_IN_AND_OUT" -> ".easeInOut"
            | "LINEAR" -> ".linear"
            | "CUSTOM_BEZIER" -> ".timingCurve(0.4, 0, 0.2, 1)"
            | _ -> ".easeOut"
          in
          let modifiers = generate_swift_modifiers trans_type props in
          sprintf {|// Animation %d: %s (type: %s)
%s
.animation(%s(duration: %.2f), value: isAnimating)|} i name trans_type modifiers swift_easing dur
        ) !animations) in

        let result = `Assoc [
          ("animations", `List (List.rev !animations));
          ("count", `Int (List.length !animations));
          ("css", `String css_animations);
          ("swiftui", `String swiftui_animations);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )
