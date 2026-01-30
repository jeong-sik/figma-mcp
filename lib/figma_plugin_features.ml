(** Figma Plugin Features - Animation, Accessibility, Variants, Responsive, Webhook *)

open Printf

(** Safe JSON member access *)
let safe_member_type obj =
  let open Yojson.Safe.Util in
  match obj with
  | `Null -> ""
  | _ -> member "type" obj |> to_string_option |> Option.value ~default:""

let to_num json =
  match json with
  | `Float f -> f
  | `Int i -> float_of_int i
  | _ -> 0.0

let safe_num key obj =
  let open Yojson.Safe.Util in
  match obj with
  | `Null -> 0.0
  | _ -> member key obj |> to_num

(** Check if name contains keyword (anywhere, not just prefix) *)
let contains_word s word =
  let re = Str.regexp_string word in
  try ignore (Str.search_forward re s 0); true
  with Not_found -> false

(* ============== Animation Extraction ============== *)

type animation = {
  element: string;
  trigger: string;
  trans_type: string;
  duration: float;
  easing: string;
  properties: (string * float) list;
}

let extract_animations node =
  let open Yojson.Safe.Util in
  let animations = ref [] in

  let rec extract_anims n =
    let name = member "name" n |> to_string_option |> Option.value ~default:"" in

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

           animations := {
             element = name;
             trigger;
             trans_type;
             duration;
             easing;
             properties = [];
           } :: !animations
         end
       ) reactions
     | _ -> ());

    (* Also check for smart animate properties *)
    let opacity = member "opacity" n |> to_num in
    let rotation = member "rotation" n |> to_num in
    if opacity < 1.0 || rotation <> 0.0 then begin
      animations := {
        element = name;
        trigger = "state_change";
        trans_type = "DISSOLVE";
        duration = 0.3;
        easing = "EASE_OUT";
        properties = [("opacity", opacity); ("rotation", rotation)];
      } :: !animations
    end;

    (match member "children" n with
     | `List kids -> List.iter extract_anims kids
     | _ -> ())
  in
  extract_anims node;
  List.rev !animations

let generate_css_keyframes trans_type opacity rotation =
  match trans_type with
  | "DISSOLVE" ->
      sprintf "from { opacity: %.2f; }\n  to { opacity: 1.00; }" opacity
  | "MOVE_IN" | "SLIDE_IN" ->
      "from { opacity: 0; transform: translateX(-100%); }\n  to { opacity: 1; transform: translateX(0); }"
  | "MOVE_OUT" | "SLIDE_OUT" ->
      "from { opacity: 1; transform: translateX(0); }\n  to { opacity: 0; transform: translateX(100%); }"
  | "PUSH" ->
      "from { transform: scale(0.8); opacity: 0; }\n  to { transform: scale(1); opacity: 1; }"
  | "SMART_ANIMATE" ->
      let rot_css = if rotation <> 0.0 then sprintf " rotate(%.1fdeg)" rotation else "" in
      sprintf "from { opacity: %.2f; transform: translateY(20px)%s; }\n  to { opacity: 1; transform: translateY(0)%s; }"
        opacity rot_css rot_css
  | _ ->
      sprintf "from { opacity: %.2f; }\n  to { opacity: 1; }" (min opacity 0.0)

let css_easing_of_figma = function
  | "EASE_IN" -> "ease-in"
  | "EASE_OUT" -> "ease-out"
  | "EASE_IN_AND_OUT" -> "ease-in-out"
  | "LINEAR" -> "linear"
  | "CUSTOM_BEZIER" -> "cubic-bezier(0.4, 0, 0.2, 1)"
  | _ -> "ease-out"

let swift_easing_of_figma = function
  | "EASE_IN" -> ".easeIn"
  | "EASE_OUT" -> ".easeOut"
  | "EASE_IN_AND_OUT" -> ".easeInOut"
  | "LINEAR" -> ".linear"
  | "CUSTOM_BEZIER" -> ".timingCurve(0.4, 0, 0.2, 1)"
  | _ -> ".easeOut"

let animation_to_css i anim =
  let opacity = List.assoc_opt "opacity" anim.properties |> Option.value ~default:0.0 in
  let rotation = List.assoc_opt "rotation" anim.properties |> Option.value ~default:0.0 in
  let dur = if anim.duration > 0.0 then anim.duration else 0.3 in
  let css_easing = css_easing_of_figma anim.easing in
  let keyframes = generate_css_keyframes anim.trans_type opacity rotation in
  sprintf {|/* Animation %d: %s (type: %s) */
@keyframes %s_anim {
  %s
}

.%s {
  animation: %s_anim %.2fs %s forwards;
}|} i anim.element anim.trans_type anim.element keyframes anim.element anim.element dur css_easing

let generate_swift_modifiers trans_type opacity rotation =
  match trans_type with
  | "DISSOLVE" ->
      sprintf ".opacity(isAnimating ? 1 : %.2f)" opacity
  | "MOVE_IN" | "SLIDE_IN" ->
      ".opacity(isAnimating ? 1 : 0)\n.offset(x: isAnimating ? 0 : -UIScreen.main.bounds.width)"
  | "MOVE_OUT" | "SLIDE_OUT" ->
      ".opacity(isAnimating ? 0 : 1)\n.offset(x: isAnimating ? UIScreen.main.bounds.width : 0)"
  | "PUSH" ->
      ".scaleEffect(isAnimating ? 1 : 0.8)\n.opacity(isAnimating ? 1 : 0)"
  | "SMART_ANIMATE" ->
      let rot_mod = if rotation <> 0.0 then sprintf "\n.rotationEffect(.degrees(isAnimating ? 0 : %.1f))" rotation else "" in
      sprintf ".opacity(isAnimating ? 1 : %.2f)\n.offset(y: isAnimating ? 0 : 20)%s" opacity rot_mod
  | _ ->
      sprintf ".opacity(isAnimating ? 1 : %.2f)" (min opacity 0.0)

let animation_to_swiftui i anim =
  let opacity = List.assoc_opt "opacity" anim.properties |> Option.value ~default:0.0 in
  let rotation = List.assoc_opt "rotation" anim.properties |> Option.value ~default:0.0 in
  let dur = if anim.duration > 0.0 then anim.duration else 0.3 in
  let swift_easing = swift_easing_of_figma anim.easing in
  let modifiers = generate_swift_modifiers anim.trans_type opacity rotation in
  sprintf {|// Animation %d: %s (type: %s)
%s
.animation(%s(duration: %.2f), value: isAnimating)|} i anim.element anim.trans_type modifiers swift_easing dur

let animations_to_json animations =
  let anim_json = List.map (fun a ->
    let props = `Assoc (List.map (fun (k, v) -> (k, `Float v)) a.properties) in
    `Assoc [
      ("element", `String a.element);
      ("trigger", `String a.trigger);
      ("type", `String a.trans_type);
      ("duration", `Float a.duration);
      ("easing", `String a.easing);
      ("properties", props);
    ]
  ) animations in
  let css = String.concat "\n\n" (List.mapi animation_to_css animations) in
  let swiftui = String.concat "\n\n" (List.mapi animation_to_swiftui animations) in
  `Assoc [
    ("animations", `List anim_json);
    ("count", `Int (List.length animations));
    ("css", `String css);
    ("swiftui", `String swiftui);
  ]

(* ============== Accessibility Analysis ============== *)

type aria_attr = {
  element: string;
  role: string;
  attributes: (string * Yojson.Safe.t) list;
}

let analyze_accessibility node =
  let open Yojson.Safe.Util in
  let suggestions = ref [] in
  let aria_attrs = ref [] in

  let rec analyze n =
    let name = member "name" n |> to_string_option |> Option.value ~default:"" in
    let ln = String.lowercase_ascii name in

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
      aria_attrs := {
        element = name;
        role = "button";
        attributes = [
          ("aria-label", `String name);
          ("tabIndex", `Int 0);
        ];
      } :: !aria_attrs;
      suggestions := sprintf "Button '%s': Add aria-label and ensure keyboard accessibility" name :: !suggestions
    end;

    if is_link then
      aria_attrs := { element = name; role = "link"; attributes = [("href", `String "#")] } :: !aria_attrs;

    if is_input then begin
      aria_attrs := {
        element = name;
        role = "textbox";
        attributes = [("aria-label", `String name); ("aria-required", `Bool false)];
      } :: !aria_attrs;
      suggestions := sprintf "Input '%s': Add label element or aria-label" name :: !suggestions
    end;

    if is_image then begin
      aria_attrs := {
        element = name;
        role = "img";
        attributes = [("alt", `String name)];
      } :: !aria_attrs;
      suggestions := sprintf "Image '%s': Ensure alt text is descriptive" name :: !suggestions
    end;

    if is_nav then
      aria_attrs := { element = name; role = "navigation"; attributes = [("aria-label", `String name)] } :: !aria_attrs;

    if is_modal then begin
      aria_attrs := {
        element = name;
        role = "dialog";
        attributes = [("aria-modal", `Bool true); ("aria-labelledby", `String (name ^ "-title"))];
      } :: !aria_attrs;
      suggestions := sprintf "Modal '%s': Trap focus and add escape key handler" name :: !suggestions
    end;

    (match member "children" n with
     | `List kids -> List.iter analyze kids
     | _ -> ())
  in
  analyze node;
  (!aria_attrs, !suggestions)

let accessibility_to_json (aria_attrs, suggestions) =
  let attrs_json = List.map (fun a ->
    `Assoc ([
      ("element", `String a.element);
      ("role", `String a.role);
    ] @ a.attributes)
  ) aria_attrs in
  let interactive_count = List.length aria_attrs in
  `Assoc [
    ("ariaAttributes", `List attrs_json);
    ("suggestions", `List (List.map (fun s -> `String s) suggestions));
    ("summary", `Assoc [
      ("interactiveElements", `Int interactive_count);
      ("suggestions", `Int (List.length suggestions));
    ]);
  ]

(* ============== Variant Extraction ============== *)

let extract_variants node =
  let open Yojson.Safe.Util in
  let variants = ref [] in
  let props = Hashtbl.create 16 in

  let rec extract ?(parent_is_set=false) n =
    let name = member "name" n |> to_string_option |> Option.value ~default:"" in
    let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
    let effective_type = if node_type = "" && parent_is_set then "COMPONENT" else node_type in

    if effective_type = "COMPONENT" then begin
      let parts = String.split_on_char ',' name in
      let variant_props = List.filter_map (fun part ->
        match String.split_on_char '=' (String.trim part) with
        | [key; value] ->
            let k = String.trim key in
            let v = String.trim value in
            let existing = try Hashtbl.find props k with Not_found -> [] in
            if not (List.mem v existing) then Hashtbl.replace props k (v :: existing);
            Some (k, v)
        | _ -> None
      ) parts in
      if variant_props <> [] then
        variants := (name, variant_props) :: !variants
    end;

    let is_set = effective_type = "COMPONENT_SET" in
    (match member "children" n with
     | `List kids -> List.iter (extract ~parent_is_set:is_set) kids
     | _ -> ())
  in
  extract ~parent_is_set:false node;
  (!variants, props)

let variants_to_json component_name (variants, props) =
  let prop_types = Hashtbl.fold (fun key values acc ->
    let union = String.concat " | " (List.map (sprintf "'%s'") (List.rev values)) in
    sprintf "  %s: %s;" key union :: acc
  ) props [] in

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

  let variant_list = List.map (fun (name, vprops) ->
    let props_json = List.map (fun (k, v) -> (k, `String v)) vprops in
    `Assoc [("name", `String name); ("props", `Assoc props_json)]
  ) variants in

  let props_json = Hashtbl.fold (fun k v acc ->
    (k, `List (List.map (fun s -> `String s) (List.rev v))) :: acc
  ) props [] in

  `Assoc [
    ("componentName", `String component_name);
    ("variantCount", `Int (List.length variants));
    ("properties", `Assoc props_json);
    ("variants", `List variant_list);
    ("typescript", `String ts_types);
  ]

(* ============== Responsive Breakpoints ============== *)

let generate_responsive_css component_name base_width base_height =
  let breakpoints = [
    ("mobile", 375, 0.8);
    ("tablet", 768, 1.2);
    ("desktop", 1440, 1.5);
  ] in

  let css_rules = List.map (fun (_bp_name, bp_width, scale) ->
    let scaled_w = int_of_float (float_of_int base_width *. scale) in
    let scaled_h = int_of_float (float_of_int base_height *. scale) in
    sprintf "@media (min-width: %dpx) {\n  .%s {\n    width: %dpx;\n    height: %dpx;\n  }\n}"
      bp_width (String.lowercase_ascii component_name) scaled_w scaled_h
  ) breakpoints in

  let bp_json = List.map (fun (name, width, _) ->
    `Assoc [("name", `String name); ("width", `Int width)]
  ) breakpoints in

  `Assoc [
    ("componentName", `String component_name);
    ("baseWidth", `Int base_width);
    ("breakpoints", `List bp_json);
    ("css", `String (String.concat "\n\n" css_rules));
  ]

(* ============== Webhook Handler ============== *)

let process_webhook event_type file_key file_name timestamp =
  let action = match event_type with
    | "FILE_UPDATE" -> "regenerate_code"
    | "LIBRARY_PUBLISH" -> "sync_design_tokens"
    | "FILE_VERSION_UPDATE" -> "check_version_diff"
    | "FILE_COMMENT" -> "review_comment"
    | _ -> "log_event"
  in
  `Assoc [
    ("status", `String "received");
    ("event_type", `String event_type);
    ("file_key", `String file_key);
    ("file_name", `String file_name);
    ("timestamp", `String timestamp);
    ("recommended_action", `String action);
    ("webhook_endpoints", `Assoc [
      ("codegen", `String "/plugin/codegen");
      ("tokens", `String "/plugin/extract-tokens");
      ("variants", `String "/plugin/extract-variants");
    ]);
  ]
