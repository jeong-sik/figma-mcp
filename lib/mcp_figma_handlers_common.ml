(** Shared helpers and state for Figma tool handlers.

    This module holds core utilities that multiple handler modules need,
    avoiding circular dependencies between the facade and its sub-modules. *)

open Printf

(* Re-bind symbols from extracted modules *)
module Response = Mcp_http_helpers.Response
module Request = Mcp_http_helpers.Request
let json_error = Mcp_http_helpers.json_error
let parse_json = Mcp_http_helpers.parse_json
let get_string_field = Mcp_http_helpers.get_string_field
let get_int_field = Mcp_http_helpers.get_int_field
let get_bool_field = Mcp_http_helpers.get_bool_field
let get_payload_field = Mcp_http_helpers.get_payload_field

let parse_positive_int value =
  try
    let v = int_of_string value in
    if v > 0 then Some v else None
  with Failure _ -> None

let env_int ~name ~default =
  match Sys.getenv_opt name with
  | Some v -> (match parse_positive_int v with Some n -> n | None -> default)
  | None -> default

let allow_no_auth =
  ref (Mcp_http_auth.env_truthy "FIGMA_MCP_ALLOW_NO_AUTH"
       || Mcp_http_auth.env_truthy "MCP_ALLOW_NO_AUTH")
let set_allow_no_auth v = allow_no_auth := v

(* Semantic analyzer - extracts structured info with exact measurements *)
let analyze_node_semantic node =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 2048 in
  let add s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
  let to_int json = int_of_float (to_num json) in

  (* Extract design tokens (colors) *)
  let colors = Hashtbl.create 16 in
  let rec collect_colors n =
    (match member "fills" n with
     | `List fills -> List.iter (fun fill ->
         match member "color" fill with
         | `String c -> Hashtbl.replace colors c (member "name" n |> to_string_option |> Option.value ~default:"")
         | _ -> ()
       ) fills
     | _ -> ());
    (match member "children" n with
     | `List kids -> List.iter collect_colors kids
     | _ -> ())
  in
  collect_colors node;

  (* Root info *)
  let name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
  let w = member "width" node |> to_int in
  let h = member "height" node |> to_int in
  let bg = match member "fills" node with
    | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> c | _ -> "transparent")
    | _ -> "transparent"
  in

  add (sprintf "## Layout: %s (%dx%d, background: %s)" name w h bg);
  add "";

  (* Analyze children with exact measurements *)
  let rec analyze_child depth n =
    let indent = String.make (depth * 2) ' ' in
    let cname = member "name" n |> to_string_option |> Option.value ~default:"Layer" in
    let ctype = member "type" n |> to_string_option |> Option.value ~default:"" in
    let cx = member "x" node |> to_int in
    let cy = member "y" node |> to_int in
    let cw = member "width" n |> to_int in
    let ch = member "height" n |> to_int in
    let radius = member "cornerRadius" n |> to_num in

    (* Component pattern detection *)
    let pattern =
      let ln = String.lowercase_ascii cname in
      if String.length ln >= 4 && String.sub ln 0 4 = "side" then "Sidebar"
      else if String.length ln >= 6 && String.sub ln 0 6 = "header" then "Header"
      else if String.length ln >= 4 && String.sub ln 0 4 = "card" then "Card"
      else if String.length ln >= 4 && String.sub ln 0 4 = "stat" then "StatCard"
      else if String.length ln >= 3 && String.sub ln 0 3 = "nav" then "NavItem"
      else if String.length ln >= 6 && String.sub ln 0 6 = "button" then "Button"
      else if String.length ln >= 6 && String.sub ln 0 6 = "search" then "SearchInput"
      else if String.length ln >= 4 && String.sub ln 0 4 = "logo" then "Logo"
      else if String.length ln >= 6 && String.sub ln 0 6 = "avatar" then "Avatar"
      else ""
    in

    let pattern_hint = if pattern <> "" then sprintf " [%s]" pattern else "" in

    (match ctype with
     | "TEXT" ->
         let text = member "text" n in
         let chars = member "characters" text |> to_string_option |> Option.value ~default:"" in
         let fontSize = member "fontSize" text |> to_num in
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> c | _ -> "")
           | _ -> ""
         in
         add (sprintf "%s- TEXT \"%s\": %dpx, color %s%s" indent chars (int_of_float fontSize) fill pattern_hint)
     | "FRAME" | "RECTANGLE" | "GROUP" ->
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> ", bg " ^ c | _ -> "")
           | _ -> ""
         in
         let rad = if radius > 0.0 then sprintf ", radius %.0fpx" radius else "" in
         add (sprintf "%s- %s \"%s\": %dx%d at (%d,%d)%s%s%s" indent ctype cname cw ch cx cy fill rad pattern_hint);
         (match member "children" n with
          | `List kids when depth < 3 -> List.iter (analyze_child (depth + 1)) kids
          | `List kids -> add (sprintf "%s  ... %d more children" indent (List.length kids))
          | _ -> ())
     | "ELLIPSE" ->
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> ", fill " ^ c | _ -> "")
           | _ -> ""
         in
         add (sprintf "%s- ELLIPSE \"%s\": %dx%d%s%s" indent cname cw ch fill pattern_hint)
     | _ ->
         add (sprintf "%s- %s \"%s\": %dx%d%s" indent ctype cname cw ch pattern_hint));
  in

  (match member "children" node with
   | `List kids ->
       add "## Structure:";
       List.iter (analyze_child 0) kids
   | _ -> ());

  (* Design tokens *)
  add "";
  add "## Design Tokens:";
  Hashtbl.iter (fun color ctx ->
    add (sprintf "- %s (used in: %s)" color (if ctx = "" then "fill" else ctx))
  ) colors;

  Buffer.contents buf
