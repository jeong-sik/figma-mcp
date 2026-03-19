open Printf
open Yojson.Safe
open Mcp_helpers

let assoc_set key value = function
  | `Assoc fields -> `Assoc ((key, value) :: List.remove_assoc key fields)
  | _ -> `Assoc [ (key, value) ]

let assoc_set_if_missing key value json =
  match member key json with
  | Some _ -> json
  | None -> assoc_set key value json

let extract_tool_text = function
  | `Assoc [ ("content", `List (`Assoc fields :: _)) ] -> (
      match List.assoc_opt "text" fields with
      | Some (`String text) -> Some text
      | _ -> None)
  | `Assoc fields -> (
      match List.assoc_opt "content" fields with
      | Some (`List (`Assoc item_fields :: _)) -> (
          match List.assoc_opt "text" item_fields with
          | Some (`String text) -> Some text
          | _ -> None)
      | _ -> None)
  | _ -> None

let extract_tool_json result =
  match extract_tool_text result with
  | Some text -> (
      try Some (from_string text)
      with Yojson.Json_error _ -> None)
  | None -> None

let make_json_content json =
  make_text_content (pretty_to_string json)

let find_document_by_node_id json node_id =
  match member "nodes" json with
  | Some (`Assoc nodes_map) -> (
      match find_node_entry nodes_map ~node_id with
      | Some (_key, node_entry) -> member "document" node_entry
      | None -> None)
  | _ -> None

let resolve_document_json ~args ~default_depth =
  let file_key, node_id = resolve_file_key_node_id args in
  let token = resolve_token args in
  let depth = get_int "depth" args in
  let version = get_string "version" args in
  match (file_key, token) with
  | Some resolved_file_key, Some resolved_token -> (
      match node_id with
      | Some resolved_node_id -> (
          match
            Figma_effects.Perform.get_nodes ~token:resolved_token ~file_key:resolved_file_key
              ~node_ids:[ resolved_node_id ]
              ~depth:(Option.value depth ~default:default_depth)
              ?version ()
          with
          | Ok json -> (
              match find_document_by_node_id json resolved_node_id with
              | Some document -> Ok (resolved_file_key, Some resolved_node_id, document)
              | None ->
                  Error (sprintf "Node not found: %s" resolved_node_id))
          | Error err -> Error err)
      | None -> (
          match
            Figma_effects.Perform.get_file ~token:resolved_token ~file_key:resolved_file_key
              ~depth:(Option.value depth ~default:default_depth)
              ?version ()
          with
          | Ok json -> (
              match Figma_api.extract_document json with
              | Some document -> Ok (resolved_file_key, None, document)
              | None -> Error "Document not found")
          | Error err -> Error err))
  | _ -> Error "Missing required parameters: file_key/url, token"

let xml_escape s =
  let buf = Buffer.create (String.length s + 16) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let number_to_string = function
  | `Float f ->
      let i = int_of_float f in
      if Float.equal f (float_of_int i) then string_of_int i else string_of_float f
  | `Int i -> string_of_int i
  | _ -> ""

let bbox_fields node =
  match member "absoluteBoundingBox" node with
  | Some (`Assoc _ as bbox) ->
      let field name =
        number_to_string (member name bbox |> Option.value ~default:`Null)
      in
      [
        ("x", field "x");
        ("y", field "y");
        ("width", field "width");
        ("height", field "height");
      ]
  | _ ->
      [
        ("x", number_to_string (member "x" node |> Option.value ~default:`Null));
        ("y", number_to_string (member "y" node |> Option.value ~default:`Null));
        ("width", number_to_string (member "width" node |> Option.value ~default:`Null));
        ("height", number_to_string (member "height" node |> Option.value ~default:`Null));
      ]

let metadata_xml_of_node ?(depth=2) ?(max_children=100) node =
  let rec render ~level node =
    let indent = String.make (level * 2) ' ' in
    let id =
      match member "id" node with
      | Some (`String s) -> s
      | _ -> ""
    in
    let name =
      match member "name" node with
      | Some (`String s) -> s
      | _ -> ""
    in
    let node_type =
      match member "type" node with
      | Some (`String s) -> s
      | _ -> "UNKNOWN"
    in
    let attrs =
      [
        ("id", id);
        ("name", name);
        ("type", node_type);
      ]
      @ bbox_fields node
      |> List.filter_map (fun (k, v) ->
             let trimmed = String.trim v in
             if trimmed = "" then None
             else Some (sprintf "%s=\"%s\"" k (xml_escape trimmed)))
      |> String.concat " "
    in
    let children =
      match member "children" node with
      | Some (`List items) -> items
      | _ -> []
    in
    if level >= depth || children = [] then
      sprintf "%s<node %s />" indent attrs
    else
      let visible_children, truncated =
        if List.length children > max_children then
          (List.filteri (fun i _ -> i < max_children) children, true)
        else (children, false)
      in
      let rendered_children =
        visible_children
        |> List.map (render ~level:(level + 1))
        |> String.concat "\n"
      in
      let truncated_comment =
        if truncated then
          sprintf "\n%s  <!-- truncated: %d more children -->" indent
            (List.length children - max_children)
        else ""
      in
      sprintf "%s<node %s>\n%s%s\n%s</node>" indent attrs rendered_children
        truncated_comment indent
  in
  render ~level:0 node

let handle_get_design_context args =
  let bundle_args =
    args
    |> assoc_set_if_missing "format" (`String "fidelity")
    |> assoc_set_if_missing "include_meta" (`Bool true)
    |> assoc_set_if_missing "include_variables" (`Bool true)
    |> assoc_set_if_missing "include_image_fills" (`Bool false)
    |> assoc_set_if_missing "include_plugin" (`Bool false)
    |> assoc_set_if_missing "include_plugin_variables" (`Bool false)
  in
  match Mcp_api_bundle_handlers.handle_get_node_bundle bundle_args with
  | Error err -> Error err
  | Ok result -> (
      match extract_tool_json result with
      | Some (`Assoc fields) ->
          let request_hints =
            List.filter_map
              (fun (name, value_opt) ->
                value_opt |> Option.map (fun value -> (name, `String value)))
              [
                ("clientFrameworks", get_string "client_frameworks" args);
                ("clientLanguages", get_string "client_languages" args);
              ]
          in
          let code_connect =
            if get_bool_or "include_code_connect" false args then
              let selector_mode =
                match get_string "node_id" args with
                | Some _ -> `Assoc [ ("mode", `String "match") ]
                | None -> `Assoc [ ("mode", `String "index") ]
              in
              let cc_args =
                match selector_mode with
                | `Assoc fields ->
                    List.fold_left
                      (fun acc (key, value) -> assoc_set_if_missing key value acc)
                      args fields
                | _ -> args
              in
              match Mcp_tool_handlers.handle_code_connect cc_args with
              | Ok cc_result -> (
                  match extract_tool_json cc_result with
                  | Some json -> json
                  | None -> `Null)
              | Error msg -> `Assoc [ ("error", `String msg) ]
            else `Null
          in
          let extensions =
            `Assoc
              (("surface", `String "v2")
              :: request_hints
              @
              if code_connect = `Null then [] else [ ("codeConnect", code_connect) ])
          in
          Ok (make_json_content (`Assoc (("$extensions", extensions) :: fields)))
      | _ -> Ok result)

let handle_get_metadata args =
  let depth = get_int_positive "depth" 2 args in
  let max_children = get_int_positive "max_children" 100 args in
  match resolve_document_json ~args ~default_depth:depth with
  | Error err -> Error err
  | Ok (file_key, node_id, document) ->
      let body =
        sprintf "<metadata file_key=\"%s\"%s>\n%s\n</metadata>" (xml_escape file_key)
          (match node_id with
          | Some id -> sprintf " node_id=\"%s\"" (xml_escape id)
          | None -> "")
          (metadata_xml_of_node ~depth ~max_children document)
      in
      Ok (make_text_content body)

let handle_get_variable_defs args =
  let file_key =
    match resolve_url_info args with
    | Some info -> prefer_some (get_string "file_key" args) info.file_key
    | None -> get_string "file_key" args
  in
  let token = resolve_token args in
  let format = String.lowercase_ascii (get_string_or "format" "resolved" args) in
  match (file_key, token) with
  | Some resolved_file_key, Some resolved_token -> (
      match fetch_variables_cached ~file_key:resolved_file_key ~token:resolved_token with
      | Error err -> Error err
      | Ok (json, source) ->
          let data =
            match format with
            | "raw" -> json
            | "summary" ->
                let meta = member "meta" json |> Option.value ~default:`Null in
                let collection_count =
                  match member "variableCollections" meta with
                  | Some (`Assoc entries) -> List.length entries
                  | _ -> 0
                in
                let variable_count =
                  match member "variables" meta with
                  | Some (`Assoc entries) -> List.length entries
                  | _ -> 0
                in
                `Assoc
                  [
                    ("collections", `Int collection_count);
                    ("variables", `Int variable_count);
                  ]
            | _ -> resolve_variables json
          in
          Ok
            (make_json_content
               (`Assoc
                 [
                   ("fileKey", `String resolved_file_key);
                   ("source", source);
                   ("format", `String format);
                   ("data", data);
                 ])))
  | _ -> Error "Missing required parameters: file_key/url, token"

let handle_get_screenshot args =
  let file_key, node_id = resolve_file_key_node_id args in
  match (file_key, node_id) with
  | Some resolved_file_key, Some resolved_node_id ->
      let export_args =
        args
        |> assoc_set "file_key" (`String resolved_file_key)
        |> assoc_set "node_ids" (`String resolved_node_id)
        |> assoc_set_if_missing "format" (`String "png")
        |> assoc_set_if_missing "scale" (`Float 1.0)
      in
      Mcp_api_export_handlers.handle_export_image export_args
  | _ -> Error "Missing required parameters: file_key/node_id or url"

let handle_get_code_connect_map args =
  let explicit_mode = get_string "mode" args in
  let has_selector =
    List.exists Option.is_some
      [ get_string "node_id" args; get_string "component_key" args; get_string "name" args ]
  in
  let mode =
    match explicit_mode with
    | Some value -> value
    | None -> if has_selector then "match" else "index"
  in
  Mcp_tool_handlers.handle_code_connect (assoc_set "mode" (`String mode) args)

let handle_whoami args =
  match resolve_token args with
  | None -> Error "Missing required parameter: token (set FIGMA_TOKEN env var or pass explicitly)"
  | Some token -> (
      match Figma_effects.Perform.get_me ~token with
      | Ok json -> Ok (make_json_content json)
      | Error err -> Error err)

let handle_verify_semantic = Mcp_visual_handlers.handle_verify_semantic
let handle_verify_visual = Mcp_visual_handlers.handle_verify_visual
