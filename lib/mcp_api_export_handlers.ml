(** Export/download handlers for Figma REST API nodes. *)

open Mcp_helpers
open Mcp_api_handler_support
open Printf

let handle_export_image args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_ids_str = get_string "node_ids" args in
  let token = resolve_token args in
  let format = get_string_or "format" "png" args in
  let scale = get_float_or "scale" 1.0 args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let version = get_string "version" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  match (file_key, node_ids_str, token) with
  | (Some file_key, Some node_ids_str, Some token) ->
      let node_ids =
        node_ids_str
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map normalize_node_id
      in
      (match Figma_effects.Perform.get_images ~token ~file_key ~node_ids ~format ~scale
               ?use_absolute_bounds ?version () with
       | Ok json ->
           let images = member "images" json in
           let result = match images with
             | Some (`Assoc img_map) ->
                 let total = List.length img_map in
                 let progress_token =
                   if download && total >= 3 then
                     Some (Mcp_progress.make_progress_token ())
                   else None
                 in
                 let _ = match progress_token with
                   | Some pt ->
                       Mcp_progress.update_progress ~token:pt ~current:0 ~total
                         ~message:(sprintf "Starting export of %d images..." total) ()
                   | None -> ()
                 in
                 let results = List.mapi (fun idx (id, url) ->
                   let result_str = match url with
                     | `String url ->
                         if download then
                           if is_http_url url then
                             let path = Printf.sprintf "%s/%s/%s.%s"
                               save_dir (sanitize_file_key file_key) (sanitize_node_id id) format in
                             (match Figma_effects.Perform.download_url ~url ~path with
                              | Ok saved -> sprintf "%s: %s -> %s" id url saved
                              | Error err -> sprintf "%s: %s (download error: %s)" id url err)
                           else
                             sprintf "%s: %s (download skipped: no URL)" id url
                         else
                           sprintf "%s: %s" id url
                     | _ -> sprintf "%s: (error)" id
                   in
                   let _ = match progress_token with
                     | Some pt ->
                         Mcp_progress.update_progress ~token:pt ~current:(idx + 1) ~total
                           ~message:(sprintf "Downloaded %d/%d: %s" (idx + 1) total id) ()
                     | None -> ()
                   in
                   result_str
                 ) img_map in
                 let _ = match progress_token with
                   | Some pt ->
                       Mcp_progress.update_progress ~token:pt ~current:total ~total
                         ~message:(sprintf "Export complete: %d images" total) ()
                   | None -> ()
                 in
                 String.concat "\n" results
             | _ -> "No images returned"
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, node_ids, token"

let handle_export_smart args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = resolve_token args in
  let format = get_string_or "format" "png" args in
  let max_pixels = get_float_or "max_pixels" 16777216.0 args in
  let split_children = get_bool_or "split_children" false args in
  let max_depth = Option.value ~default:1 (get_int "max_depth" args) in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  let include_debug = get_bool_or "debug" false args in
  let auto_scale ~width ~height =
    let actual = float_of_int (width * height) in
    if actual <= max_pixels then 1.0
    else
      let ratio = sqrt (max_pixels /. actual) in
      max 0.01 (min 4.0 ratio)
  in
  let get_node_dims json =
    match member "absoluteBoundingBox" json with
    | Some box ->
        let w = match member "width" box with Some (`Float f) -> int_of_float f | Some (`Int i) -> i | _ -> 0 in
        let h = match member "height" box with Some (`Float f) -> int_of_float f | Some (`Int i) -> i | _ -> 0 in
        (w, h)
    | None -> (0, 0)
  in
  let get_child_ids json =
    match member "children" json with
    | Some (`List children) ->
        List.filter_map (fun child ->
          match member "id" child with
          | Some (`String id) -> Some id
          | _ -> None
        ) children
    | _ -> []
  in
  let export_node ~node_id ~scale =
    match Figma_effects.Perform.get_images ~token:(Option.get token)
            ~file_key:(Option.get file_key) ~node_ids:[node_id] ~format
            ~scale () with
    | Ok json ->
        (match member "images" json with
         | Some (`Assoc img_map) ->
             List.filter_map (fun (id, url) ->
               match url with
               | `String url_str ->
                   let final_path =
                     if download && is_http_url url_str then
                      let path = Printf.sprintf "%s/%s/%s.%s"
                        save_dir (sanitize_file_key (Option.get file_key)) (sanitize_node_id id) format in
                       match Figma_effects.Perform.download_url ~url:url_str ~path with
                       | Ok saved -> Some saved
                       | Error _ -> Some url_str
                     else Some url_str
                   in
                   Option.map (fun p -> `Assoc [
                     ("node_id", `String id);
                     ("url", `String url_str);
                     ("scale", `Float scale);
                     ("path", `String p);
                   ]) final_path
               | _ -> None
             ) img_map
         | _ -> [])
    | Error _ -> []
  in
  let debug_info = ref [] in
  let rec export_recursive ~node_id ~depth results =
    if depth > max_depth then results
    else
      match Figma_effects.Perform.get_nodes ~token:(Option.get token)
              ~file_key:(Option.get file_key) ~node_ids:[node_id] ~depth:1 () with
      | Ok json ->
          let nodes_opt = member "nodes" json in
          let node_json = match nodes_opt with
            | Some (`Assoc nodes) ->
                debug_info := !debug_info @ [Printf.sprintf "Found nodes with %d entries, looking for '%s'" (List.length nodes) node_id];
                debug_info := !debug_info @ [Printf.sprintf "Available keys: %s" (String.concat ", " (List.map fst nodes))];
                (match List.assoc_opt node_id nodes with
                 | Some node_data ->
                     let node_data_str = Yojson.Safe.to_string node_data in
                     let truncated = if String.length node_data_str > 200 then String.sub node_data_str 0 200 ^ "..." else node_data_str in
                     debug_info := !debug_info @ [Printf.sprintf "Found node_data: %s" truncated];
                     let doc_opt = member "document" node_data in
                     (match doc_opt with
                      | Some doc -> debug_info := !debug_info @ ["document found!"]; Some doc
                      | None ->
                          let keys = match node_data with `Assoc lst -> List.map fst lst | _ -> [] in
                          debug_info := !debug_info @ [Printf.sprintf "document NOT found. node_data keys: %s" (String.concat ", " keys)];
                          None)
                 | None ->
                     debug_info := !debug_info @ ["Node ID not found in nodes"];
                     None)
            | Some other ->
                let str = Yojson.Safe.to_string other in
                let truncated = if String.length str > 100 then String.sub str 0 100 ^ "..." else str in
                debug_info := !debug_info @ [Printf.sprintf "nodes is not Assoc: %s" truncated];
                None
            | None ->
                debug_info := !debug_info @ ["No 'nodes' key in response"];
                None
          in
          let (w, h) = match node_json with
            | Some n ->
                let dims = get_node_dims n in
                debug_info := !debug_info @ [Printf.sprintf "Got dimensions: %dx%d" (fst dims) (snd dims)];
                dims
            | None ->
                debug_info := !debug_info @ ["node_json is None"];
                (0, 0)
          in
          let actual_pixels = w * h in
          if actual_pixels = 0 then (debug_info := !debug_info @ ["actual_pixels=0, returning empty"]; results)
          else if float_of_int actual_pixels <= max_pixels then
            let scale = auto_scale ~width:w ~height:h in
            let exported = export_node ~node_id ~scale in
            results @ exported
          else if split_children && depth < max_depth then
            let child_ids = match node_json with Some n -> get_child_ids n | None -> [] in
            if child_ids = [] then
              let scale = auto_scale ~width:w ~height:h in
              let exported = export_node ~node_id ~scale in
              results @ exported
            else
              List.fold_left (fun acc child_id ->
                export_recursive ~node_id:child_id ~depth:(depth + 1) acc
              ) results child_ids
          else
            let scale = auto_scale ~width:w ~height:h in
            let exported = export_node ~node_id ~scale in
            results @ exported
      | Error err ->
          debug_info := !debug_info @ [Printf.sprintf "get_nodes returned Error: %s" err];
          results
  in
  match (file_key, node_id, token) with
  | (Some _file_key, Some node_id, Some _token) ->
      let normalized = normalize_node_id node_id in
      debug_info := !debug_info @ [Printf.sprintf "Starting with node_id='%s', normalized='%s'" node_id normalized];
      let results = export_recursive ~node_id:normalized ~depth:0 [] in
      let base_fields = [
        ("total_exports", `Int (List.length results));
        ("max_pixels", `Float max_pixels);
        ("split_children", `Bool split_children);
        ("exports", `List results);
      ] in
      let summary = `Assoc (
        if include_debug then
          base_fields @ [("debug", `List (List.map (fun s -> `String s) !debug_info))]
        else
          base_fields
      ) in
      Ok (make_text_content (Yojson.Safe.pretty_to_string summary))
  | _ -> Error "Missing required parameters: file_key, node_id, token"

let handle_get_image_fills args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let version = get_string "version" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_images ~token ~file_key ?version () with
       | Ok json ->
           let images =
             match member "images" json with
             | Some (`Assoc _ as m) -> m
             | _ -> `Null
           in
           let downloads =
             if download then
               match images with
               | `Assoc items ->
                   `List (List.map (download_image_fill save_dir file_key) items)
               | _ -> `List []
             else
               `List []
           in
           let result = `Assoc [
             ("images", images);
             ("downloads", downloads);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"
