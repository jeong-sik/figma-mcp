(** Figma gRPC Server - Streaming API for large file handling

    Solves: "7MB JSON crashing Claude Code" problem
    Uses: grpc-direct (OCaml 5.x native gRPC library)

    Expected compression when gzip is enabled: 7MB JSON → ~700KB protobuf+gzip (90% reduction)
*)

open Printf

(** ============== Configuration ============== *)

let default_port = 50052  (* Separate from MCP server *)
let max_chunk_size = 64 * 1024  (* 64KB per chunk *)

(** ============== Protobuf Encoding/Decoding ============== *)

module Proto = Grpc_proto

(** ============== JSON to Streaming Converter ============== *)

module Streamer = struct
  (** Extract node ID from JSON *)
  let get_node_id json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "id" fields with
         | Some (`String id) -> id
         | _ -> "unknown")
    | _ -> "unknown"

  let get_node_name json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "name" fields with
         | Some (`String name) -> name
         | _ -> "")
    | _ -> ""

  (** Extract node type from JSON *)
  let get_node_type json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "type" fields with
         | Some (`String t) -> t
         | _ -> "UNKNOWN")
    | _ -> "UNKNOWN"

  let get_child_count json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "children" fields with
         | Some (`List kids) -> List.length kids
         | _ -> 0)
    | _ -> 0

  (** Count total nodes in tree *)
  let rec count_nodes json =
    match json with
    | `Assoc fields ->
        let children = match List.assoc_opt "children" fields with
          | Some (`List kids) -> kids
          | _ -> []
        in
        1 + List.fold_left (fun acc c -> acc + count_nodes c) 0 children
    | _ -> 0

  (** Stream nodes from JSON tree (breadth-first for progressive rendering) *)
  let stream_nodes ~format json stream =
    let total = count_nodes json in
    let index = ref 0 in

    (* BFS queue: (node_json, depth, parent_id) *)
    let queue = Queue.create () in
    Queue.add (json, 0, "") queue;

    while not (Queue.is_empty queue) do
      let (node, depth, parent_id) = Queue.pop queue in
      let node_id = get_node_id node in
      let node_name = get_node_name node in
      let child_count = get_child_count node in

      (* Convert node to DSL format *)
      let dsl = match format with
        | "raw" -> Yojson.Safe.to_string node
        | "fidelity" | "html" ->
            (* Use existing Fidelity DSL converter *)
            (match Mcp_tools.process_json_string ~format (Yojson.Safe.to_string node) with
             | Ok result -> result
             | Error _ -> Yojson.Safe.to_string node)
        | _ -> Yojson.Safe.to_string node
      in

      (* Encode and add to stream *)
      let proto_bytes = Proto.encode_figma_node
        ~node_id ~node_name ~depth ~parent_id ~child_count ~dsl
        ~node_index:!index ~total_nodes:total
      in
      Grpc_eio.Stream.add stream proto_bytes;
      incr index;

      (* Add children to queue *)
      (match node with
       | `Assoc fields ->
           (match List.assoc_opt "children" fields with
            | Some (`List kids) ->
                List.iter (fun kid ->
                  Queue.add (kid, depth + 1, node_id) queue
                ) kids
            | _ -> ())
       | _ -> ())
    done
end

(** ============== Split Stream: Style/Layout/Content 분리 ============== *)

module Splitter = struct
  (** JSON에서 float 추출 *)
  let get_float key fields =
    match List.assoc_opt key fields with
    | Some (`Float f) -> Some f
    | Some (`Int i) -> Some (float_of_int i)
    | _ -> None

  (** JSON에서 string 추출 *)
  let get_string key fields =
    match List.assoc_opt key fields with
    | Some (`String s) -> Some s
    | _ -> None

  (** JSON에서 bool 추출 *)
  let get_bool key fields =
    match List.assoc_opt key fields with
    | Some (`Bool b) -> Some b
    | _ -> None

  (** 색상 추출 (fills/strokes에서) *)
  let extract_colors key fields =
    match List.assoc_opt key fields with
    | Some (`List fills) ->
        List.filter_map (fun fill ->
          match fill with
          | `Assoc f ->
              (match List.assoc_opt "color" f with
               | Some (`Assoc c) ->
                   let r = get_float "r" c |> Option.value ~default:0.0 in
                   let g = get_float "g" c |> Option.value ~default:0.0 in
                   let b = get_float "b" c |> Option.value ~default:0.0 in
                   let a = get_float "a" c |> Option.value ~default:1.0 in
                   Some (Proto.{ r; g; b; a })
               | _ -> None)
          | _ -> None
        ) fills
    | _ -> []

  (** StyleChunk 생성 *)
  let extract_style node_id fields =
    let fill_colors = extract_colors "fills" fields in
    let stroke_colors = extract_colors "strokes" fields in
    let (font_family, font_size, font_weight, line_height, letter_spacing) =
      match List.assoc_opt "style" fields with
      | Some (`Assoc style) ->
          (get_string "fontFamily" style,
           get_float "fontSize" style,
           get_float "fontWeight" style |> Option.map int_of_float,
           get_float "lineHeightPx" style,
           get_float "letterSpacing" style)
      | _ -> (None, None, None, None, None)
    in
    {
      Proto.id = node_id ^ "-style";
      node_id;
      fill_colors;
      stroke_colors;
      font_family;
      font_size;
      font_weight;
      line_height;
      letter_spacing;
      opacity = get_float "opacity" fields;
      corner_radius = get_float "cornerRadius" fields;
      stroke_weight = get_float "strokeWeight" fields;
    }

  (** LayoutChunk 생성 *)
  let extract_layout node_id parent_id fields =
    let (x, y, width, height) =
      match List.assoc_opt "absoluteBoundingBox" fields with
      | Some (`Assoc bbox) ->
          (get_float "x" bbox,
           get_float "y" bbox,
           get_float "width" bbox,
           get_float "height" bbox)
      | _ -> (None, None, None, None)
    in
    let (horizontal_constraint, vertical_constraint) =
      match List.assoc_opt "constraints" fields with
      | Some (`Assoc c) ->
          (get_string "horizontal" c, get_string "vertical" c)
      | _ -> (None, None)
    in
    {
      Proto.id = node_id ^ "-layout";
      node_id;
      parent_id = if parent_id = "" then None else Some parent_id;
      x;
      y;
      width;
      height;
      horizontal_constraint;
      vertical_constraint;
      layout_mode = get_string "layoutMode" fields;
      primary_axis_align = get_string "primaryAxisAlignItems" fields;
      counter_axis_align = get_string "counterAxisAlignItems" fields;
      item_spacing = get_float "itemSpacing" fields;
      padding_left = get_float "paddingLeft" fields;
      padding_right = get_float "paddingRight" fields;
      padding_top = get_float "paddingTop" fields;
      padding_bottom = get_float "paddingBottom" fields;
      clips_content = get_bool "clipsContent" fields;
    }

  (** ContentChunk 생성 *)
  let extract_content node_id node_type name fields =
    let image_ref =
      match List.assoc_opt "fills" fields with
      | Some (`List fills) ->
          let refs =
            List.filter_map (fun fill ->
              match fill with
              | `Assoc f -> get_string "imageRef" f
              | _ -> None
            ) fills
          in
          (match refs with
           | x :: _ -> Some x
           | [] -> None)
      | _ -> None
    in
    {
      Proto.id = node_id ^ "-content";
      node_id;
      node_type;
      name;
      text_content = get_string "characters" fields;
      image_ref;
    }

  (** 노드를 Style/Layout/Content로 분리하여 스트림에 추가 *)
  let stream_split_node ~seq ~total ~node_id ~parent_id ~node_type ~name ~fields
      ~include_styles ~include_layouts ~include_contents stream =
    if include_styles then begin
      let style_chunk = extract_style node_id fields in
      let payload = Proto.encode_split_chunk
        ~sequence:!seq ~total_chunks:total ~node_id
        ~chunk:(Proto.Style style_chunk)
      in
      Grpc_eio.Stream.add stream payload;
      incr seq;
    end;

    if include_layouts then begin
      let layout_chunk = extract_layout node_id parent_id fields in
      let payload = Proto.encode_split_chunk
        ~sequence:!seq ~total_chunks:total ~node_id
        ~chunk:(Proto.Layout layout_chunk)
      in
      Grpc_eio.Stream.add stream payload;
      incr seq;
    end;

    if include_contents then begin
      let content_chunk = extract_content node_id node_type name fields in
      let payload = Proto.encode_split_chunk
        ~sequence:!seq ~total_chunks:total ~node_id
        ~chunk:(Proto.Content content_chunk)
      in
      Grpc_eio.Stream.add stream payload;
      incr seq;
    end

  (** 전체 트리를 Split Stream으로 변환 (BFS) *)
  let stream_split json stream ~include_styles ~include_layouts ~include_contents =
    let total_nodes = Streamer.count_nodes json in
    let per_node =
      (if include_styles then 1 else 0)
      + (if include_layouts then 1 else 0)
      + (if include_contents then 1 else 0)
    in
    let total_chunks = max 1 (total_nodes * per_node) in
    let seq = ref 0 in

    (* BFS *)
    let queue = Queue.create () in
    Queue.add (json, "") queue;

    while not (Queue.is_empty queue) do
      let (node, parent_id) = Queue.pop queue in
      match node with
      | `Assoc fields ->
          let node_id = Streamer.get_node_id node in
          let node_type = Streamer.get_node_type node in
          let name = match get_string "name" fields with
            | Some n -> n | None -> "" in

          stream_split_node ~seq ~total:total_chunks ~node_id ~parent_id
            ~node_type ~name ~fields ~include_styles ~include_layouts
            ~include_contents stream;

          (* Children *)
          (match List.assoc_opt "children" fields with
           | Some (`List kids) ->
               List.iter (fun kid -> Queue.add (kid, node_id) queue) kids
           | _ -> ())
      | _ -> ()
    done
end

(** ============== gRPC Service Handlers ============== *)

module Handlers = struct
  let normalize_node_id id = Figma_api.normalize_node_id id
  let normalize_node_id_opt value = Option.map normalize_node_id value

  type node_request = {
    file_key: string option;
    node_id: string option;
    token: string option;
    format: string;
    depth: int option;
    depth_start: int option;
    depth_end: int option;
    geometry: string option;
    plugin_data: string option;
    version: string option;
    recursive: bool;
    recursive_max_depth: int option;
    recursive_max_nodes: int option;
    recursive_depth_per_call: int option;
  }

  let env_int name default =
    match Sys.getenv_opt name with
    | Some raw -> (match int_of_string_opt raw with Some v -> v | None -> default)
    | None -> default

  let close_stream stream =
    Grpc_eio.Stream.close stream;
    stream

  let extract_node_document node_id json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "nodes" fields with
         | Some (`Assoc nodes) ->
             (match List.assoc_opt node_id nodes with
              | Some (`Assoc node_fields) ->
                  (match List.assoc_opt "document" node_fields with
                   | Some doc -> doc
                   | None -> json)
              | _ -> json)
         | _ -> json)
    | _ -> json

  let strip_children json =
    match json with
    | `Assoc fields ->
        `Assoc (List.filter (fun (key, _) -> key <> "children") fields)
    | _ -> json

  let child_ids json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "children" fields with
         | Some (`List kids) ->
             kids
             |> List.filter_map (function
                  | `Assoc child_fields ->
                      (match List.assoc_opt "id" child_fields with
                       | Some (`String id) -> Some (normalize_node_id id)
                       | _ -> None)
                  | _ -> None)
         | _ -> [])
    | _ -> []

  type requirements_stats = {
    total_nodes: int;
    parsed_nodes: int;
    skipped_nodes: int;
    frames: int;
    texts: int;
    vectors: int;
    rectangles: int;
    components: int;
    instances: int;
    auto_layout: int;
    image_fills: int;
  }

  let empty_requirements = {
    total_nodes = 0;
    parsed_nodes = 0;
    skipped_nodes = 0;
    frames = 0;
    texts = 0;
    vectors = 0;
    rectangles = 0;
    components = 0;
    instances = 0;
    auto_layout = 0;
    image_fills = 0;
  }

  let is_frame_node = function
    | Figma_types.Frame
    | Figma_types.Group
    | Figma_types.Section -> true
    | _ -> false

  let is_vector_node = function
    | Figma_types.Vector
    | Figma_types.Line
    | Figma_types.Star
    | Figma_types.Ellipse
    | Figma_types.RegularPolygon -> true
    | _ -> false

  let is_component_node = function
    | Figma_types.Component
    | Figma_types.ComponentSet -> true
    | _ -> false

  let has_image_fill (node : Figma_types.ui_node) =
    List.exists
      (fun paint -> paint.Figma_types.paint_type = Figma_types.Image)
      node.Figma_types.fills

  let add_node_stats stats (node : Figma_types.ui_node) =
    let inc cond value = if cond then value + 1 else value in
    {
      total_nodes = stats.total_nodes + 1;
      parsed_nodes = stats.parsed_nodes + 1;
      skipped_nodes = stats.skipped_nodes;
      frames = inc (is_frame_node node.Figma_types.node_type) stats.frames;
      texts = inc (node.Figma_types.node_type = Figma_types.Text) stats.texts;
      vectors = inc (is_vector_node node.Figma_types.node_type) stats.vectors;
      rectangles = inc (node.Figma_types.node_type = Figma_types.Rectangle) stats.rectangles;
      components = inc (is_component_node node.Figma_types.node_type) stats.components;
      instances = inc (node.Figma_types.node_type = Figma_types.Instance) stats.instances;
      auto_layout = inc (node.Figma_types.layout_mode <> Figma_types.None') stats.auto_layout;
      image_fills = inc (has_image_fill node) stats.image_fills;
    }

  let add_skipped stats =
    { stats with
      total_nodes = stats.total_nodes + 1;
      skipped_nodes = stats.skipped_nodes + 1;
    }

  let requirements_json stats top_level =
    let top_level =
      top_level
      |> List.rev
      |> List.filter (fun s -> s <> "")
      |> List.fold_left (fun acc name ->
           if List.mem name acc then acc else name :: acc) []
      |> List.rev
    in
    let json = `Assoc [
      ("total_nodes", `Int stats.total_nodes);
      ("parsed_nodes", `Int stats.parsed_nodes);
      ("skipped_nodes", `Int stats.skipped_nodes);
      ("frames", `Int stats.frames);
      ("texts", `Int stats.texts);
      ("vectors", `Int stats.vectors);
      ("rectangles", `Int stats.rectangles);
      ("components", `Int stats.components);
      ("instances", `Int stats.instances);
      ("auto_layout", `Int stats.auto_layout);
      ("image_fills", `Int stats.image_fills);
      ("top_level", `List (List.map (fun name -> `String name) top_level));
    ] in
    Yojson.Safe.to_string json

  let rec flatten_ui_node ?(parent_id=None) ?(depth=0) (node : Figma_types.ui_node) =
    let current : Task_planner.flat_node = {
      Task_planner.node = node;
      parent_node_id = parent_id;
      depth;
    } in
    let children =
      node.Figma_types.children
      |> List.map (flatten_ui_node ~parent_id:(Some node.Figma_types.id) ~depth:(depth + 1))
      |> List.flatten
    in
    current :: children

  let requirements_from_flat_nodes flat_nodes =
    let stats = ref empty_requirements in
    let top_level = ref [] in
    List.iter (fun (flat : Task_planner.flat_node) ->
      stats := add_node_stats !stats flat.Task_planner.node;
      if flat.Task_planner.depth = 1 && List.length !top_level < 30 then
        top_level := flat.Task_planner.node.Figma_types.name :: !top_level
    ) flat_nodes;
    requirements_json !stats !top_level

  (** Parse GetNodeRequest from bytes *)
  let parse_node_request bytes =
    let req = Proto.decode_get_node_request bytes in
    let format = Option.value req.format ~default:"fidelity" in
    let depth =
      match req.depth_end with
      | Some d when d >= 0 -> Some d
      | _ -> None
    in
    let geometry = if req.geometry then Some "paths" else None in
    let plugin_data = if req.plugin_data then Some "shared" else None in
    let node_id = normalize_node_id_opt req.node_id in
    {
      file_key = req.file_key;
      node_id;
      token = req.token;
      format;
      depth;
      depth_start = req.depth_start;
      depth_end = req.depth_end;
      geometry;
      plugin_data;
      version = req.version;
      recursive = req.recursive;
      recursive_max_depth = req.recursive_max_depth;
      recursive_max_nodes = req.recursive_max_nodes;
      recursive_depth_per_call = req.recursive_depth_per_call;
    }

  (** GetNodeStream: Stream nodes progressively *)
  let get_node_stream request_bytes =
    let stream = Grpc_eio.Stream.create 64 in

    let req = parse_node_request request_bytes in

    match (req.file_key, req.node_id, req.token) with
    | (Some file_key, Some node_id, Some token) ->
        if req.recursive then begin
          let max_depth =
            match req.recursive_max_depth with
            | Some d when d > 0 -> d
            | _ -> env_int "FIGMA_RECURSIVE_MAX_DEPTH" 20
          in
          let max_nodes =
            match req.recursive_max_nodes with
            | Some d when d > 0 -> d
            | _ -> env_int "FIGMA_RECURSIVE_MAX_NODES" 5000
          in
          let depth_per_call =
            match req.recursive_depth_per_call with
            | Some d when d > 0 -> d
            | _ -> env_int "FIGMA_RECURSIVE_DEPTH_PER_CALL" 1
          in
          let cache_options =
            List.filter_map Fun.id [
              Some (Printf.sprintf "depth:%d" depth_per_call);
              Option.map (Printf.sprintf "geometry:%s") req.geometry;
              Option.map (Printf.sprintf "plugin:%s") req.plugin_data;
            ]
          in
          eprintf "[gRPC] GetNodeStream (recursive) file_key=%s node_id=%s depth=%d max_nodes=%d token_len=%d\n%!"
            file_key node_id max_depth max_nodes (String.length token);
          let visited : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
          let queue : (string * string * int) Queue.t = Queue.create () in
          Queue.add (node_id, "", 0) queue;
          let node_index = ref 0 in
          let total_nodes = if max_nodes > 0 then max_nodes else 0 in
          while not (Queue.is_empty queue) &&
                (max_nodes <= 0 || !node_index < max_nodes) do
            let (current_id, parent_id, depth) = Queue.pop queue in
            if not (Hashtbl.mem visited current_id) then begin
              Hashtbl.add visited current_id ();
              let fetched =
                match Figma_cache.get ~file_key ~node_id:current_id ~options:cache_options () with
                | Some json -> Ok json
                | None ->
                    let result =
                      Lwt_main.run
                        (Figma_api.get_file_nodes
                           ~depth:depth_per_call ?geometry:req.geometry ?plugin_data:req.plugin_data ?version:req.version
                           ~token ~file_key ~node_ids:[current_id] ())
                    in
                    (match result with
                     | Ok json ->
                         Figma_cache.set ~file_key ~node_id:current_id ~options:cache_options json;
                         Ok json
                     | Error err -> Error err)
              in
              (match fetched with
               | Ok json ->
                   let node_json = extract_node_document current_id json in
                   let children = child_ids node_json in
                   let child_count = List.length children in
                   let node_name = Streamer.get_node_name node_json in
                   let dsl_source = strip_children node_json in
                   let dsl = match req.format with
                     | "raw" -> Yojson.Safe.to_string dsl_source
                     | "fidelity" | "html" ->
                         (match Mcp_tools.process_json_string ~format:req.format (Yojson.Safe.to_string dsl_source) with
                          | Ok result -> result
                          | Error _ -> Yojson.Safe.to_string dsl_source)
                     | _ -> Yojson.Safe.to_string dsl_source
                   in
                   let payload =
                     Proto.encode_figma_node
                       ~node_id:current_id ~node_name ~depth ~parent_id
                       ~child_count ~dsl
                       ~node_index:!node_index ~total_nodes
                   in
                   Grpc_eio.Stream.add stream payload;
                   incr node_index;
                   if depth < max_depth then
                     List.iter (fun child_id ->
                       if not (Hashtbl.mem visited child_id) then
                         Queue.add (child_id, current_id, depth + 1) queue
                     ) children
               | Error err ->
                   let error_msg = Figma_api.error_to_string err in
                   let payload =
                     Proto.encode_figma_node
                       ~node_id:current_id ~node_name:"" ~depth ~parent_id
                       ~child_count:0 ~dsl:("{\"error\":\"" ^ error_msg ^ "\"}")
                       ~node_index:!node_index ~total_nodes
                  in
                  Grpc_eio.Stream.add stream payload;
                  incr node_index)
            end
          done;
          close_stream stream
        end else begin
          eprintf "[gRPC] GetNodeStream file_key=%s node_id=%s token_len=%d\n%!"
            file_key node_id (String.length token);
          (* Fetch from Figma API using Lwt bridge *)
          let result =
            Lwt_main.run
              (Figma_api.get_file_nodes
                 ?depth:req.depth ?geometry:req.geometry ?plugin_data:req.plugin_data ?version:req.version
                 ~token ~file_key ~node_ids:[node_id] ())
          in
          (match result with
           | Ok json ->
               (* Extract the specific node from response *)
               let node_json = extract_node_document node_id json in
               Streamer.stream_nodes ~format:req.format node_json stream
           | Error err ->
               let error_msg = Figma_api.error_to_string err in
               let payload =
                 Proto.encode_figma_node
                   ~node_id ~node_name:"" ~depth:0 ~parent_id:""
                   ~child_count:0 ~dsl:("{\"error\":\"" ^ error_msg ^ "\"}")
                   ~node_index:0 ~total_nodes:1
               in
               Grpc_eio.Stream.add stream payload);
          close_stream stream
        end
    | _ ->
        let payload =
          Proto.encode_figma_node
            ~node_id:"" ~node_name:"" ~depth:0 ~parent_id:""
            ~child_count:0 ~dsl:"{\"error\":\"Missing required parameters\"}"
            ~node_index:0 ~total_nodes:1
        in
        Grpc_eio.Stream.add stream payload;
        close_stream stream

  (** GetFileMeta: Unary call for file metadata *)
  let get_file_meta request_bytes =
    let req = Proto.decode_file_meta_request request_bytes in
    let file_key_opt = req.file_key in
    let token_opt = req.token in
    let version = req.version in

    match (file_key_opt, token_opt) with
    | (Some file_key, Some token) ->
        eprintf "[gRPC] GetFileMeta file_key=%s token_len=%d\n%!"
          file_key (String.length token);
        let result =
          Lwt_main.run
            (Figma_api.get_file_meta ~token ~file_key ?version ())
        in
        (match result with
         | Ok json ->
             let open Yojson.Safe.Util in
             let file_meta = json |> member "file" in
             let name = file_meta |> member "name" |> to_string_option in
             let last_modified = file_meta |> member "last_touched_at" |> to_string_option in
             let thumbnail_url = file_meta |> member "thumbnail_url" |> to_string_option in
             let version = file_meta |> member "version" |> to_string_option in
             let role = file_meta |> member "role" |> to_string_option in
             let component_count = None in
             let style_count = None in
             Proto.encode_file_meta_response
               ?name ?last_modified ?thumbnail_url ?version ?role
               ?component_count ?style_count ()
         | Error err ->
             Proto.encode_file_meta_response
               ~name:(Printf.sprintf "ERROR: %s" (Figma_api.error_to_string err)) ())
    | _ ->
        Proto.encode_file_meta_response ~name:"ERROR: Missing required parameters" ()

  (** FidelityLoop: Stream progressive depth increases *)
  let fidelity_loop request_bytes =
    let stream = Grpc_eio.Stream.create 16 in
    let req = Proto.decode_fidelity_loop_request request_bytes in

    match (req.file_key, req.node_id, req.token) with
    | (Some fk, Some nid, Some tok) ->
        let node_id = normalize_node_id nid in
        let attempt = ref 0 in
        let current_depth = ref req.start_depth in
        let done_flag = ref false in

        while not !done_flag && !current_depth <= req.max_depth do
          incr attempt;

          let result =
            Lwt_main.run
              (Figma_api.get_file_nodes ~depth:!current_depth ~token:tok ~file_key:fk ~node_ids:[node_id] ())
          in

          (match result with
           | Ok json ->
               let dsl = Yojson.Safe.to_string json in
               let raw_size = String.length dsl in
               let compressed = Proto.compress dsl in
               let compressed_size = String.length compressed in
               let score = Figma_early_stop.calculate_text_density json in

               let done_now = score >= req.target_score in
               let progress =
                 Proto.encode_fidelity_progress {
                   Proto.attempt = !attempt;
                   current_depth = !current_depth;
                   current_score = score;
                   dsl = Some compressed;
                   done_ = done_now;
                   success = done_now;
                   final_dsl = (if done_now then Some compressed else None);
                   error = None;
                   node_count = None;
                   raw_size = Some raw_size;
                   compressed_size = Some compressed_size;
                 }
               in
               Grpc_eio.Stream.add stream progress;

               if done_now then done_flag := true
               else current_depth := !current_depth + req.depth_step
           | Error err ->
               let error_msg = Figma_api.error_to_string err in
               let progress =
                 Proto.encode_fidelity_progress {
                   Proto.attempt = !attempt;
                   current_depth = !current_depth;
                   current_score = 0.0;
                   dsl = None;
                   done_ = true;
                   success = false;
                   final_dsl = None;
                   error = Some error_msg;
                   node_count = None;
                   raw_size = None;
                   compressed_size = None;
                 }
               in
               Grpc_eio.Stream.add stream progress;
               done_flag := true)
        done;

        if not !done_flag then begin
          let progress =
            Proto.encode_fidelity_progress {
              Proto.attempt = !attempt;
              current_depth = !current_depth;
              current_score = 0.0;
              dsl = None;
              done_ = true;
              success = false;
              final_dsl = None;
              error = Some "Max depth reached";
              node_count = None;
              raw_size = None;
              compressed_size = None;
            }
          in
          Grpc_eio.Stream.add stream progress
        end;
        close_stream stream
    | _ ->
        let progress =
          Proto.encode_fidelity_progress {
            Proto.attempt = 0;
            current_depth = 0;
            current_score = 0.0;
            dsl = None;
            done_ = true;
            success = false;
            final_dsl = None;
            error = Some "Missing required parameters";
            node_count = None;
            raw_size = None;
            compressed_size = None;
        }
        in
        Grpc_eio.Stream.add stream progress;
        close_stream stream

  (** GetSplitStream: Stream nodes split into Style/Layout/Content chunks *)
  let get_split_stream request_bytes =
    let req = Proto.decode_split_stream_request request_bytes in

    match (req.file_key, req.node_id, req.token) with
    | (Some file_key, Some node_id, Some token) ->
        let node_id = normalize_node_id node_id in
        let cache_options =
          match req.depth with
          | Some depth -> [Printf.sprintf "depth:%d" depth]
          | None -> []
        in
        let cached_json =
          match Figma_cache.get ~file_key ~node_id ~options:cache_options () with
          | Some json ->
              eprintf "[gRPC] SplitStream cache HIT (exact) node=%s\n%!" node_id;
              Some json
          | None ->
              let fallback_options =
                let depth_fallback =
                  match req.depth with
                  | Some depth when depth > 2 ->
                      [([Printf.sprintf "depth:%d" 2], "depth:2")]
                  | _ -> []
                in
                let no_depth =
                  if cache_options <> [] then [([], "no-depth")] else []
                in
                depth_fallback @ no_depth
              in
              let rec find_fallback = function
                | [] -> None
                | (options, label) :: rest ->
                    (match Figma_cache.get ~file_key ~node_id ~options () with
                     | Some json ->
                         eprintf "[gRPC] SplitStream cache HIT (%s) node=%s\n%!"
                           label node_id;
                         Some json
                     | None -> find_fallback rest)
              in
              find_fallback fallback_options
        in
        let result =
          match cached_json with
          | Some json -> Ok json
          | None ->
              (* Fetch from Figma API using Lwt bridge *)
              let fetched =
                Lwt_main.run
                  (Figma_api.get_file_nodes
                     ?depth:req.depth
                     ~token ~file_key ~node_ids:[node_id] ())
              in
              (match fetched with
               | Ok json ->
                   Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                   Ok json
               | Error err -> Error err)
        in
        (match result with
         | Ok json ->
             (* Extract the specific node from response *)
             let node_json = match json with
               | `Assoc fields ->
                   (match List.assoc_opt "nodes" fields with
                    | Some (`Assoc nodes) ->
                        (match List.assoc_opt node_id nodes with
                         | Some (`Assoc node_fields) ->
                             (match List.assoc_opt "document" node_fields with
                              | Some doc -> doc
                              | None -> json)
                         | _ -> json)
                    | _ -> json)
               | _ -> json
             in
             let total_nodes = Streamer.count_nodes node_json in
            let per_node =
              (if req.include_styles then 1 else 0)
              + (if req.include_layouts then 1 else 0)
              + (if req.include_contents then 1 else 0)
            in
            if per_node = 0 then
              let stream = Grpc_eio.Stream.create 1 in
              let payload =
                Proto.encode_split_chunk ~sequence:0 ~total_chunks:1 ~node_id
                  ~chunk:(Proto.Content Proto.{
                    id = "error-content";
                    node_id;
                    node_type = "ERROR";
                    name = "error";
                    text_content = Some "SplitStream requires at least one include flag";
                    image_ref = None;
                  })
              in
              Grpc_eio.Stream.add stream payload;
              close_stream stream
            else
              let total_chunks = max 1 (total_nodes * per_node) in
              let stream = Grpc_eio.Stream.create total_chunks in
              Splitter.stream_split node_json stream
                ~include_styles:req.include_styles
                ~include_layouts:req.include_layouts
                ~include_contents:req.include_contents;
              close_stream stream
         | Error err ->
             let error_msg = Figma_api.error_to_string err in
             let stream = Grpc_eio.Stream.create 1 in
             let payload =
               Proto.encode_split_chunk ~sequence:0 ~total_chunks:1 ~node_id
                 ~chunk:(Proto.Content Proto.{
                   id = "error-content";
                   node_id;
                   node_type = "ERROR";
                   name = "error";
                   text_content = Some error_msg;
                   image_ref = None;
                 })
             in
             Grpc_eio.Stream.add stream payload;
             close_stream stream)
    | _ ->
        let stream = Grpc_eio.Stream.create 1 in
        let payload =
          Proto.encode_split_chunk ~sequence:0 ~total_chunks:1 ~node_id:""
            ~chunk:(Proto.Content Proto.{
              id = "error-content";
              node_id = "";
              node_type = "ERROR";
              name = "error";
              text_content = Some "Missing required parameters";
              image_ref = None;
            })
        in
        Grpc_eio.Stream.add stream payload;
        close_stream stream

  (** PlanTasks: Generate ROI-based implementation tasks from Figma node

      Hybrid Architecture (gRPC + Agent):
      - gRPC side (this handler): Fast parsing, ROI tier classification, Task skeleton generation
      - Agent side: Context-aware dependency analysis, TodoWrite/MASC generation

      ROI Tiers (UIFormer research-based):
      - P1_Layout: row/col, size, gap, padding - 80% SSIM contribution
      - P2_Style: bg, radius, shadow, border - +10% SSIM
      - P3_Text: font-size, color, weight - +5% SSIM
      - P4_Specialist: vectors, animations, components
  *)
  let plan_tasks request_bytes =
    let req = Proto.decode_plan_tasks_request request_bytes in
    let limit_tasks tasks =
      match req.max_tasks with
      | Some limit when limit > 0 ->
          let rec take n lst =
            match (n, lst) with
            | (n, _) when n <= 0 -> []
            | (_, []) -> []
            | (n, x :: xs) -> x :: take (n - 1) xs
          in
          take limit tasks
      | _ -> tasks
    in
    let to_proto_tasks tasks =
      List.map (fun (t : Task_planner.task) ->
        let priority =
          match t.Task_planner.priority with
          | Task_planner.P1_Layout -> 0
          | Task_planner.P2_Style -> 1
          | Task_planner.P3_Text -> 2
          | Task_planner.P4_Specialist -> 3
        in
        Proto.{
          id = t.id;
          node_id = t.node_id;
          node_name = t.node_name;
          node_type = t.node_type;
          priority;
          dependencies = t.dependencies;
          estimated_tokens = t.estimated_tokens;
          semantic_dsl = t.semantic_dsl;
          hints = t.hints;
        }
      ) tasks
    in
    match (req.file_key, req.node_id, req.token) with
    | (Some file_key, Some node_id, Some token) ->
        let node_id = normalize_node_id node_id in
        if req.recursive then begin
          let max_depth =
            match req.recursive_max_depth with
            | Some d when d > 0 -> d
            | _ -> env_int "FIGMA_RECURSIVE_MAX_DEPTH" 20
          in
          let max_nodes =
            match req.recursive_max_nodes with
            | Some d when d > 0 -> d
            | _ -> env_int "FIGMA_RECURSIVE_MAX_NODES" 5000
          in
          let depth_per_call =
            match req.recursive_depth_per_call with
            | Some d when d > 0 -> d
            | _ -> env_int "FIGMA_RECURSIVE_DEPTH_PER_CALL" 1
          in
          let cache_options =
            List.filter_map Fun.id [
              Some (Printf.sprintf "depth:%d" depth_per_call);
            ]
          in
          let visited : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
          let queue : (string * string option * int) Queue.t = Queue.create () in
          Queue.add (node_id, None, 0) queue;
          let flat_nodes = ref [] in
          let stats = ref empty_requirements in
          let top_level = ref [] in
          let processed = ref 0 in

          while not (Queue.is_empty queue)
                && (max_nodes <= 0 || !processed < max_nodes) do
            let (current_id, parent_id, depth) = Queue.pop queue in
            if not (Hashtbl.mem visited current_id) then begin
              Hashtbl.add visited current_id ();
              incr processed;

              let fetched =
                match Figma_cache.get ~file_key ~node_id:current_id ~options:cache_options () with
                | Some json -> Ok json
                | None ->
                    let result =
                      Lwt_main.run
                        (Figma_api.get_file_nodes
                           ~depth:depth_per_call
                           ~token ~file_key ~node_ids:[current_id] ())
                    in
                    (match result with
                     | Ok json ->
                         Figma_cache.set ~file_key ~node_id:current_id ~options:cache_options json;
                         Ok json
                     | Error err -> Error err)
              in

              (match fetched with
               | Ok json ->
                   let node_json_full = extract_node_document current_id json in
                   let children = child_ids node_json_full in
                   let node_json = strip_children node_json_full in
                   (match Figma_parser.parse_json node_json with
                    | Some ui_node ->
                        let flat : Task_planner.flat_node = {
                          Task_planner.node = ui_node;
                          parent_node_id = parent_id;
                          depth;
                        } in
                        flat_nodes := flat :: !flat_nodes;
                        stats := add_node_stats !stats ui_node;
                        if depth = 1 && List.length !top_level < 30 then
                          top_level := ui_node.Figma_types.name :: !top_level
                    | None ->
                        stats := add_skipped !stats);
                   if depth < max_depth then
                     List.iter (fun child_id ->
                       if not (Hashtbl.mem visited child_id) then
                         Queue.add (child_id, Some current_id, depth + 1) queue
                     ) children
               | Error _ ->
                   stats := add_skipped !stats)
            end
          done;

          let flat_nodes = List.rev !flat_nodes in
          let tasks = Task_planner.tasks_from_flat flat_nodes in
          let sorted = Task_planner.sort_by_priority tasks |> limit_tasks in
          let total_tokens = List.fold_left
            (fun acc t -> acc + t.Task_planner.estimated_tokens) 0 sorted in
          let summary = Task_planner.summarize_plan sorted in
          let requirements_payload = requirements_json !stats !top_level in
          Proto.encode_plan_tasks_response
            ~summary:(Some summary)
            ~requirements_json:(Some requirements_payload)
            ~tasks:(to_proto_tasks sorted)
            ~total_estimated_tokens:total_tokens
            ~root_node_id:node_id
        end else begin
          let result =
            Lwt_main.run
              (Figma_api.get_file_nodes
                 ?depth:req.depth
                 ~token ~file_key ~node_ids:[node_id] ())
          in
          (match result with
           | Ok json ->
               (* Extract node document from API response *)
               let node_json = extract_node_document node_id json in
               (* Parse JSON to ui_node, then generate tasks *)
               (match Figma_parser.parse_json node_json with
                | Some ui_node ->
                    let tasks = Task_planner.collect_tasks ui_node in
                    let sorted = Task_planner.sort_by_priority tasks |> limit_tasks in
                    let total_tokens = List.fold_left
                      (fun acc t -> acc + t.Task_planner.estimated_tokens) 0 sorted in
                    let summary = Task_planner.summarize_plan sorted in
                    let flat_nodes = flatten_ui_node ui_node in
                    let requirements_payload = requirements_from_flat_nodes flat_nodes in
                    Proto.encode_plan_tasks_response
                      ~summary:(Some summary)
                      ~requirements_json:(Some requirements_payload)
                      ~tasks:(to_proto_tasks sorted)
                      ~total_estimated_tokens:total_tokens
                      ~root_node_id:node_id
                | None ->
                    Proto.encode_plan_tasks_response
                      ~summary:None
                      ~requirements_json:None
                      ~tasks:[] ~total_estimated_tokens:0 ~root_node_id:node_id)
           | Error err ->
               Proto.encode_plan_tasks_response
                 ~summary:None
                 ~requirements_json:None
                 ~tasks:[] ~total_estimated_tokens:0
                 ~root_node_id:(Printf.sprintf "ERROR: %s" (Figma_api.error_to_string err)))
        end
    | _ ->
        Proto.encode_plan_tasks_response
          ~summary:None
          ~requirements_json:None
          ~tasks:[] ~total_estimated_tokens:0
          ~root_node_id:"ERROR: Missing required parameters"
end

(** ============== gRPC Server Setup ============== *)

let create_service () =
  Grpc_eio.Service.create "figma.v1.FigmaService"
  |> Grpc_eio.Service.add_server_streaming "GetNodeStream" Handlers.get_node_stream
  |> Grpc_eio.Service.add_unary "GetFileMeta" Handlers.get_file_meta
  |> Grpc_eio.Service.add_server_streaming "FidelityLoop" Handlers.fidelity_loop
  |> Grpc_eio.Service.add_server_streaming "GetSplitStream" Handlers.get_split_stream
  |> Grpc_eio.Service.add_unary "PlanTasks" Handlers.plan_tasks

let create_server ~port () =
  let config = { Grpc_eio.Server.default_config with port } in
  Grpc_eio.Server.create ~config ()
  |> Grpc_eio.Server.add_service (create_service ())
  |> Grpc_eio.Server.with_interceptor (Grpc_eio.Interceptor.logging ())

(** Start the gRPC server *)
let serve ~sw ~env ?(port=default_port) () =
  let server = create_server ~port () in
  printf "🚀 Figma gRPC server starting on port %d...\n%!" port;
  printf "   Methods:\n%!";
  printf "   - GetNodeStream (Server Streaming) - Progressive node loading\n%!";
  printf "   - GetFileMeta (Unary) - File metadata only\n%!";
  printf "   - FidelityLoop (Server Streaming) - Auto depth iteration\n%!";
  printf "   - GetSplitStream (Server Streaming) - Style/Layout/Content split\n%!";
  printf "   - PlanTasks (Unary) - ROI-based implementation task planning\n%!";
  printf "\n%!";
  printf "   Test with:\n%!";
  printf "   grpcurl -plaintext -d '{\"file_key\":\"...\",\"node_id\":\"...\",\"token\":\"...\"}' \\\n%!";
  printf "     localhost:%d figma.v1.FigmaService/GetNodeStream\n%!" port;
  Grpc_eio.Server.serve ~sw ~env server

(** Run standalone server *)
let run_standalone ?(port=default_port) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  serve ~sw ~env ~port ()
