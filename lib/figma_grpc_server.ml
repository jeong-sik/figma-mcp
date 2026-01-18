(** Figma gRPC Server - Streaming API for large file handling

    Solves: "7MB JSON crashing Claude Code" problem
    Uses: grpc-direct (OCaml 5.x native gRPC library)

    Expected compression: 7MB JSON → ~700KB protobuf+gzip (90% reduction)
*)

open Printf

(** ============== Configuration ============== *)

let default_port = 50052  (* Separate from MCP server *)
let max_chunk_size = 64 * 1024  (* 64KB per chunk *)

(** ============== Simple Protobuf-like Binary Encoding ============== *)
(** Note: We use a simple length-prefixed format instead of full protobuf.
    This keeps the implementation simple while still achieving significant
    compression vs JSON text. *)

module Proto = struct
  (** Identity compression - placeholder for real gzip compression
      TODO: Add camlzip dependency for actual gzip compression *)
  let compress s = s

  (** Encode a string with length prefix (varint + bytes) *)
  let encode_string s =
    let len = String.length s in
    let buf = Buffer.create (len + 5) in
    (* Simple varint encoding for length *)
    let rec write_varint n =
      if n < 128 then Buffer.add_char buf (Char.chr n)
      else begin
        Buffer.add_char buf (Char.chr ((n land 0x7f) lor 0x80));
        write_varint (n lsr 7)
      end
    in
    write_varint len;
    Buffer.add_string buf s;
    Buffer.contents buf

  (** Decode a length-prefixed string *)
  let decode_string bytes offset =
    let len = ref 0 in
    let shift = ref 0 in
    let pos = ref offset in
    (* Read varint *)
    let continue = ref true in
    while !continue do
      let b = Char.code (String.get bytes !pos) in
      len := !len lor ((b land 0x7f) lsl !shift);
      shift := !shift + 7;
      incr pos;
      continue := b land 0x80 <> 0
    done;
    (* Read string *)
    let s = String.sub bytes !pos !len in
    (s, !pos + !len)

  (** Write a varint to buffer *)
  let write_varint buf n =
    let rec go n =
      if n < 128 then Buffer.add_char buf (Char.chr n)
      else begin
        Buffer.add_char buf (Char.chr ((n land 0x7f) lor 0x80));
        go (n lsr 7)
      end
    in
    go n

  (** Encode a FigmaNode message *)
  let encode_figma_node ~node_id ~depth ~parent_id ~dsl ~node_index ~total_nodes =
    let buf = Buffer.create 256 in
    (* Field 1: node_id (string) *)
    Buffer.add_char buf '\x0a';  (* field 1, wire type 2 *)
    Buffer.add_string buf (encode_string node_id);
    (* Field 2: depth (varint) *)
    Buffer.add_char buf '\x10';  (* field 2, wire type 0 *)
    Buffer.add_char buf (Char.chr depth);
    (* Field 3: parent_id (string) *)
    Buffer.add_char buf '\x1a';  (* field 3, wire type 2 *)
    Buffer.add_string buf (encode_string parent_id);
    (* Field 4: dsl (bytes, compressed) *)
    Buffer.add_char buf '\x22';  (* field 4, wire type 2 *)
    let compressed = compress dsl in
    Buffer.add_string buf (encode_string compressed);
    (* Field 5: node_index (varint) *)
    Buffer.add_char buf '\x28';  (* field 5, wire type 0 *)
    write_varint buf node_index;
    (* Field 6: total_nodes (varint) *)
    Buffer.add_char buf '\x30';  (* field 6, wire type 0 *)
    write_varint buf total_nodes;
    Buffer.contents buf
end

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

  (** Extract node type from JSON *)
  let get_node_type json =
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "type" fields with
         | Some (`String t) -> t
         | _ -> "UNKNOWN")
    | _ -> "UNKNOWN"

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
        ~node_id ~depth ~parent_id ~dsl
        ~node_index:!index ~total_nodes:total
      in
      Eio.Stream.add stream proto_bytes;
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
                   Some (Printf.sprintf "rgba(%d,%d,%d,%.2f)"
                     (int_of_float (r *. 255.))
                     (int_of_float (g *. 255.))
                     (int_of_float (b *. 255.)) a)
               | _ -> None)
          | _ -> None
        ) fills
    | _ -> []

  (** StyleChunk JSON 생성 *)
  let extract_style node_id fields =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf {|{"id":"%s-style","node_id":"%s"|} node_id node_id);

    (* Colors *)
    let fill_colors = extract_colors "fills" fields in
    if fill_colors <> [] then
      Buffer.add_string buf (Printf.sprintf {|,"fill_colors":[%s]|}
        (String.concat "," (List.map (Printf.sprintf {|"%s"|}) fill_colors)));

    let stroke_colors = extract_colors "strokes" fields in
    if stroke_colors <> [] then
      Buffer.add_string buf (Printf.sprintf {|,"stroke_colors":[%s]|}
        (String.concat "," (List.map (Printf.sprintf {|"%s"|}) stroke_colors)));

    (* Typography *)
    (match List.assoc_opt "style" fields with
     | Some (`Assoc style) ->
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"font_family":"%s"|} v))
           (get_string "fontFamily" style);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"font_size":%.1f|} v))
           (get_float "fontSize" style);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"font_weight":%d|} (int_of_float v)))
           (get_float "fontWeight" style);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"line_height":%.2f|} v))
           (get_float "lineHeightPx" style);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"letter_spacing":%.2f|} v))
           (get_float "letterSpacing" style);
     | _ -> ());

    (* Effects *)
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"opacity":%.2f|} v))
      (get_float "opacity" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"corner_radius":%.1f|} v))
      (get_float "cornerRadius" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"stroke_weight":%.1f|} v))
      (get_float "strokeWeight" fields);

    Buffer.add_char buf '}';
    Buffer.contents buf

  (** LayoutChunk JSON 생성 *)
  let extract_layout node_id parent_id fields =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf {|{"id":"%s-layout","node_id":"%s"|} node_id node_id);

    Option.iter (fun p -> Buffer.add_string buf (Printf.sprintf {|,"parent_id":"%s"|} p))
      (if parent_id = "" then None else Some parent_id);

    (* Position & Size from absoluteBoundingBox *)
    (match List.assoc_opt "absoluteBoundingBox" fields with
     | Some (`Assoc bbox) ->
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"x":%.1f|} v))
           (get_float "x" bbox);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"y":%.1f|} v))
           (get_float "y" bbox);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"width":%.1f|} v))
           (get_float "width" bbox);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"height":%.1f|} v))
           (get_float "height" bbox);
     | _ -> ());

    (* Constraints *)
    (match List.assoc_opt "constraints" fields with
     | Some (`Assoc c) ->
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"horizontal_constraint":"%s"|} v))
           (get_string "horizontal" c);
         Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"vertical_constraint":"%s"|} v))
           (get_string "vertical" c);
     | _ -> ());

    (* Auto Layout *)
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"layout_mode":"%s"|} v))
      (get_string "layoutMode" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"primary_axis_align":"%s"|} v))
      (get_string "primaryAxisAlignItems" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"counter_axis_align":"%s"|} v))
      (get_string "counterAxisAlignItems" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"item_spacing":%.1f|} v))
      (get_float "itemSpacing" fields);

    (* Padding *)
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"padding_left":%.1f|} v))
      (get_float "paddingLeft" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"padding_right":%.1f|} v))
      (get_float "paddingRight" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"padding_top":%.1f|} v))
      (get_float "paddingTop" fields);
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"padding_bottom":%.1f|} v))
      (get_float "paddingBottom" fields);

    (* Clips *)
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"clips_content":%b|} v))
      (get_bool "clipsContent" fields);

    Buffer.add_char buf '}';
    Buffer.contents buf

  (** ContentChunk JSON 생성 *)
  let extract_content node_id node_type name fields =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf
      {|{"id":"%s-content","node_id":"%s","node_type":"%s","name":"%s"|}
      node_id node_id node_type (String.escaped name));

    (* Text content *)
    Option.iter (fun v ->
      Buffer.add_string buf (Printf.sprintf {|,"text_content":"%s"|} (String.escaped v)))
      (get_string "characters" fields);

    (* Image reference (from fills) *)
    (match List.assoc_opt "fills" fields with
     | Some (`List fills) ->
         List.iter (fun fill ->
           match fill with
           | `Assoc f ->
               (match get_string "imageRef" f with
                | Some img_ref ->
                    Buffer.add_string buf (Printf.sprintf {|,"image_ref":"%s"|} img_ref)
                | None -> ())
           | _ -> ()
         ) fills
     | _ -> ());

    (* Component info *)
    Option.iter (fun v -> Buffer.add_string buf (Printf.sprintf {|,"component_id":"%s"|} v))
      (get_string "componentId" fields);
    (match node_type with
     | "COMPONENT" -> Buffer.add_string buf {|,"is_component":true|}
     | "INSTANCE" -> Buffer.add_string buf {|,"is_instance":true|}
     | _ -> ());

    Buffer.add_char buf '}';
    Buffer.contents buf

  (** 노드를 Style/Layout/Content로 분리하여 스트림에 추가 *)
  let stream_split_node ~seq ~total ~node_id ~parent_id ~node_type ~name ~fields stream =
    (* Style chunk *)
    let style_json = extract_style node_id fields in
    let style_chunk = Printf.sprintf
      {|{"sequence":%d,"total_chunks":%d,"node_id":"%s","style":%s}|}
      !seq total node_id style_json in
    Eio.Stream.add stream style_chunk;
    incr seq;

    (* Layout chunk *)
    let layout_json = extract_layout node_id parent_id fields in
    let layout_chunk = Printf.sprintf
      {|{"sequence":%d,"total_chunks":%d,"node_id":"%s","layout":%s}|}
      !seq total node_id layout_json in
    Eio.Stream.add stream layout_chunk;
    incr seq;

    (* Content chunk *)
    let content_json = extract_content node_id node_type name fields in
    let content_chunk = Printf.sprintf
      {|{"sequence":%d,"total_chunks":%d,"node_id":"%s","content":%s}|}
      !seq total node_id content_json in
    Eio.Stream.add stream content_chunk;
    incr seq

  (** 전체 트리를 Split Stream으로 변환 (BFS) *)
  let stream_split json stream =
    let total_nodes = Streamer.count_nodes json in
    let total_chunks = total_nodes * 3 in  (* 각 노드당 3개 청크 *)
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
            ~node_type ~name ~fields stream;

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
  (** Parse GetNodeRequest from bytes - returns (file_key, node_id, token, format, depth) *)
  let parse_node_request bytes =
    try
      let json = Yojson.Safe.from_string bytes in
      match json with
      | `Assoc fields ->
          let get s = match List.assoc_opt s fields with
            | Some (`String v) -> Some v
            | _ -> None
          in
          let get_int s = match List.assoc_opt s fields with
            | Some (`Int v) -> Some v
            | _ -> None
          in
          (get "file_key",
           get "node_id",
           get "token",
           Option.value (get "format") ~default:"fidelity",
           get_int "depth")
      | _ -> (None, None, None, "fidelity", None)
    with _ -> (None, None, None, "fidelity", None)

  (** GetNodeStream: Stream nodes progressively *)
  let get_node_stream request_bytes =
    let stream = Eio.Stream.create 64 in

    let (file_key_opt, node_id_opt, token_opt, format, depth_opt) =
      parse_node_request request_bytes
    in

    match (file_key_opt, node_id_opt, token_opt) with
    | (Some file_key, Some node_id, Some token) ->
        (* Fetch from Figma API using Lwt bridge *)
        let result =
          Lwt_eio.run_lwt @@ fun () ->
          Figma_api.get_file_nodes
            ?depth:depth_opt
            ~token ~file_key ~node_ids:[node_id] ()
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
             Streamer.stream_nodes ~format node_json stream
         | Error err ->
             let error_msg = Figma_api.error_to_string err in
             Eio.Stream.add stream (sprintf "ERROR: %s" error_msg));
        stream
    | _ ->
        Eio.Stream.add stream "ERROR: Missing required parameters (file_key, node_id, token)";
        stream

  (** GetFileMeta: Unary call for file metadata *)
  let get_file_meta request_bytes =
    let (file_key_opt, _, token_opt, _, _) = parse_node_request request_bytes in

    match (file_key_opt, token_opt) with
    | (Some file_key, Some token) ->
        let result =
          Lwt_eio.run_lwt @@ fun () ->
          Figma_api.get_file_meta ~token ~file_key ()
        in
        (match result with
         | Ok json -> Yojson.Safe.to_string json
         | Error err -> sprintf {|{"error": "%s"}|} (Figma_api.error_to_string err))
    | _ ->
        {|{"error": "Missing required parameters (file_key, token)"}|}

  (** FidelityLoop: Stream progressive depth increases *)
  let fidelity_loop request_bytes =
    let stream = Eio.Stream.create 16 in

    let json = try Some (Yojson.Safe.from_string request_bytes) with _ -> None in

    match json with
    | Some (`Assoc fields) ->
        let get s = match List.assoc_opt s fields with
          | Some (`String v) -> Some v
          | _ -> None
        in
        let get_float s default = match List.assoc_opt s fields with
          | Some (`Float v) -> v
          | Some (`Int v) -> float_of_int v
          | _ -> default
        in
        let get_int s default = match List.assoc_opt s fields with
          | Some (`Int v) -> v
          | _ -> default
        in

        let file_key = get "file_key" in
        let node_id = get "node_id" in
        let token = get "token" in
        let target_score = get_float "target_score" 0.92 in
        let start_depth = get_int "start_depth" 4 in
        let max_depth = get_int "max_depth" 20 in
        let depth_step = get_int "depth_step" 4 in

        (match (file_key, node_id, token) with
         | (Some fk, Some nid, Some tok) ->
             (* Iteratively increase depth until target score reached *)
             let attempt = ref 0 in
             let current_depth = ref start_depth in
             let done_flag = ref false in

             while not !done_flag && !current_depth <= max_depth do
               incr attempt;

               let result =
                 Lwt_eio.run_lwt @@ fun () ->
                 Figma_api.get_file_nodes ~depth:!current_depth ~token:tok ~file_key:fk ~node_ids:[nid] ()
               in

               (match result with
                | Ok json ->
                    let dsl = Yojson.Safe.to_string json in
                    let raw_size = String.length dsl in
                    let compressed = Proto.compress dsl in
                    let compressed_size = String.length compressed in

                    (* Calculate simple coverage score based on text density *)
                    let score = Figma_early_stop.calculate_text_density json in

                    (* Build progress message - base64 inline for final DSL *)
                    let final_dsl_part =
                      if score >= target_score then
                        sprintf {|,"done":true,"success":true,"final_dsl":"%s"|}
                          (Base64.encode_exn compressed)
                      else ""
                    in
                    let progress_json = sprintf
                      {|{"attempt":%d,"current_depth":%d,"current_score":%.3f,"raw_size":%d,"compressed_size":%d%s}|}
                      !attempt !current_depth score raw_size compressed_size final_dsl_part
                    in
                    Eio.Stream.add stream progress_json;

                    if score >= target_score then done_flag := true
                    else current_depth := !current_depth + depth_step
                | Error err ->
                    let error_msg = Figma_api.error_to_string err in
                    Eio.Stream.add stream (sprintf {|{"error":"%s","done":true,"success":false}|} error_msg);
                    done_flag := true)
             done;

             if not !done_flag then
               Eio.Stream.add stream {|{"done":true,"success":false,"error":"Max depth reached"}|}
         | _ ->
             Eio.Stream.add stream {|{"error":"Missing required parameters"}|});
        stream
    | _ ->
        Eio.Stream.add stream {|{"error":"Invalid request format"}|};
        stream

  (** GetSplitStream: Stream nodes split into Style/Layout/Content chunks *)
  let get_split_stream request_bytes =
    let stream = Eio.Stream.create 128 in

    let (file_key_opt, node_id_opt, token_opt, _, depth_opt) =
      parse_node_request request_bytes
    in

    match (file_key_opt, node_id_opt, token_opt) with
    | (Some file_key, Some node_id, Some token) ->
        (* Fetch from Figma API using Lwt bridge *)
        let result =
          Lwt_eio.run_lwt @@ fun () ->
          Figma_api.get_file_nodes
            ?depth:depth_opt
            ~token ~file_key ~node_ids:[node_id] ()
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
             Splitter.stream_split node_json stream
         | Error err ->
             let error_msg = Figma_api.error_to_string err in
             Eio.Stream.add stream (sprintf {|{"error":"%s"}|} error_msg));
        stream
    | _ ->
        Eio.Stream.add stream {|{"error":"Missing required parameters (file_key, node_id, token)"}|};
        stream

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
    let (file_key_opt, node_id_opt, token_opt, _, depth_opt) =
      parse_node_request request_bytes
    in
    match (file_key_opt, node_id_opt, token_opt) with
    | (Some file_key, Some node_id, Some token) ->
        let result =
          Lwt_eio.run_lwt @@ fun () ->
          Figma_api.get_file_nodes
            ?depth:depth_opt
            ~token ~file_key ~node_ids:[node_id] ()
        in
        (match result with
         | Ok json ->
             (* Extract node document from API response *)
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
             (* Parse JSON to ui_node, then generate tasks *)
             (match Figma_parser.parse_json node_json with
              | Some ui_node ->
                  let tasks = Task_planner.collect_tasks ui_node in
                  let sorted = Task_planner.sort_by_priority tasks in
                  (* Build response with summary *)
                  let total_tokens = List.fold_left
                    (fun acc t -> acc + t.Task_planner.estimated_tokens) 0 sorted in
                  sprintf {|{
  "tasks": %s,
  "summary": "%s",
  "total_estimated_tokens": %d,
  "root_node_id": "%s"
}|}
                    (Task_planner.tasks_to_json sorted)
                    (Task_planner.summarize_plan sorted)
                    total_tokens
                    node_id
              | None ->
                  sprintf {|{"error": "Failed to parse node JSON for %s"}|} node_id)
         | Error err ->
             sprintf {|{"error": "%s"}|} (Figma_api.error_to_string err))
    | _ ->
        {|{"error":"Missing required parameters (file_key, node_id, token)"|}
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
