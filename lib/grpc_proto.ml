(** gRPC Protobuf encoder/decoder (minimal, proto3-compatible). *)

type field_value =
  | Varint of int64
  | Fixed32 of int32
  | Fixed64 of int64
  | Bytes of string

type field = {
  num: int;
  wire: int;
  value: field_value;
}

let compress s = s

let decode_varint bytes pos =
  let len = String.length bytes in
  let rec loop shift acc idx =
    if idx >= len then Error "varint: truncated"
    else if shift > 63 then Error "varint: overflow"
    else
      let b = Char.code (String.get bytes idx) in
      let acc' = Int64.logor acc (Int64.shift_left (Int64.of_int (b land 0x7f)) shift) in
      if b land 0x80 = 0 then Ok (acc', idx + 1)
      else loop (shift + 7) acc' (idx + 1)
  in
  loop 0 0L pos

let decode_fixed32 bytes pos =
  if pos + 4 > String.length bytes then Error "fixed32: truncated"
  else
    let b0 = Char.code (String.get bytes pos) in
    let b1 = Char.code (String.get bytes (pos + 1)) in
    let b2 = Char.code (String.get bytes (pos + 2)) in
    let b3 = Char.code (String.get bytes (pos + 3)) in
    let v =
      Int32.logor (Int32.of_int b0)
        (Int32.logor (Int32.shift_left (Int32.of_int b1) 8)
           (Int32.logor (Int32.shift_left (Int32.of_int b2) 16)
              (Int32.shift_left (Int32.of_int b3) 24)))
    in
    Ok (v, pos + 4)

let decode_fixed64 bytes pos =
  if pos + 8 > String.length bytes then Error "fixed64: truncated"
  else
    let b i = Int64.of_int (Char.code (String.get bytes (pos + i))) in
    let v =
      Int64.logor (b 0)
        (Int64.logor (Int64.shift_left (b 1) 8)
           (Int64.logor (Int64.shift_left (b 2) 16)
              (Int64.logor (Int64.shift_left (b 3) 24)
                 (Int64.logor (Int64.shift_left (b 4) 32)
                    (Int64.logor (Int64.shift_left (b 5) 40)
                       (Int64.logor (Int64.shift_left (b 6) 48)
                          (Int64.shift_left (b 7) 56)))))))
    in
    Ok (v, pos + 8)

let decode_fields bytes =
  let len = String.length bytes in
  let rec loop pos acc =
    if pos >= len then List.rev acc
    else
      match decode_varint bytes pos with
      | Error _ -> List.rev acc
      | Ok (key, pos1) ->
          let num = Int64.to_int (Int64.shift_right_logical key 3) in
          let wire = Int64.to_int (Int64.logand key 0x7L) in
          let result =
            match wire with
            | 0 -> (
                match decode_varint bytes pos1 with
                | Ok (v, pos2) -> Some ({ num; wire; value = Varint v }, pos2)
                | Error _ -> None)
            | 1 -> (
                match decode_fixed64 bytes pos1 with
                | Ok (v, pos2) -> Some ({ num; wire; value = Fixed64 v }, pos2)
                | Error _ -> None)
            | 2 -> (
                match decode_varint bytes pos1 with
                | Ok (len64, pos2) ->
                    let l = Int64.to_int len64 in
                    if l < 0 || pos2 + l > len then None
                    else
                      let data = String.sub bytes pos2 l in
                      Some ({ num; wire; value = Bytes data }, pos2 + l)
                | Error _ -> None)
            | 5 -> (
                match decode_fixed32 bytes pos1 with
                | Ok (v, pos2) -> Some ({ num; wire; value = Fixed32 v }, pos2)
                | Error _ -> None)
            | _ -> None
          in
          (match result with
           | Some (field, pos2) -> loop pos2 (field :: acc)
           | None -> List.rev acc)
  in
  loop 0 []

let find_last_field num fields =
  List.fold_left (fun acc field ->
    if field.num = num then Some field else acc
  ) None fields

let find_all_fields num fields =
  List.filter (fun field -> field.num = num) fields

let get_string num fields =
  match find_last_field num fields with
  | Some { value = Bytes s; _ } -> Some s
  | _ -> None

let get_strings num fields =
  find_all_fields num fields
  |> List.filter_map (fun field ->
      match field.value with
      | Bytes s -> Some s
      | _ -> None)

let get_int num fields =
  match find_last_field num fields with
  | Some { value = Varint v; _ } -> Some (Int64.to_int v)
  | _ -> None

let get_int64 num fields =
  match find_last_field num fields with
  | Some { value = Varint v; _ } -> Some v
  | Some { value = Fixed64 v; _ } -> Some v
  | _ -> None

let get_bool num fields =
  match find_last_field num fields with
  | Some { value = Varint v; _ } -> Some (v <> 0L)
  | _ -> None

let get_float32 num fields =
  match find_last_field num fields with
  | Some { value = Fixed32 v; _ } -> Some (Int32.float_of_bits v)
  | _ -> None

let get_float64 num fields =
  match find_last_field num fields with
  | Some { value = Fixed64 v; _ } -> Some (Int64.float_of_bits v)
  | _ -> None

let write_varint buf n =
  let rec loop v =
    let byte = Int64.to_int (Int64.logand v 0x7fL) in
    let rest = Int64.shift_right_logical v 7 in
    if rest = 0L then Buffer.add_char buf (Char.chr byte)
    else begin
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop rest
    end
  in
  loop n

let write_fixed32 buf v =
  let open Int32 in
  let b0 = logand v 0xFFl |> to_int in
  let b1 = shift_right_logical v 8 |> logand 0xFFl |> to_int in
  let b2 = shift_right_logical v 16 |> logand 0xFFl |> to_int in
  let b3 = shift_right_logical v 24 |> logand 0xFFl |> to_int in
  Buffer.add_char buf (Char.chr b0);
  Buffer.add_char buf (Char.chr b1);
  Buffer.add_char buf (Char.chr b2);
  Buffer.add_char buf (Char.chr b3)

let write_fixed64 buf v =
  let open Int64 in
  let b n = to_int (logand (shift_right_logical v n) 0xFFL) in
  Buffer.add_char buf (Char.chr (b 0));
  Buffer.add_char buf (Char.chr (b 8));
  Buffer.add_char buf (Char.chr (b 16));
  Buffer.add_char buf (Char.chr (b 24));
  Buffer.add_char buf (Char.chr (b 32));
  Buffer.add_char buf (Char.chr (b 40));
  Buffer.add_char buf (Char.chr (b 48));
  Buffer.add_char buf (Char.chr (b 56))

let add_key buf field wire =
  let key = Int64.of_int ((field lsl 3) lor wire) in
  write_varint buf key

let add_varint buf field value =
  add_key buf field 0;
  write_varint buf value

let add_int32 buf field value =
  add_varint buf field (Int64.of_int value)

let add_int64 buf field value =
  add_varint buf field value

let add_bool buf field value =
  add_varint buf field (if value then 1L else 0L)

let add_float32 buf field value =
  add_key buf field 5;
  write_fixed32 buf (Int32.bits_of_float value)

let add_float64 buf field value =
  add_key buf field 1;
  write_fixed64 buf (Int64.bits_of_float value)

let add_bytes buf field value =
  add_key buf field 2;
  write_varint buf (Int64.of_int (String.length value));
  Buffer.add_string buf value

let add_string buf field value =
  add_bytes buf field value

let add_message buf field encode =
  let payload = encode () in
  add_bytes buf field payload

type get_node_request = {
  file_key: string option;
  node_id: string option;
  token: string option;
  format: string option;
  depth_start: int option;
  depth_end: int option;
  geometry: bool;
  plugin_data: bool;
  version: string option;
  recursive: bool;
  recursive_max_depth: int option;
  recursive_max_nodes: int option;
  recursive_depth_per_call: int option;
}

let decode_get_node_request bytes =
  let fields = decode_fields bytes in
  {
    file_key = get_string 1 fields;
    node_id = get_string 2 fields;
    token = get_string 3 fields;
    depth_start = get_int 4 fields;
    depth_end = get_int 5 fields;
    format = get_string 6 fields;
    geometry = get_bool 7 fields |> Option.value ~default:false;
    plugin_data = get_bool 8 fields |> Option.value ~default:false;
    version = get_string 10 fields;
    recursive = get_bool 11 fields |> Option.value ~default:false;
    recursive_max_depth = get_int 12 fields;
    recursive_max_nodes = get_int 13 fields;
    recursive_depth_per_call = get_int 14 fields;
  }

type file_meta_request = {
  file_key: string option;
  token: string option;
  version: string option;
}

let decode_file_meta_request bytes =
  let fields = decode_fields bytes in
  {
    file_key = get_string 1 fields;
    token = get_string 2 fields;
    version = get_string 3 fields;
  }

type fidelity_loop_request = {
  file_key: string option;
  node_id: string option;
  token: string option;
  target_score: float;
  start_depth: int;
  max_depth: int;
  depth_step: int;
  include_meta: bool;
  include_variables: bool;
  include_image_fills: bool;
}

let decode_fidelity_loop_request bytes =
  let fields = decode_fields bytes in
  {
    file_key = get_string 1 fields;
    node_id = get_string 2 fields;
    token = get_string 3 fields;
    target_score = get_float32 4 fields |> Option.value ~default:0.92;
    start_depth = get_int 5 fields |> Option.value ~default:4;
    max_depth = get_int 6 fields |> Option.value ~default:20;
    depth_step = get_int 7 fields |> Option.value ~default:4;
    include_meta = get_bool 10 fields |> Option.value ~default:false;
    include_variables = get_bool 11 fields |> Option.value ~default:false;
    include_image_fills = get_bool 12 fields |> Option.value ~default:false;
  }

type split_stream_request = {
  file_key: string option;
  node_id: string option;
  token: string option;
  depth: int option;
  include_styles: bool;
  include_layouts: bool;
  include_contents: bool;
}

let decode_split_stream_request bytes =
  let fields = decode_fields bytes in
  {
    file_key = get_string 1 fields;
    node_id = get_string 2 fields;
    token = get_string 3 fields;
    depth = get_int 4 fields;
    include_styles = get_bool 10 fields |> Option.value ~default:true;
    include_layouts = get_bool 11 fields |> Option.value ~default:true;
    include_contents = get_bool 12 fields |> Option.value ~default:true;
  }

type plan_tasks_request = {
  file_key: string option;
  node_id: string option;
  token: string option;
  depth: int option;
  max_tasks: int option;
  recursive: bool;
  recursive_max_depth: int option;
  recursive_max_nodes: int option;
  recursive_depth_per_call: int option;
}

let decode_plan_tasks_request bytes =
  let fields = decode_fields bytes in
  {
    file_key = get_string 1 fields;
    node_id = get_string 2 fields;
    token = get_string 3 fields;
    depth = get_int 4 fields;
    max_tasks = get_int 5 fields;
    recursive = get_bool 10 fields |> Option.value ~default:false;
    recursive_max_depth = get_int 11 fields;
    recursive_max_nodes = get_int 12 fields;
    recursive_depth_per_call = get_int 13 fields;
  }

type color = { r: float; g: float; b: float; a: float }

let encode_color (c : color) =
  let buf = Buffer.create 32 in
  add_float32 buf 1 c.r;
  add_float32 buf 2 c.g;
  add_float32 buf 3 c.b;
  add_float32 buf 4 c.a;
  Buffer.contents buf

let encode_node ~id ~name =
  let buf = Buffer.create 64 in
  add_string buf 1 id;
  add_string buf 2 name;
  Buffer.contents buf

let encode_figma_node ~node_id ~node_name ~depth ~parent_id ~child_count ~dsl
    ~node_index ~total_nodes =
  let buf = Buffer.create 256 in
  add_message buf 1 (fun () -> encode_node ~id:node_id ~name:node_name);
  add_int32 buf 10 depth;
  if parent_id <> "" then add_string buf 11 parent_id;
  add_int32 buf 12 child_count;
  add_bytes buf 20 dsl;
  add_int32 buf 30 node_index;
  add_int32 buf 31 total_nodes;
  Buffer.contents buf

type fidelity_progress = {
  attempt: int;
  current_depth: int;
  current_score: float;
  dsl: string option;
  done_: bool;
  success: bool;
  final_dsl: string option;
  error: string option;
  node_count: int option;
  raw_size: int option;
  compressed_size: int option;
}

let encode_fidelity_progress (p : fidelity_progress) =
  let buf = Buffer.create 128 in
  add_int32 buf 1 p.attempt;
  add_int32 buf 2 p.current_depth;
  add_float32 buf 3 p.current_score;
  (match p.dsl with Some v -> add_bytes buf 10 v | None -> ());
  add_bool buf 20 p.done_;
  add_bool buf 21 p.success;
  (match p.final_dsl with Some v -> add_bytes buf 22 v | None -> ());
  (match p.error with Some v -> add_string buf 23 v | None -> ());
  (match p.node_count with Some v -> add_int32 buf 30 v | None -> ());
  (match p.raw_size with Some v -> add_int64 buf 31 (Int64.of_int v) | None -> ());
  (match p.compressed_size with Some v -> add_int64 buf 32 (Int64.of_int v) | None -> ());
  Buffer.contents buf

type style_chunk = {
  id: string;
  node_id: string;
  fill_colors: color list;
  stroke_colors: color list;
  font_family: string option;
  font_size: float option;
  font_weight: int option;
  line_height: float option;
  letter_spacing: float option;
  opacity: float option;
  corner_radius: float option;
  stroke_weight: float option;
}

type layout_chunk = {
  id: string;
  node_id: string;
  parent_id: string option;
  x: float option;
  y: float option;
  width: float option;
  height: float option;
  horizontal_constraint: string option;
  vertical_constraint: string option;
  layout_mode: string option;
  primary_axis_align: string option;
  counter_axis_align: string option;
  item_spacing: float option;
  padding_left: float option;
  padding_right: float option;
  padding_top: float option;
  padding_bottom: float option;
  clips_content: bool option;
}

type content_chunk = {
  id: string;
  node_id: string;
  node_type: string;
  name: string;
  text_content: string option;
  image_ref: string option;
}

type split_chunk =
  | Style of style_chunk
  | Layout of layout_chunk
  | Content of content_chunk

let encode_style_chunk (c : style_chunk) =
  let buf = Buffer.create 128 in
  add_string buf 1 c.id;
  add_string buf 2 c.node_id;
  List.iter (fun color -> add_message buf 10 (fun () -> encode_color color)) c.fill_colors;
  List.iter (fun color -> add_message buf 11 (fun () -> encode_color color)) c.stroke_colors;
  (match c.font_family with Some v -> add_string buf 20 v | None -> ());
  (match c.font_size with Some v -> add_float64 buf 21 v | None -> ());
  (match c.font_weight with Some v -> add_int32 buf 22 v | None -> ());
  (match c.line_height with Some v -> add_float64 buf 23 v | None -> ());
  (match c.letter_spacing with Some v -> add_float64 buf 24 v | None -> ());
  (match c.opacity with Some v -> add_float64 buf 31 v | None -> ());
  (match c.corner_radius with Some v -> add_float64 buf 32 v | None -> ());
  (match c.stroke_weight with Some v -> add_float64 buf 40 v | None -> ());
  Buffer.contents buf

let encode_layout_chunk (c : layout_chunk) =
  let buf = Buffer.create 128 in
  add_string buf 1 c.id;
  add_string buf 2 c.node_id;
  (match c.parent_id with Some v -> add_string buf 3 v | None -> ());
  (match c.x with Some v -> add_float64 buf 10 v | None -> ());
  (match c.y with Some v -> add_float64 buf 11 v | None -> ());
  (match c.width with Some v -> add_float64 buf 12 v | None -> ());
  (match c.height with Some v -> add_float64 buf 13 v | None -> ());
  (match c.horizontal_constraint with Some v -> add_string buf 20 v | None -> ());
  (match c.vertical_constraint with Some v -> add_string buf 21 v | None -> ());
  (match c.layout_mode with Some v -> add_string buf 30 v | None -> ());
  (match c.primary_axis_align with Some v -> add_string buf 31 v | None -> ());
  (match c.counter_axis_align with Some v -> add_string buf 32 v | None -> ());
  (match c.item_spacing with Some v -> add_float64 buf 33 v | None -> ());
  (match c.padding_left with Some v -> add_float64 buf 34 v | None -> ());
  (match c.padding_right with Some v -> add_float64 buf 35 v | None -> ());
  (match c.padding_top with Some v -> add_float64 buf 36 v | None -> ());
  (match c.padding_bottom with Some v -> add_float64 buf 37 v | None -> ());
  (match c.clips_content with Some v -> add_bool buf 51 v | None -> ());
  Buffer.contents buf

let encode_content_chunk (c : content_chunk) =
  let buf = Buffer.create 128 in
  add_string buf 1 c.id;
  add_string buf 2 c.node_id;
  add_string buf 3 c.node_type;
  add_string buf 4 c.name;
  (match c.text_content with Some v -> add_string buf 10 v | None -> ());
  (match c.image_ref with Some v -> add_string buf 20 v | None -> ());
  Buffer.contents buf

let encode_split_chunk ~sequence ~total_chunks ~node_id ~chunk =
  let buf = Buffer.create 128 in
  add_int32 buf 1 sequence;
  add_int32 buf 2 total_chunks;
  add_string buf 3 node_id;
  (match chunk with
   | Style style ->
       add_message buf 10 (fun () -> encode_style_chunk style)
   | Layout layout ->
       add_message buf 11 (fun () -> encode_layout_chunk layout)
   | Content content ->
       add_message buf 12 (fun () -> encode_content_chunk content));
  Buffer.contents buf

type task = {
  id: string;
  node_id: string;
  node_name: string;
  node_type: string;
  priority: int;
  dependencies: string list;
  estimated_tokens: int;
  semantic_dsl: string;
  hints: string list;
}

let encode_task (t : task) =
  let buf = Buffer.create 128 in
  add_string buf 1 t.id;
  add_string buf 2 t.node_id;
  add_string buf 3 t.node_name;
  add_string buf 4 t.node_type;
  add_int32 buf 5 t.priority;
  List.iter (fun dep -> add_string buf 6 dep) t.dependencies;
  add_int32 buf 7 t.estimated_tokens;
  add_string buf 8 t.semantic_dsl;
  List.iter (fun hint -> add_string buf 9 hint) t.hints;
  Buffer.contents buf

let encode_plan_tasks_response ~summary ~requirements_json ~tasks ~total_estimated_tokens ~root_node_id =
  let buf = Buffer.create 256 in
  List.iter (fun t -> add_message buf 1 (fun () -> encode_task t)) tasks;
  add_int32 buf 2 total_estimated_tokens;
  add_string buf 3 root_node_id;
  (match summary with Some s -> add_string buf 4 s | None -> ());
  (match requirements_json with Some s -> add_string buf 5 s | None -> ());
  Buffer.contents buf

let encode_file_meta_response ?name ?last_modified ?thumbnail_url ?version ?role
    ?component_count ?style_count ?pages () =
  let buf = Buffer.create 128 in
  (match name with Some v -> add_string buf 1 v | None -> ());
  (match last_modified with Some v -> add_string buf 2 v | None -> ());
  (match thumbnail_url with Some v -> add_string buf 3 v | None -> ());
  (match version with Some v -> add_string buf 4 v | None -> ());
  (match role with Some v -> add_string buf 5 v | None -> ());
  (match component_count with Some v -> add_int32 buf 10 v | None -> ());
  (match style_count with Some v -> add_int32 buf 11 v | None -> ());
  (match pages with Some encoded_pages -> List.iter (fun p -> add_message buf 20 (fun () -> p)) encoded_pages | None -> ());
  Buffer.contents buf
