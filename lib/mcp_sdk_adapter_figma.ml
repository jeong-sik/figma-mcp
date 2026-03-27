module MP = Mcp_protocol
module MT = MP.Mcp_types

type context = {
  send_notification :
    method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  send_log :
    MP.Logging.log_level -> string -> (unit, string) result;
  send_progress :
    token:MP.Mcp_result.progress_token ->
    progress:float ->
    total:float option ->
    message:string option ->
    (unit, string) result;
  request_sampling :
    MP.Sampling.create_message_params ->
    (MP.Sampling.create_message_result, string) result;
  request_roots_list : unit -> (MT.root list, string) result;
  request_elicitation :
    MT.elicitation_params -> (MT.elicitation_result, string) result;
}

type tool_handler =
  context -> string -> Yojson.Safe.t option -> (MT.tool_result, string) result

type resource_handler =
  context -> string -> (MT.resource_contents list, string) result

type prompt_handler =
  context -> string -> (string * string) list -> (MT.prompt_result, string) result

type registered_tool = {
  tool : MT.tool;
  handler : tool_handler;
}

type registered_resource = {
  resource : MT.resource;
  handler : resource_handler;
}

type registered_prompt = {
  prompt : MT.prompt;
  handler : prompt_handler;
}

type snapshot = {
  server_name : string;
  server_version : string;
  instructions : string option;
  tools : registered_tool list;
  resources : registered_resource list;
  resource_templates : MT.resource_template list;
  prompts : registered_prompt list;
}

let noop_context =
  {
    send_notification = (fun ~method_:_ ~params:_ -> Ok ());
    send_log = (fun _ _ -> Ok ());
    send_progress = (fun ~token:_ ~progress:_ ~total:_ ~message:_ -> Ok ());
    request_sampling = (fun _ -> Error "sampling not available in adapter context");
    request_roots_list = (fun () -> Ok []);
    request_elicitation =
      (fun _ -> Error "elicitation not available in adapter context");
  }

(** Convert local prompt (with text field) to SDK prompt type. *)
let sdk_prompt_of_local (prompt : Figma_mcp_protocol.mcp_prompt) : MT.prompt =
  let sdk_arg_of_local (arg : Figma_mcp_protocol.prompt_arg) : MT.prompt_argument =
    arg  (* prompt_arg = MT.prompt_argument, identity *)
  in
  {
    name = prompt.name;
    title = None;
    description = Some prompt.description;
    arguments = Some (List.map sdk_arg_of_local prompt.arguments);
    icon = None;
  }

let normalize_local_tool_result json =
  let open Yojson.Safe.Util in
  let top_is_error =
    match json |> member "isError" with
    | `Bool value -> value
    | _ -> false
  in
  let content_items =
    match json |> member "content" with
    | `List items -> items
    | _ -> []
  in
  let item_is_error item =
    match item with
    | `Assoc fields -> (
        match List.assoc_opt "isError" fields with
        | Some (`Bool value) -> value
        | _ -> false)
    | _ -> false
  in
  let strip_item_error item =
    match item with
    | `Assoc fields ->
        `Assoc (List.filter (fun (k, _) -> k <> "isError") fields)
    | _ -> item
  in
  let any_item_error = List.exists item_is_error content_items in
  let normalized_json =
    match json with
    | `Assoc fields ->
        let fields =
          List.map
            (fun (k, v) ->
              if k = "content" then
                (k, `List (List.map strip_item_error content_items))
              else (k, v))
            fields
        in
        let fields =
          if top_is_error || any_item_error then
            ("isError", `Bool true)
            :: List.filter (fun (k, _) -> k <> "isError") fields
          else fields
        in
        `Assoc fields
    | _ -> json
  in
  match MT.tool_result_of_yojson normalized_json with
  | Ok result -> result
  | Error _ ->
      if top_is_error || any_item_error then
        MT.tool_result_of_error (Yojson.Safe.pretty_to_string json)
      else
        MT.tool_result_of_text (Yojson.Safe.pretty_to_string json)

let render_prompt_text prompt arguments =
  List.fold_left
    (fun acc (key, value) ->
      Str.global_replace
        (Str.regexp_string ("{{" ^ key ^ "}}"))
        value acc)
    prompt.Figma_mcp_protocol.text arguments

let find_handler name =
  List.find_opt (fun (handler_name, _) -> String.equal handler_name name)
    Mcp_tool_registry.all_handlers_sync

let find_direct_handler name =
  match name with
  | "figma_parse_url" -> Some Mcp_tool_handlers.handle_parse_url
  | "figma_cache_stats" -> Some Mcp_tool_handlers.handle_cache_stats
  | "figma_doctor" -> Some Mcp_tool_handlers.handle_doctor
  | _ -> None

let make_tool_binding (tool : Figma_mcp_protocol.tool_def) : registered_tool =
  (* tool_def = MT.tool, no conversion needed *)
  let handler _ctx _name arguments =
    let args_json = Option.value arguments ~default:`Null in
    match find_direct_handler tool.name with
    | Some handler -> (
        match handler args_json with
        | Ok result -> Ok (normalize_local_tool_result result)
        | Error msg -> Error msg)
    | None -> (
        match find_handler tool.name with
        | None -> Error ("Handler not found: " ^ tool.name)
        | Some (_handler_name, handler) -> (
            match handler args_json with
            | Ok result -> Ok (normalize_local_tool_result result)
            | Error msg -> Error msg))
  in
  { tool; handler }

let make_resource_binding
    (resource : Figma_mcp_protocol.mcp_resource) : registered_resource =
  (* mcp_resource = MT.resource, no conversion needed *)
  let handler _ctx uri =
    match Mcp_tool_registry.read_resource uri with
    | Ok (mime_type, text) ->
        Ok [{ MT.uri; mime_type = Some mime_type; text = Some text; blob = None }]
    | Error msg -> Error msg
  in
  { resource; handler }

let make_prompt_binding (prompt : Figma_mcp_protocol.mcp_prompt) : registered_prompt =
  let sdk_prompt = sdk_prompt_of_local prompt in
  let handler _ctx _name arguments =
    let text = render_prompt_text prompt arguments in
    Ok
      {
        MT.description = Some prompt.description;
        messages =
          [
            {
              MT.role = MT.User;
              content = MT.PromptText { type_ = "text"; text };
            };
          ];
      }
  in
  { prompt = sdk_prompt; handler }

let make_snapshot () =
  {
    server_name = Figma_mcp_protocol.server_name;
    server_version = Figma_mcp_protocol.server_version;
    instructions = Some Figma_mcp_protocol.mcp_instructions;
    tools = List.map make_tool_binding Mcp_tool_defs.public_tools;
    resources = List.map make_resource_binding Mcp_tool_registry.resources;
    resource_templates = Mcp_tool_registry.resource_templates;
    prompts = List.map make_prompt_binding Mcp_tool_registry.prompts;
  }
