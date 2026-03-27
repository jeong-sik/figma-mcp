module MP = Mcp_protocol
module MT = MP.Mcp_types

type context = Mcp_protocol_eio.Handler.context = {
  send_notification :
    method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  send_log :
    MP.Logging.log_level -> string -> (unit, string) result;
  send_progress :
    token:MP.Mcp_result.progress_token ->
    progress:float ->
    total:float option ->
    (unit, string) result;
  request_sampling :
    MP.Sampling.create_message_params ->
    (MP.Sampling.create_message_result, string) result;
  request_roots_list : unit -> (MT.root list, string) result;
  request_elicitation :
    MT.elicitation_params -> (MT.elicitation_result, string) result;
}

type tool_handler = Mcp_protocol_eio.Handler.tool_handler

type resource_handler = Mcp_protocol_eio.Handler.resource_handler

type prompt_handler = Mcp_protocol_eio.Handler.prompt_handler

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
    send_progress = (fun ~token:_ ~progress:_ ~total:_ -> Ok ());
    request_sampling = (fun _ -> Error "sampling not available in adapter context");
    request_roots_list = (fun () -> Ok []);
    request_elicitation =
      (fun _ -> Error "elicitation not available in adapter context");
  }

let sdk_tool_of_local (tool : Figma_mcp_protocol.tool_def) : MT.tool =
  {
    name = tool.name;
    description = Some tool.description;
    input_schema = tool.input_schema;
    output_schema = None;
    title = None;
    annotations = None;
    icon = None;
    execution = None;
  }

let sdk_resource_of_local (resource : Figma_mcp_protocol.mcp_resource) : MT.resource =
  {
    uri = resource.uri;
    name = resource.name;
    description = Some resource.description;
    mime_type = Some resource.mime_type;
    icon = None;
  }

let sdk_resource_template_of_local
    (template : Figma_mcp_protocol.mcp_resource_template) : MT.resource_template =
  {
    uri_template = template.uri_template;
    name = template.name;
    description = Some template.description;
    mime_type = Some template.mime_type;
    icon = None;
  }

let sdk_prompt_of_local (prompt : Figma_mcp_protocol.mcp_prompt) : MT.prompt =
  let sdk_arg_of_local (arg : Figma_mcp_protocol.prompt_arg) : MT.prompt_argument =
    {
      name = arg.name;
      description = Some arg.description;
      required = Some arg.required;
    }
  in
  {
    name = prompt.name;
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
  let sdk_tool = sdk_tool_of_local tool in
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
  { tool = sdk_tool; handler }

let make_resource_binding
    (resource : Figma_mcp_protocol.mcp_resource) : registered_resource =
  let sdk_resource = sdk_resource_of_local resource in
  let handler _ctx uri =
    match Mcp_tool_registry.read_resource uri with
    | Ok (mime_type, text) ->
        Ok [{ MT.uri; mime_type = Some mime_type; text = Some text; blob = None }]
    | Error msg -> Error msg
  in
  { resource = sdk_resource; handler }

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
    resource_templates =
      List.map sdk_resource_template_of_local Mcp_tool_registry.resource_templates;
    prompts = List.map make_prompt_binding Mcp_tool_registry.prompts;
  }

let make_context () : Mcp_protocol_eio.Handler.context =
  {
    send_notification = (fun ~method_:_ ~params:_ -> Ok ());
    send_log = (fun _ _ -> Ok ());
    send_progress = (fun ~token:_ ~progress:_ ~total:_ -> Ok ());
    request_sampling =
      (fun _ -> Error "sampling/createMessage not available in figma adapter");
    request_roots_list = (fun () -> Ok []);
    request_elicitation =
      (fun _ -> Error "elicitation/create not available in figma adapter");
  }

let strip_logging_capability = function
  | `Assoc fields as json -> (
      match List.assoc_opt "result" fields with
      | Some (`Assoc result_fields) -> (
          match List.assoc_opt "capabilities" result_fields with
          | Some (`Assoc caps) ->
              let caps = List.remove_assoc "logging" caps in
              let result_fields =
                ("capabilities", `Assoc caps)
                :: List.remove_assoc "capabilities" result_fields
              in
              `Assoc
                (("result", `Assoc result_fields) :: List.remove_assoc "result" fields)
          | _ -> json)
      | _ -> json)
  | json -> json

let create_handler ?(snapshot = make_snapshot ()) () =
  let handler =
    Mcp_protocol_eio.Handler.create
      ~name:snapshot.server_name
      ~version:snapshot.server_version
      ?instructions:snapshot.instructions
      ()
  in
  let handler =
    List.fold_left
      (fun acc (registered : registered_tool) ->
        Mcp_protocol_eio.Handler.add_tool registered.tool registered.handler acc)
      handler snapshot.tools
  in
  let handler =
    List.fold_left
      (fun acc (registered : registered_resource) ->
        Mcp_protocol_eio.Handler.add_resource registered.resource
          registered.handler acc)
      handler snapshot.resources
  in
  let handler =
    List.fold_left
      (fun acc template ->
        Mcp_protocol_eio.Handler.add_resource_template template
          (fun _ctx uri ->
            match Mcp_tool_registry.read_resource uri with
            | Ok (mime_type, text) ->
                Ok
                  [
                    {
                      MT.uri;
                      mime_type = Some mime_type;
                      text = Some text;
                      blob = None;
                    };
                  ]
            | Error msg -> Error msg)
          acc)
      handler snapshot.resource_templates
  in
  List.fold_left
    (fun acc (registered : registered_prompt) ->
      Mcp_protocol_eio.Handler.add_prompt registered.prompt registered.handler acc)
    handler snapshot.prompts

let process_jsonrpc ?snapshot body_str =
  let json =
    try Ok (Yojson.Safe.from_string body_str)
    with Yojson.Json_error msg -> Error msg
  in
  match json with
  | Error msg ->
      Some
        (Yojson.Safe.to_string
           (Mcp_protocol.Jsonrpc.message_to_yojson
              (Mcp_protocol.Jsonrpc.make_error
                 ~id:(Mcp_protocol.Jsonrpc.Int 0)
                 ~code:Mcp_protocol.Error_codes.parse_error
                 ~message:("JSON parse error: " ^ msg)
                 ())))
  | Ok json -> (
      match Mcp_protocol.Jsonrpc.message_of_yojson json with
      | Error msg ->
          Some
            (Yojson.Safe.to_string
               (Mcp_protocol.Jsonrpc.message_to_yojson
                  (Mcp_protocol.Jsonrpc.make_error
                     ~id:(Mcp_protocol.Jsonrpc.Int 0)
                     ~code:Mcp_protocol.Error_codes.parse_error
                     ~message:("JSON-RPC parse error: " ^ msg)
                     ())))
      | Ok message ->
          let handler = create_handler ?snapshot () in
          let ctx = make_context () in
          let log_level_ref = ref Mcp_protocol.Logging.Warning in
          Mcp_protocol_eio.Handler.dispatch handler ctx log_level_ref message
          |> Option.map (fun response ->
                 let json = Mcp_protocol.Jsonrpc.message_to_yojson response in
                 let json =
                   match message with
                   | Mcp_protocol.Jsonrpc.Request req
                     when String.equal req.method_ Mcp_protocol.Notifications.initialize ->
                       strip_logging_capability json
                   | _ -> json
                 in
                 json
                 |> Yojson.Safe.to_string))
