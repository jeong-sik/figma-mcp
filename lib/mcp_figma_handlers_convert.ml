(** Conversion handlers: webhook, code-to-figma, vision compare. *)

open Printf
open Mcp_figma_handlers_common

(* ============== Webhook ============== *)

[@@@coverage on]
let constant_time_equal a b =
  let a = String.trim a in
  let b = String.trim b in
  let la = String.length a in
  let lb = String.length b in
  if la <> lb then false
  else
    let diff = ref 0 in
    for i = 0 to la - 1 do
      diff := !diff lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !diff = 0

let webhook_passcode_secret_opt () =
  let nonempty name =
    match Sys.getenv_opt name with
    | Some v ->
        let v = String.trim v in
        if v = "" then None else Some v
    | None -> None
  in
  match nonempty "FIGMA_MCP_WEBHOOK_PASSCODE" with
  | Some _ as v -> v
  | None -> nonempty "FIGMA_WEBHOOK_PASSCODE"

let validate_webhook_passcode ~allow_no_auth ~secret_opt ~passcode =
  match secret_opt with
  | None ->
      if allow_no_auth then
        Error "Webhook passcode required when no-auth mode is enabled (set FIGMA_MCP_WEBHOOK_PASSCODE)"
      else
        Ok ()
  | Some secret ->
      let passcode = String.trim passcode in
      if passcode = "" then Error "Missing webhook passcode"
      else if constant_time_equal secret passcode then Ok ()
      else Error "Invalid webhook passcode"

[@@@coverage off]
let webhook_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in

        (* Parse Figma webhook payload *)
        let event_type = member "event_type" json |> to_string_option |> Option.value ~default:"" in
        let file_key = member "file_key" json |> to_string_option |> Option.value ~default:"" in
        let file_name = member "file_name" json |> to_string_option |> Option.value ~default:"" in
        let timestamp = member "timestamp" json |> to_string_option |> Option.value ~default:"" in
        let passcode = member "passcode" json |> to_string_option |> Option.value ~default:"" in

        let secret_opt = webhook_passcode_secret_opt () in
        (match validate_webhook_passcode ~allow_no_auth:!(allow_no_auth) ~secret_opt ~passcode with
         | Error err ->
             let body = `Assoc [("error", `String err)] in
             Response.json ~status:`Forbidden (Yojson.Safe.to_string body) reqd
         | Ok () ->

             (* Log the webhook event *)
             printf "[Webhook] %s: file=%s (%s) at %s\n%!" event_type file_key file_name timestamp;

        (* Determine action based on event type *)
        let action = match event_type with
          | "FILE_UPDATE" -> "regenerate_code"
          | "FILE_VERSION_UPDATE" -> "sync_version"
          | "FILE_DELETE" -> "archive_code"
          | "LIBRARY_PUBLISH" -> "update_tokens"
          | _ -> "unknown"
        in

        (* Build response with recommended action *)
        let result = `Assoc [
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
        ] in
             Response.json (Yojson.Safe.to_string result) reqd)
  )

(* ============== Code to Figma ============== *)

(** POST /plugin/code-to-figma - Convert code to Figma DSL *)
let code_to_figma_handler ~sw ~eio_ctx _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let target_name = member "name" json |> to_string_option |> Option.value ~default:"GeneratedComponent" in

        if String.length code < 10 then begin
          json_error "Code too short" reqd
        end else begin
          (* Build prompt for code -> Figma DSL conversion *)
          let prompt = sprintf {|Analyze this React component and generate Figma node creation instructions.

CODE:
%s

Generate a JSON array of Figma node creation operations. Each operation should have:
- "action": one of "create_frame", "create_rectangle", "create_text", "create_ellipse"
- "name": node name
- "width", "height": dimensions in pixels
- "x", "y": position relative to parent
- "fills": array of {type: "SOLID", color: "#hexcolor"}
- "cornerRadius": optional, for rounded corners
- "children": nested operations array
- For text: "text", "fontSize", "fontWeight"

Output ONLY valid JSON array, no explanation. Example:
[
  {"action": "create_frame", "name": "Card", "width": 270, "height": 120, "fills": [{"type": "SOLID", "color": "#ffffff"}], "cornerRadius": 16, "children": [
    {"action": "create_text", "name": "Title", "x": 24, "y": 20, "text": "Hello", "fontSize": 16, "fills": [{"type": "SOLID", "color": "#1a1a26"}]}
  ]}
]|} code
          in

          (* Try LLM - Ollama for now *)
          let send_error msg =
            let result = `Assoc [("error", `String msg)] in
            Response.json (Yojson.Safe.to_string result) reqd
          in

          (try
            let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_helpers.client in
            let ollama_url = "http://127.0.0.1:11434/api/generate" in
            let ollama_body = `Assoc [
              ("model", `String "qwen3-coder:30b");
              ("prompt", `String prompt);
              ("stream", `Bool false);
            ] in
            let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
            let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string ollama_body) in
            let uri = Uri.of_string ollama_url in
            let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
            let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
            if status_code < 200 || status_code >= 300 then
              send_error "LLM request failed"
            else begin
              let ollama_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
              let ollama_resp = Yojson.Safe.from_string ollama_resp_str in
              let llm_output = member "response" ollama_resp |> to_string_option |> Option.value ~default:"[]" in
              (* Extract JSON from response (might have markdown code blocks) *)
              let json_str =
                if String.length llm_output > 0 && llm_output.[0] = '[' then llm_output
                else
                  let re = Str.regexp {|\[\(.*\)\]|} in
                  if Str.string_match re llm_output 0 then Str.matched_string llm_output
                  else "[]"
              in
              let result = `Assoc [
                ("operations", Yojson.Safe.from_string json_str);
                ("name", `String target_name);
                ("source", `String "ollama")
              ] in
              Response.json (Yojson.Safe.to_string result) reqd
            end
          with exn ->
            send_error (sprintf "Error: %s" (Printexc.to_string exn)))
        end
  )

(* ============== Vision Compare Safety ============== *)

[@@@coverage on]
let has_suffix ~suffix s =
  let ls = String.length s in
  let l = String.length suffix in
  ls >= l && String.sub s (ls - l) l = suffix

let trim s = String.trim s

let split_csv s =
  s
  |> String.split_on_char ','
  |> List.map trim
  |> List.filter (fun x -> x <> "")

let normalize_dir_prefix path =
  let p = trim path in
  if p = "" then None
  else
    let rp = try Unix.realpath p with exn ->
      Printf.eprintf "[mcp_protocol] Warning: realpath failed for '%s': %s, using original\n%!" p (Printexc.to_string exn);
      p
    in
    if rp = "/" then Some rp
    else if has_suffix ~suffix:"/" rp then Some rp
    else Some (rp ^ "/")

let is_under_dir ~dir_prefix path =
  if dir_prefix = "/" then true
  else
    let lp = String.length dir_prefix in
    String.length path >= lp && String.sub path 0 lp = dir_prefix

let validate_reference_image_path ~roots ~max_bytes path : (string, string) result =
  if trim path = "" then
    Error "reference path required"
  else if not (Sys.file_exists path) then
    Error "Reference image not found"
  else
    let lower = String.lowercase_ascii path in
    if not (has_suffix ~suffix:".png" lower) then
      Error "Reference must be a .png file"
    else
      let st =
        try Ok (Unix.stat path)
        with Unix.Unix_error (e, _, _) ->
          Error (Printf.sprintf "Failed to stat reference image: %s" (Unix.error_message e))
      in
      match st with
      | Error e -> Error e
      | Ok st ->
          if st.Unix.st_kind <> Unix.S_REG then
            Error "Reference must be a regular file"
          else if st.Unix.st_size > max_bytes then
            Error "Reference image too large"
          else
            let rp =
              try Ok (Unix.realpath path)
              with Unix.Unix_error _ -> Error "Failed to resolve reference image path"
            in
            match rp with
            | Error e -> Error e
            | Ok rp ->
                let prefixes =
                  roots
                  |> List.filter_map normalize_dir_prefix
                in
                if prefixes = [] then Ok rp
                else if List.exists (fun dir_prefix -> is_under_dir ~dir_prefix rp) prefixes then Ok rp
                else Error "Reference path not allowed (set FIGMA_MCP_VISION_REFERENCE_ROOTS)"

[@@@coverage off]
(* POST /plugin/vision-compare - Compare Figma export with rendered code *)
let vision_compare_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let reference_path = member "reference" json |> to_string_option |> Option.value ~default:"" in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let width = member "width" json |> to_int_option |> Option.value ~default:375 in
        let height = member "height" json |> to_int_option |> Option.value ~default:812 in
        let threshold = member "threshold" json |> to_float_option |> Option.value ~default:0.95 in

        if reference_path = "" then begin
          json_error "reference path required" reqd
        end else if code = "" then begin
          json_error "code required" reqd
        end else begin
          let roots =
            match Sys.getenv_opt "FIGMA_MCP_VISION_REFERENCE_ROOTS" with
            | None -> []
            | Some v -> split_csv v
          in
          let max_bytes =
            env_int ~name:"FIGMA_MCP_VISION_REFERENCE_MAX_BYTES" ~default:(50 * 1024 * 1024)
          in
          match validate_reference_image_path ~roots ~max_bytes reference_path with
          | Error err ->
              let result = `Assoc [
                ("error", `String err);
                ("hint", `String "Configure FIGMA_MCP_VISION_REFERENCE_ROOTS to restrict allowed reference paths.");
              ] in
              Response.json ~status:`Bad_request (Yojson.Safe.to_string result) reqd
          | Ok reference_path ->
          (* Wrap code in HTML boilerplate *)
          let html_content = sprintf {|<!DOCTYPE html>
<html><head>
<meta charset="UTF-8">
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: 'Inter', -apple-system, sans-serif; }
</style>
</head><body>
<div id="root">%s</div>
</body></html>|} code
          in
          (* Render HTML -> PNG (Playwright) *)
          match Visual_verifier.render_html_to_png ~width ~height html_content with
          | Error err ->
              let result = `Assoc [
                ("error", `String ("Render failed: " ^ err));
                ("reference", `String reference_path);
                ("hint", `String "Ensure Node + Playwright deps are installed under ./scripts (npm ci) and try again.");
              ] in
              Response.json ~status:`Internal_server_error (Yojson.Safe.to_string result) reqd
          | Ok rendered_path ->
              (* Compare with SSIM + region analysis (Node script fallback if needed) *)
              (match Visual_verifier.compare_renders_with_regions ~figma_png:reference_path ~html_png:rendered_path with
               | Error err ->
                   let result = `Assoc [
                     ("error", `String ("SSIM comparison failed: " ^ err));
                     ("reference", `String reference_path);
                     ("rendered", `String rendered_path);
                   ] in
                   Response.json ~status:`Internal_server_error (Yojson.Safe.to_string result) reqd
               | Ok metrics ->
                   let ssim_score = metrics.ssim in
                   let delta_e = metrics.delta_e in
                   let human_ssim = Visual_verifier.calculate_human_ssim ssim_score delta_e in
                   let passed = ssim_score >= threshold in
                   if passed then (try Sys.remove rendered_path with Sys_error _ -> ());
                   let advanced_json =
                     match metrics.advanced with
                     | None -> `Null
                     | Some adv ->
                         `Assoc [
                           ("true_ssim", `Float adv.true_ssim);
                           ("ms_ssim", `Float adv.ms_ssim);
                           ("pixel_match", `Float adv.pixel_match);
                           ("lpips", (match adv.lpips with Some v -> `Float v | None -> `Null));
                         ]
                   in
                   let result = `Assoc [
                     ("ssim", `Float ssim_score);
                     ("delta_e", `Float delta_e);
                     ("human_ssim", `Float human_ssim);
                     ("threshold", `Float threshold);
                     ("pass", `Bool passed);
                     ("reference", `String reference_path);
                     ("rendered", `String (if passed then "(cleaned up)" else rendered_path));
                     ("regions", `Assoc [
                       ("quadrants", `Assoc [
                         ("top_left", `Float metrics.regions.quadrants.top_left);
                         ("top_right", `Float metrics.regions.quadrants.top_right);
                         ("bottom_left", `Float metrics.regions.quadrants.bottom_left);
                         ("bottom_right", `Float metrics.regions.quadrants.bottom_right);
                       ]);
                       ("strips", `Assoc [
                         ("top", `Float metrics.regions.strips.strip_top);
                         ("middle", `Float metrics.regions.strips.strip_middle);
                         ("bottom", `Float metrics.regions.strips.strip_bottom);
                       ]);
                       ("edges", `Assoc [
                         ("top", `Float metrics.regions.edges.edge_top);
                         ("bottom", `Float metrics.regions.edges.edge_bottom);
                         ("left", `Float metrics.regions.edges.edge_left);
                         ("right", `Float metrics.regions.edges.edge_right);
                       ]);
                     ]);
	                     ("advanced", advanced_json);
	                   ] in
	                   Response.json (Yojson.Safe.to_string result) reqd)
        end
  )
