(** Visual & verification handlers extracted from mcp_tools.ml.
    Handlers: fidelity_loop, image_similarity, verify_visual,
    verify_semantic, compare_regions, evolution_report,
    compare_elements, compare. *)

open Mcp_helpers
open Mcp_plugin_handlers
open Printf

(** figma_fidelity_loop 핸들러 *)
let handle_fidelity_loop args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let format = get_string_or "format" "fidelity" args in
  let target_score = get_float_or "target_score" 0.92 args in
  let start_depth = get_int_positive "start_depth" 4 args in
  let depth_step = get_int_positive "depth_step" 4 args in
  let max_depth = get_int_positive "max_depth" 20 args in
  let max_attempts = get_int_positive "max_attempts" 4 args in
  let geometry = match get_string "geometry" args with Some g -> Some g | None -> Some "paths" in
  let plugin_data = get_string "plugin_data" args in
  let include_meta = get_bool_or "include_meta" true args in
  let include_variables = get_bool_or "include_variables" true args in
  let include_image_fills = get_bool_or "include_image_fills" true args in
  let auto_plugin =
    match get_bool "auto_plugin" args with
    | Some b -> b
    | None -> Option.is_some (get_string "url" args)
  in
  let include_plugin =
    match get_bool "include_plugin" args with
    | Some b -> b
    | None -> auto_plugin
  in
  let include_plugin_variables = get_bool_or "include_plugin_variables" false args in
  let plugin_channel_id = get_string "plugin_channel_id" args in
  let plugin_depth = get_int_positive "plugin_depth" 6 args in
  let plugin_timeout_ms = get_int "plugin_timeout_ms" args |> Option.value ~default:20000 in
  let summary_only = get_bool_or "summary_only" false args in
  let max_inline_bytes =
    match get_int "max_inline_bytes" args with
    | Some n when n > 0 -> n
    | _ -> Large_response.max_inline_size
  in

  let clamp_score v =
    if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v
  in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      if format <> "fidelity" then
        Error "figma_fidelity_loop only supports format=fidelity"
      else
        let node_id = normalize_node_id node_id in
        let target_score = clamp_score target_score in
        let file_meta =
          if include_meta then
            match Figma_effects.Perform.get_file_meta ~token ~file_key () with
            | Ok meta_json -> build_file_meta meta_json
            | Error err -> `Assoc [("error", `String err)]
          else
            `Null
        in
        let resolve_plugin_channel () =
          match plugin_channel_id with
          | Some id -> Ok id
          | None -> resolve_channel_id args
        in
        let want_plugin_variables =
          include_plugin_variables || (include_plugin && include_variables)
        in
        let plugin_variables =
          if want_plugin_variables then
            match resolve_plugin_channel () with
            | Error msg -> `Assoc [("error", `String msg)]
            | Ok channel_id ->
                let payload = `Assoc [] in
                let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_variables" ~payload in
                (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                 | Error err -> `Assoc [("error", `String err)]
                 | Ok result ->
                     `Assoc [
                       ("channel_id", `String channel_id);
                       ("command_id", `String command_id);
                       ("ok", `Bool result.ok);
                       ("payload", result.payload);
                     ])
          else
            `Null
        in
        let (variables, variables_source) =
          if include_variables then
            match fetch_variables_cached ~file_key ~token with
            | Ok (vars_json, source) -> (resolve_variables vars_json, source)
            | Error err ->
                (match plugin_payload_if_ok plugin_variables with
                 | Some payload -> (resolve_plugin_variables payload, `String "plugin")
                 | None -> (`Assoc [("error", `String err)], `String "error"))
          else
            (`Null, `Null)
        in
        let image_fills =
          if include_image_fills then
            match Figma_effects.Perform.get_file_images ~token ~file_key () with
            | Ok img_json ->
                let images =
                  match member "images" img_json with
                  | Some (`Assoc _ as m) -> m
                  | _ -> `Null
                in
                `Assoc [("images", images)]
            | Error err -> `Assoc [("error", `String err)]
          else
            `Null
        in
        let plugin_snapshot =
          if include_plugin then
            match resolve_plugin_channel () with
            | Error msg -> `Assoc [("error", `String msg)]
            | Ok channel_id ->
                let payload = `Assoc [
                  ("node_id", `String node_id);
                  ("depth", `Int plugin_depth);
                ] in
                let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_node" ~payload in
                (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                 | Error err -> `Assoc [("error", `String err)]
                 | Ok result ->
                     `Assoc [
                       ("channel_id", `String channel_id);
                       ("command_id", `String command_id);
                       ("ok", `Bool result.ok);
                       ("payload", result.payload);
                     ])
          else
            `Null
        in
        (* Early Stop 감지기 생성 *)
        let early_stop_config = Figma_early_stop.{
          target_ssim = target_score;
          plateau_threshold = 0.005;  (* 0.5% *)
          plateau_patience = 3;
          text_ceiling = 0.88;
          max_iterations = max_attempts;
        } in
        let early_stop_detector = Figma_early_stop.create ~config:early_stop_config () in

        let rec loop attempt depth best attempts =
          if attempt > max_attempts then
            (best, attempts, None)
          else
            (* 캐시 옵션: depth와 geometry 포함 *)
            let cache_options = List.filter_map Fun.id [
              Some (sprintf "depth:%d" depth);
              Option.map (sprintf "geometry:%s") geometry;
              Option.map (sprintf "plugin_data:%s") plugin_data;
            ] in
            let cached = Figma_cache.get ~file_key ~node_id ~options:cache_options () in
            let json_result = match cached with
              | Some json ->
                  Printf.eprintf "[FidelityLoop] Cache HIT: depth=%d\n%!" depth;
                  Ok json
              | None ->
                  Printf.eprintf "[FidelityLoop] Cache MISS: depth=%d → API call\n%!" depth;
                  match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id]
                          ?geometry ?plugin_data ~depth () with
                  | Error err -> Error err
                  | Ok json ->
                      Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                      Ok json
            in
            (match json_result with
            | Error err -> (best, (`Assoc [("attempt", `Int attempt); ("error", `String err)]) :: attempts, None)
            | Ok json ->
                let node_lookup =
                  match member "nodes" json with
                  | Some (`Assoc nodes_map) ->
                      (match find_node_entry nodes_map ~node_id with
                       | Some (_node_key, node_entry) ->
                           (match member "document" node_entry with
                            | Some doc -> Some doc
                            | None -> None)
                       | None -> None)
                  | _ -> None
                in
                (match node_lookup with
                 | None ->
                     let entry = `Assoc [
                       ("attempt", `Int attempt);
                       ("depth", `Int depth);
                       ("error", `String ("Node not found: " ^ node_id));
                     ] in
                     (best, entry :: attempts, None)
                 | Some node ->
                     let node_str = Yojson.Safe.to_string node in
                     let dsl_str =
                       match process_json_string ~format node_str with
                       | Ok s -> s
                       | Error msg -> msg
                     in
                     let dsl_json =
                       try Yojson.Safe.from_string dsl_str
                       with _ -> `Null
                     in
                     let (overall, missing_total, sections) =
                       match dsl_json with
                       | `Assoc _ as json ->
                           fidelity_score_of_bundle
                             ~dsl_json:json
                             ~variables
                             ~image_fills
                             ~plugin_snapshot
                             ~include_variables
                             ~include_image_fills
                             ~include_plugin
                       | _ -> (0.0, 0, `Null)
                     in
                     let fidelity = `Assoc [
                       ("overall", `Float overall);
                       ("missing_total", `Int missing_total);
                       ("sections", sections);
                     ] in
                     let best =
                       match best with
                       | None ->
                           let payload = `Assoc [
                             ("depth", `Int depth);
                             ("dsl", `String dsl_str);
                             ("dsl_json", dsl_json);
                             ("node_raw", node);
                             ("fidelity", fidelity);
                           ] in
                           Some (overall, payload)
                       | Some (best_score, _) when overall > best_score ->
                           let payload = `Assoc [
                             ("depth", `Int depth);
                             ("dsl", `String dsl_str);
                             ("dsl_json", dsl_json);
                             ("node_raw", node);
                             ("fidelity", fidelity);
                           ] in
                           Some (overall, payload)
                       | Some _ -> best
                     in
                     (* Early Stop 체크 *)
                     let text_density = Figma_early_stop.calculate_text_density dsl_json in
                     let stop_condition = Figma_early_stop.check early_stop_detector
                       ~current_ssim:overall ~iteration:attempt ~text_density () in
                     let entry_with_stop = `Assoc [
                       ("attempt", `Int attempt);
                       ("depth", `Int depth);
                       ("geometry", match geometry with Some g -> `String g | None -> `Null);
                       ("fidelity", fidelity);
                       ("early_stop", `Assoc [
                         ("should_stop", `Bool stop_condition.should_stop);
                         ("reason", `String stop_condition.message);
                         ("text_density", `Float text_density);
                       ]);
                     ] in
                     if stop_condition.should_stop || depth >= max_depth then
                       (best, entry_with_stop :: attempts, Some stop_condition)
                     else
                       let next_depth = min max_depth (depth + depth_step) in
                       if next_depth = depth then
                         (best, entry_with_stop :: attempts, Some stop_condition)
                       else
                         loop (attempt + 1) next_depth best (entry_with_stop :: attempts)))
        in
        let (best, attempts, final_stop) = loop 1 start_depth None [] in
        let (best_score, best_payload) =
          match best with
          | Some (score, payload) -> (score, payload)
          | None -> (0.0, `Null)
        in
        let early_stop_summary =
          match final_stop with
          | Some cond -> Figma_early_stop.to_json early_stop_detector cond
          | None -> `Assoc [("summary", `String (Figma_early_stop.summary early_stop_detector))]
        in
        let attempt_overall entry =
          match member "fidelity" entry with
          | Some fidelity ->
              (match member "overall" fidelity with
               | Some (`Float f) -> Some f
               | Some (`Int i) -> Some (float_of_int i)
               | _ -> None)
          | None -> None
        in
        let summarize_attempt entry =
          let overall_json =
            match attempt_overall entry with
            | Some f -> `Float f
            | None -> `Null
          in
          let missing_total =
            match member "fidelity" entry with
            | Some fidelity ->
                (match member "missing_total" fidelity with
                 | Some v -> v
                 | None -> `Null)
            | None -> `Null
          in
          `Assoc [
            ("attempt", member "attempt" entry |> Option.value ~default:`Null);
            ("depth", member "depth" entry |> Option.value ~default:`Null);
            ("fidelity", `Assoc [
              ("overall", overall_json);
              ("missing_total", missing_total);
            ]);
            ("early_stop", member "early_stop" entry |> Option.value ~default:`Null);
            ("error", member "error" entry |> Option.value ~default:`Null);
          ]
        in
        let summarize_best payload =
          match payload with
          | `Assoc _ ->
              let overall_json =
                match member "fidelity" payload with
                | Some fidelity ->
                    (match member "overall" fidelity with
                     | Some (`Float f) -> `Float f
                     | Some (`Int i) -> `Float (float_of_int i)
                     | _ -> `Null)
                | None -> `Null
              in
              let missing_total =
                match member "fidelity" payload with
                | Some fidelity ->
                    (match member "missing_total" fidelity with
                     | Some v -> v
                     | None -> `Null)
                | None -> `Null
              in
              `Assoc [
                ("depth", member "depth" payload |> Option.value ~default:`Null);
                ("fidelity", `Assoc [
                  ("overall", overall_json);
                  ("missing_total", missing_total);
                ]);
              ]
          | _ -> `Null
        in
        let attempts_list = List.rev attempts in
        let result = `Assoc [
          ("target_score", `Float target_score);
          ("early_stop", early_stop_summary);
          ("best_score", `Float best_score);
          ("achieved", `Bool (best_score >= target_score));
          ("best", best_payload);
          ("attempts", `List attempts_list);
          ("file_meta", file_meta);
          ("variables", variables);
          ("variables_source", variables_source);
          ("plugin_variables", plugin_variables);
          ("image_fills", image_fills);
          ("plugin_snapshot", plugin_snapshot);
        ] in
        let full_str = Yojson.Safe.pretty_to_string result in
        let full_size = String.length full_str in
        let prefix = Printf.sprintf "fidelity_%s" (sanitize_node_id node_id) in
        let needs_summary = summary_only || full_size > max_inline_bytes in
        if needs_summary then
          let summary_json = `Assoc [
            ("target_score", `Float target_score);
            ("early_stop", early_stop_summary);
            ("best_score", `Float best_score);
            ("achieved", `Bool (best_score >= target_score));
            ("best", summarize_best best_payload);
            ("attempts", `List (List.map summarize_attempt attempts_list));
            ("options", `Assoc [
              ("include_meta", `Bool include_meta);
              ("include_variables", `Bool include_variables);
              ("include_image_fills", `Bool include_image_fills);
              ("include_plugin", `Bool include_plugin);
            ]);
            ("full_result_size_bytes", `Int full_size);
          ] in
          if full_size > max_inline_bytes then
            let filepath = Large_response.save_to_file ~prefix full_str in
            let large_meta = [
              ("status", `String "large_result");
              ("file_path", `String filepath);
              ("size_bytes", `Int full_size);
              ("size_human", `String (Large_response.human_size full_size));
              ("format", `String format);
              ("ttl_seconds", `Int Large_response.response_ttl);
              ("hint", `String "Full result saved to file due to size. Use figma_read_large_result.");
            ] in
            let summary_content = make_text_content (Yojson.Safe.pretty_to_string summary_json) in
            (match summary_content with
             | `Assoc fields -> Ok (`Assoc (fields @ large_meta))
             | _ -> Ok summary_content)
          else
            Ok (make_text_content (Yojson.Safe.pretty_to_string summary_json))
        else
          Ok (Large_response.wrap_string_result ~prefix ~format full_str)
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_image_similarity 핸들러 *)
let handle_image_similarity args : (Yojson.Safe.t, string) result =
  let format = get_string_or "format" "png" args in
  let start_scale = get_float_or "start_scale" 1.0 args in
  let max_scale = get_float_or "max_scale" start_scale args in
  let scale_step = get_float_or "scale_step" 1.0 args in
  let target_ssim = get_float "target_ssim" args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let version = get_string "version" args in
  let save_dir = get_string_or "save_dir" (default_compare_dir ()) args in

  let clamp_scale s = max 0.01 (min 4.0 s) in

  match (get_string "file_key" args, get_string "node_a_id" args, get_string "node_b_id" args, resolve_token args) with
  | (Some file_key, Some node_a_id, Some node_b_id, Some token) ->
      let compare_scale scale =
        match Figma_effects.Perform.get_images ~token ~file_key
                ~node_ids:[node_a_id; node_b_id]
                ~format ~scale ?use_absolute_bounds ?version () with
        | Error err -> Error err
        | Ok json ->
            let images = match member "images" json with
              | Some (`Assoc map) -> map
              | _ -> []
            in
            let url_for id =
              match List.assoc_opt id images with
              | Some (`String url) -> Ok url
              | _ -> Error (Printf.sprintf "Image URL not found: %s" id)
            in
            (match (url_for node_a_id, url_for node_b_id) with
             | (Ok url_a, Ok url_b) ->
                 let path_a = Printf.sprintf "%s/%s/%s__%.2f.%s"
                   save_dir (sanitize_file_key file_key) (sanitize_node_id node_a_id) scale format in
                 let path_b = Printf.sprintf "%s/%s/%s__%.2f.%s"
                   save_dir (sanitize_file_key file_key) (sanitize_node_id node_b_id) scale format in
                 (match Figma_effects.Perform.download_url ~url:url_a ~path:path_a with
                  | Error err -> Error err
                  | Ok saved_a ->
                      (match Figma_effects.Perform.download_url ~url:url_b ~path:path_b with
                       | Error err -> Error err
                       | Ok saved_b ->
                           (match Figma_image_similarity.compare_paths ~path_a:saved_a ~path_b:saved_b with
                            | Error err -> Error err
                            | Ok metrics ->
                                let result = `Assoc [
                                  ("scale", `Float scale);
                                  ("format", `String format);
                                  ("image_a", `String saved_a);
                                  ("image_b", `String saved_b);
                                  ("metrics", `Assoc [
                                    ("ssim", `Float metrics.ssim);
                                    ("psnr", `Float metrics.psnr);
                                    ("mse", `Float metrics.mse);
                                    ("width_a", `Int metrics.width_a);
                                    ("height_a", `Int metrics.height_a);
                                    ("width_b", `Int metrics.width_b);
                                    ("height_b", `Int metrics.height_b);
                                    ("overlap_width", `Int metrics.overlap_width);
                                    ("overlap_height", `Int metrics.overlap_height);
                                  ]);
                                ] in
                                Ok result)))
             | (Error err, _) -> Error err
             | (_, Error err) -> Error err)
      in
      let max_scale = clamp_scale max_scale in
      let start_scale = clamp_scale start_scale in
      let rec loop scale best attempts =
        if scale > max_scale then
          (best, attempts)
        else
          let scale = clamp_scale scale in
          let result = compare_scale scale in
          let attempts = (match result with Ok r -> r | Error err ->
            `Assoc [("scale", `Float scale); ("error", `String err)]) :: attempts
          in
          let best =
            match (best, result) with
            | (None, Ok r) ->
                let ssim = match member "metrics" r with
                  | Some (`Assoc m) -> (match List.assoc_opt "ssim" m with Some (`Float v) -> v | _ -> 0.0)
                  | _ -> 0.0
                in
                Some (ssim, r)
            | (Some (best_score, _), Ok r) ->
                let ssim = match member "metrics" r with
                  | Some (`Assoc m) -> (match List.assoc_opt "ssim" m with Some (`Float v) -> v | _ -> 0.0)
                  | _ -> 0.0
                in
                if ssim > best_score then Some (ssim, r) else best
            | _ -> best
          in
          let should_stop =
            match target_ssim with
            | Some target ->
                (match result with
                 | Ok r ->
                     let ssim = match member "metrics" r with
                       | Some (`Assoc m) -> (match List.assoc_opt "ssim" m with Some (`Float v) -> v | _ -> 0.0)
                       | _ -> 0.0
                     in
                     ssim >= target
                 | Error _ -> false)
            | None -> true
          in
          if should_stop then
            (best, attempts)
          else
            loop (scale +. scale_step) best attempts
      in
      let (best, attempts) = loop start_scale None [] in
      let (best_score, best_payload) =
        match best with
        | Some (score, payload) -> (score, payload)
        | None -> (0.0, `Null)
      in
      let result : Yojson.Safe.t = `Assoc [
        ("file_key", `String file_key);
        ("node_a_id", `String node_a_id);
        ("node_b_id", `String node_b_id);
        ("target_ssim", match target_ssim with Some v -> `Float v | None -> `Null);
        ("best_score", `Float best_score);
        ("best", best_payload);
        ("attempts", `List (List.rev attempts));
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))
  | _ -> Error "Missing required parameters: file_key, node_a_id, node_b_id, token"

(** figma_verify_visual 핸들러 - Visual Feedback Loop *)
let handle_verify_visual args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = resolve_token args in
  let html = get_string "html" args in
  let html_screenshot = get_string "html_screenshot" args in
  let target_ssim = get_float_or "target_ssim" 0.95 args in
  let max_iterations = get_int_positive "max_iterations" 3 args in
  let width = get_int_positive "width" 375 args in
  let height = get_int_positive "height" 812 args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (* 1. Figma에서 노드 PNG 내보내기 *)
      (* Use unique temp paths to avoid cross-run collisions. *)
      let figma_png_path =
        Visual_verifier.temp_file
          ~prefix:(Printf.sprintf "figma_%s_%s" (sanitize_file_key file_key) (sanitize_node_id node_id))
          ~ext:"png"
      in
      (match Figma_effects.Perform.get_images ~token ~file_key
              ~node_ids:[node_id] ~format:"png" ~scale:1.0 ?version () with
       | Error err -> Error (Printf.sprintf "Failed to get Figma image: %s" err)
       | Ok images_json ->
           let url_opt =
             match member "images" images_json with
             | Some (`Assoc map) ->
                 (match List.assoc_opt node_id map with
                  | Some (`String url) -> Some url
                  | _ -> None)
             | _ -> None
           in
           (match url_opt with
            | None -> Error (Printf.sprintf "Image URL not found for node: %s" node_id)
            | Some img_url ->
                (match Figma_effects.Perform.download_url ~url:img_url ~path:figma_png_path with
                 | Error err -> Error (Printf.sprintf "Failed to download Figma image: %s" err)
                 | Ok saved_figma_png ->
                     (* 2. 노드 데이터 가져오기 (HTML 생성 + 텍스트 검증용) *)
                     let parsed_node_opt, html_code =
                       match Figma_effects.Perform.get_nodes ~token ~file_key
                               ~node_ids:[node_id] ~depth:10 ?version () with
                       | Error _ -> (None, match html with Some h -> h | None -> "<html><body><div>Auto-generation failed</div></body></html>")
                       | Ok nodes_json ->
                           match member "nodes" nodes_json with
                           | Some (`Assoc nodes_map) ->
                               (match List.assoc_opt node_id nodes_map with
                                | Some node_data ->
                                    (match member "document" node_data with
                                     | Some doc_json ->
                                         let parsed = Figma_parser.parse_node doc_json in
                                         let generated_html = match parsed with
                                           | Some node -> Figma_codegen.generate_flat_html node
                                           | None -> "<html><body><div>Failed to parse node</div></body></html>"
                                         in
                                         (parsed, match html with Some h -> h | None -> generated_html)
                                     | _ -> (None, match html with Some h -> h | None -> "<html><body><div>No document</div></body></html>"))
                                | _ -> (None, match html with Some h -> h | None -> "<html><body><div>Node not found</div></body></html>"))
                           | _ -> (None, match html with Some h -> h | None -> "<html><body><div>No nodes</div></body></html>")
                     in
                     (* 3. Visual Feedback Loop 실행 (SSIM) *)
                     let result = Visual_verifier.verify_visual
                       ~target_ssim ~max_iterations ~width ~height
                       ?html_png_provided:html_screenshot
                       ~figma_png:saved_figma_png html_code
                     in
                     let result_json = Visual_verifier.result_to_json result in
                     (* 4. 텍스트 정확도 검증 *)
                     let text_verification_json = match parsed_node_opt with
                       | Some dsl_node ->
                           let text_result = Text_verifier.verify_texts ~dsl_node ~html:html_code in
                           Text_verifier.result_to_json text_result
                       | None -> `Assoc [
                           ("error", `String "Could not parse DSL node for text verification");
                           ("passed", `Bool false);
                         ]
                     in
                     (* 5. 종합 PASS/FAIL 결정 *)
                     let ssim_passed = result.Visual_verifier.passed in
                     let text_passed = match text_verification_json with
                       | `Assoc fields -> (match List.assoc_opt "passed" fields with Some (`Bool b) -> b | _ -> false)
                       | _ -> false
                     in
                     let overall_passed = ssim_passed && text_passed in
                     let full_result = `Assoc [
                       ("file_key", `String file_key);
                       ("node_id", `String node_id);
                       ("overall_passed", `Bool overall_passed);
                       ("visual_verification", result_json);
                       ("text_verification", text_verification_json);
                     ] in
                     Ok (make_text_content (Yojson.Safe.pretty_to_string full_result)))))
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_verify_semantic 핸들러 - Semantic-first Verification *)
let handle_verify_semantic args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = resolve_token args in
  let html = get_string "html" args in
  let width = get_int_positive "width" 375 args in
  let height = get_int_positive "height" 812 args in
  let version = get_string "version" args in

  let score_threshold =
    get_float_or "score_threshold" Semantic_verifier.default_config.score_threshold args
  in
  let text_bbox_tol_px =
    get_float_or "text_bbox_tol_px" Semantic_verifier.default_config.text_bbox_tol_px args
  in
  let font_size_tol_px =
    get_float_or "font_size_tol_px" Semantic_verifier.default_config.font_size_tol_px args
  in
  let font_weight_tol =
    get_int_positive "font_weight_tol" Semantic_verifier.default_config.font_weight_tol args
  in
  let text_color_tol_rgb =
    get_float_or "text_color_tol_rgb" Semantic_verifier.default_config.text_color_tol_rgb args
  in

  let config = {
    Semantic_verifier.default_config with
    score_threshold;
    text_bbox_tol_px;
    font_size_tol_px;
    font_weight_tol;
    text_color_tol_rgb;
  } in

  let ( let* ) r f = match r with Ok v -> f v | Error e -> Error e in

  match (file_key, node_id, token, html) with
  | (Some file_key, Some node_id, Some token, Some html_code) ->
      let* nodes_json =
        match Figma_effects.Perform.get_nodes ~token ~file_key
                ~node_ids:[node_id] ~depth:10 ?version () with
        | Ok j -> Ok j
        | Error err -> Error (Printf.sprintf "Failed to get node JSON: %s" err)
      in

      let* dsl_node =
        let parsed_node_opt =
          match member "nodes" nodes_json with
          | Some (`Assoc nodes_map) ->
              (match find_node_entry nodes_map ~node_id with
               | Some (_key, node_data) ->
                   (match member "document" node_data with
                    | Some doc_json -> Figma_parser.parse_node doc_json
                    | _ -> None)
               | None -> None)
          | _ -> None
        in
        match parsed_node_opt with
        | Some n -> Ok n
        | None -> Error "Failed to parse DSL node for semantic verification"
      in

      let* metrics =
        match Html_metrics.extract ~width ~height html_code with
        | Ok m -> Ok m
        | Error e -> Error (Printf.sprintf "Failed to extract HTML metrics: %s" e)
      in

      let* sem = Semantic_verifier.verify ~config ~dsl_node ~html:metrics () in

      let full_result = `Assoc [
        ("file_key", `String file_key);
        ("node_id", `String node_id);
        ("semantic_verification", Semantic_verifier.result_to_json sem);
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string full_result))
  | _ -> Error "Missing required parameters: file_key, node_id, token, html"

(** figma_compare_regions 핸들러 - 영역별 상세 비교 *)
let handle_compare_regions args : (Yojson.Safe.t, string) result =
  let allowed_output_base = "/tmp/figma-evolution" in

  let starts_with ~prefix s =
    let lp = String.length prefix in
    String.length s >= lp && String.sub s 0 lp = prefix
  in

  let validate_output_dir dir =
    let dir = String.trim dir in
    if dir = "" then Error "output_dir cannot be empty"
    else if dir.[0] <> '/' then Error "output_dir must be an absolute path"
    else
      let segments =
        String.split_on_char '/' dir |> List.filter (fun s -> s <> "")
      in
      if List.exists (fun s -> s = "..") segments then
        Error "output_dir must not contain '..'"
      else if dir = allowed_output_base
              || starts_with ~prefix:(allowed_output_base ^ "/") dir then
        Ok dir
      else
        Error (Printf.sprintf "output_dir must be under %s" allowed_output_base)
  in

  let is_safe_region_name name =
    let len = String.length name in
    if len = 0 || len > 64 then false
    else
      let is_ok = function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '.' -> true
        | _ -> false
      in
      let rec loop i =
        if i = len then true
        else if is_ok name.[i] then loop (i + 1)
        else false
      in
      name <> "." && name <> ".." && loop 0
  in

  let output_dir_raw = get_string_or "output_dir" "/tmp/figma-evolution/regions" args in
  match validate_output_dir output_dir_raw with
  | Error e -> Error e
  | Ok output_dir ->
    let generate_diff = get_bool_or "generate_diff" true args in
    let trim = String.trim in
    let split_csv s =
      s
      |> String.split_on_char ','
      |> List.map trim
      |> List.filter (fun x -> x <> "")
    in
    let normalize_dir_prefix path =
      let p = trim path in
      if p = "" then None
      else
        let rp = try Unix.realpath p with exn ->
          Printf.eprintf "[mcp_tools] Warning: realpath failed for '%s': %s, using original\n%!" p (Printexc.to_string exn);
          p
        in
        if rp = "/" then Some rp
        else if String.ends_with ~suffix:"/" rp then Some rp
        else Some (rp ^ "/")
    in
    let is_under_dir ~dir_prefix path =
      if dir_prefix = "/" then true
      else
        let lp = String.length dir_prefix in
        String.length path >= lp && String.sub path 0 lp = dir_prefix
    in
    let roots =
      match Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" with
      | None -> []
      | Some v -> split_csv v
    in
    let max_bytes_default = 50 * 1024 * 1024 in
    let max_bytes =
      match Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" with
      | None -> max_bytes_default
      | Some v ->
          (try int_of_string (trim v) with _ -> max_bytes_default)
    in
    let validate_png_path ~label path =
      if trim path = "" then
        Error (Printf.sprintf "%s path required" label)
      else if not (Sys.file_exists path) then
        Error (Printf.sprintf "%s image not found" label)
      else
        let lower = String.lowercase_ascii path in
        if not (String.ends_with ~suffix:".png" lower) then
          Error (Printf.sprintf "%s must be a .png file" label)
        else
          let st =
            try Ok (Unix.stat path)
            with Unix.Unix_error (e, _, _) ->
              Error (Printf.sprintf "Failed to stat %s: %s" label (Unix.error_message e))
          in
          match st with
          | Error e -> Error e
          | Ok st ->
              if st.Unix.st_kind <> Unix.S_REG then
                Error (Printf.sprintf "%s must be a regular file" label)
              else if st.Unix.st_size > max_bytes then
                Error (Printf.sprintf "%s image too large" label)
              else
                let rp =
                  try Ok (Unix.realpath path)
                  with _ -> Error (Printf.sprintf "Failed to resolve %s path" label)
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
                    else
                      Error (Printf.sprintf "%s path not allowed (set FIGMA_MCP_COMPARE_IMAGE_ROOTS)" label)
    in

    match (get_string "image_a" args, get_string "image_b" args, get_string "regions" args) with
    | (Some image_a, Some image_b, Some regions_json) -> (
        match validate_png_path ~label:"image_a" image_a with
        | Error e -> Error e
        | Ok image_a ->
            match validate_png_path ~label:"image_b" image_b with
            | Error e -> Error e
            | Ok image_b ->
                (* regions JSON 파싱 *)
                let regions_result =
                  try
                    let json = Yojson.Safe.from_string regions_json in
                    match json with
                    | `List items ->
                        let open Yojson.Safe.Util in
                        let rec loop acc = function
                          | [] -> Ok (List.rev acc)
                          | item :: rest ->
                              try
                                let name = item |> member "name" |> to_string in
                                let x = item |> member "x" |> to_int in
                                let y = item |> member "y" |> to_int in
                                let width = item |> member "width" |> to_int in
                                let height = item |> member "height" |> to_int in
                                if not (is_safe_region_name name) then
                                  Error (Printf.sprintf "Invalid region name: %s" name)
                                else if x < 0 || y < 0 || width <= 0 || height <= 0 then
                                  Error (Printf.sprintf "Invalid region bounds for %s" name)
                                else
                                  loop ((name, x, y, width, height) :: acc) rest
                              with exn ->
                                Error (Printf.sprintf "Region field extraction failed: %s. Expected: {name:string, x:int, y:int, width:int, height:int}" (Printexc.to_string exn))
                        in
                        loop [] items
                    | _ -> Error "Invalid regions JSON: expected array of objects [{name, x, y, width, height}, ...]"
                  with exn -> Error (Printf.sprintf "Regions JSON parse error: %s" (Printexc.to_string exn))
                in

                (match regions_result with
                | Error e -> Error e
                | Ok regions ->
                    if regions = [] then
                      Error "Invalid regions JSON format. Expected: [{name, x, y, width, height}, ...]"
                    else begin
                      (* 디렉토리 생성 *)
                      mkdir_p output_dir;

                      (* 각 영역별 SSIM 계산 *)
                      let compare_region (name, x, y, w, h) =
                        let crop_a = Filename.concat output_dir (Printf.sprintf "figma_%s.png" name) in
                        let crop_b = Filename.concat output_dir (Printf.sprintf "html_%s.png" name) in

                        (* ImageMagick으로 영역 crop *)
                        let args_a = [| "magick"; image_a; "-crop"; Printf.sprintf "%dx%d+%d+%d" w h x y; "+repage"; crop_a |] in
                        let args_b = [| "magick"; image_b; "-crop"; Printf.sprintf "%dx%d+%d+%d" w h x y; "+repage"; crop_b |] in
                        (match Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_a with
                         | Error msg -> Printf.eprintf "[visual] magick crop A failed: %s\n%!" msg
                         | Ok _ -> ());
                        (match Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_b with
                         | Error msg -> Printf.eprintf "[visual] magick crop B failed: %s\n%!" msg
                         | Ok _ -> ());

                        (* SSIM 계산 *)
                        let args_ssim = [| "magick"; "compare"; "-metric"; "SSIM"; crop_a; crop_b; "null:" |] in
                        let output =
                          match Safe_exec.run ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_ssim with
                          | Ok out -> out.stderr
                          | Error _ -> ""
                        in

                        (* 결과 파싱: "0.876543 (0.123457)" 형식 *)
                        let ssim =
                          try
                            let re = Str.regexp "(\\([0-9.]+\\))" in
                            if Str.string_match re output 0 then
                              let diff = float_of_string (Str.matched_group 1 output) in
                              (1.0 -. diff) *. 100.0  (* 유사도 = (1 - 차이율) * 100 *)
                            else
                              let parts = String.split_on_char ' ' output in
                              match parts with
                              | first :: _ -> float_of_string first *. 100.0
                              | _ -> 0.0
                          with exn ->
                            Printf.eprintf "[mcp_tools] Warning: SSIM parse failed for output '%s': %s\n%!" output (Printexc.to_string exn);
                            0.0
                        in

                        (* 차이 이미지 생성 *)
                        let diff_image =
                          if generate_diff then begin
                            let diff_path = Filename.concat output_dir (Printf.sprintf "diff_%s.png" name) in
                            let args_diff = [| "magick"; "compare"; crop_a; crop_b; diff_path |] in
                            (match Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_diff with
                             | Error msg -> Printf.eprintf "[visual] magick compare failed: %s\n%!" msg
                             | Ok _ -> ());
                            Some diff_path
                          end else None
                        in

                        `Assoc [
                          ("name", `String name);
                          ("region", `Assoc [
                            ("x", `Int x);
                            ("y", `Int y);
                            ("width", `Int w);
                            ("height", `Int h);
                          ]);
                          ("ssim_percent", `Float ssim);
                          ("status", `String (if ssim >= 90.0 then "good" else if ssim >= 75.0 then "acceptable" else "needs_work"));
                          ("figma_crop", `String crop_a);
                          ("html_crop", `String crop_b);
                          ("diff_image", match diff_image with Some p -> `String p | None -> `Null);
                        ]
                      in

                      let results = List.map compare_region regions in

                      (* 전체 통계 *)
                      let ssims = List.filter_map (fun r ->
                        match r with
                        | `Assoc items ->
                            (match List.assoc_opt "ssim_percent" items with
                            | Some (`Float f) -> Some f
                            | _ -> None)
                        | _ -> None
                      ) results in
                      let avg_ssim = if ssims = [] then 0.0 else
                        (List.fold_left (+.) 0.0 ssims) /. (float_of_int (List.length ssims)) in
                      let min_ssim = if ssims = [] then 0.0 else List.fold_left min 100.0 ssims in
                      let max_ssim = if ssims = [] then 0.0 else List.fold_left max 0.0 ssims in

                      let summary = `Assoc [
                        ("total_regions", `Int (List.length regions));
                        ("average_ssim", `Float avg_ssim);
                        ("min_ssim", `Float min_ssim);
                        ("max_ssim", `Float max_ssim);
                        ("overall_status", `String (
                          if min_ssim >= 90.0 then "excellent"
                          else if avg_ssim >= 85.0 then "good"
                          else if avg_ssim >= 70.0 then "acceptable"
                          else "needs_improvement"
                        ));
                      ] in

                      let result = `Assoc [
                        ("summary", summary);
                        ("regions", `List results);
                        ("output_dir", `String output_dir);
                      ] in
                      Ok (make_text_content (Yojson.Safe.pretty_to_string result))
                    end)
      )

    | _ -> Error "Missing required parameters: image_a, image_b, regions"

(** figma_evolution_report 핸들러 - 진화 과정 리포트 생성 *)
let handle_evolution_report args : (Yojson.Safe.t, string) result =
  let run_dir = get_string "run_dir" args in
  let generate_image = get_bool_or "generate_image" true args in

  (* 최근 evolution 디렉토리 목록 *)
  let list_recent_runs () =
    let base = "/tmp/figma-evolution" in
    if not (Sys.file_exists base) then []
    else
      let entries = Sys.readdir base |> Array.to_list in
      let runs =
        entries
        |> List.filter (fun name -> String.starts_with ~prefix:"run_" name)
        |> List.map (fun name ->
          let path = Filename.concat base name in
          let mtime =
            try (Unix.stat path).Unix.st_mtime with Unix.Unix_error _ -> 0.0
          in
          (mtime, path)
        )
        |> List.sort (fun (a, _) (b, _) -> compare b a)
        |> List.map snd
      in
      let rec take n xs = match (n, xs) with | (0, _) -> [] | (_, []) -> [] | (k, x :: tl) -> x :: take (k - 1) tl in
      take 10 runs
  in

  match run_dir with
  | None ->
      (* run_dir 없으면 최근 실행 목록 반환 *)
      let runs = list_recent_runs () in
      let runs_json = `List (List.map (fun r -> `String r) runs) in
      let result = `Assoc [
        ("recent_runs", runs_json);
        ("count", `Int (List.length runs));
        ("hint", `String "특정 run에 대한 리포트를 보려면 run_dir 파라미터를 지정하세요");
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))
  | Some dir ->
      if not (Sys.file_exists dir) then
        Error (sprintf "Evolution directory not found: %s" dir)
      else
        (* 해당 디렉토리의 진화 과정 분석 *)
        let figma_png = Filename.concat dir "figma_original.png" in
        let html_dir = Filename.concat dir "html" in

        (* step 파일들 읽기 *)
        let steps =
          if Sys.file_exists html_dir then
            let files = Sys.readdir html_dir |> Array.to_list in
            List.filter (fun f -> Filename.check_suffix f ".html") files
            |> List.sort compare
          else []
        in

        (* PNG 파일들 읽기 *)
        let pngs =
          Sys.readdir dir |> Array.to_list
          |> List.filter (fun f -> Filename.check_suffix f "_render.png")
          |> List.sort compare
        in

        (* 비교 이미지 생성 *)
        let comparison_image =
          if generate_image && List.length pngs > 0 then
            let last_png = Filename.concat dir (List.hd (List.rev pngs)) in
            let output = Filename.concat dir "evolution_comparison.png" in
            if Sys.file_exists figma_png && Sys.file_exists last_png then
              let args = [| "montage"; figma_png; last_png; "-tile"; "2x1"; "-geometry"; "+5+5"; "-background"; "#1a1a1a"; output |] in
              (match Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "montage" args with
               | Error msg -> Printf.eprintf "[visual] montage failed: %s\n%!" msg
               | Ok _ -> ());
              if Sys.file_exists output then Some output else None
            else None
          else None
        in

        let result = `Assoc [
          ("run_dir", `String dir);
          ("figma_original", `String figma_png);
          ("html_steps", `List (List.map (fun f -> `String (Filename.concat html_dir f)) steps));
          ("png_renders", `List (List.map (fun f -> `String (Filename.concat dir f)) pngs));
          ("step_count", `Int (List.length steps));
          ("comparison_image", match comparison_image with Some p -> `String p | None -> `Null);
          ("summary", `String (sprintf "Evolution with %d steps. Final PNG: %s"
            (List.length steps)
            (if List.length pngs > 0 then List.hd (List.rev pngs) else "none")));
        ] in
        Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_compare_elements 핸들러 - 색상/박스 확장 메트릭 비교 *)
let handle_compare_elements args : (Yojson.Safe.t, string) result =
  let compare_type = get_string "type" args in
  let color1 = get_string "color1" args in
  let color2 = get_string "color2" args in
  let box1 = get_string "box1" args in
  let box2 = get_string "box2" args in

  (* 색상 파싱 헬퍼 *)
  let parse_color str =
    let str = String.trim str in
    if String.length str > 0 && str.[0] = '#' then
      (* Hex format: #RRGGBB *)
      let hex = String.sub str 1 (String.length str - 1) in
      let r = int_of_string ("0x" ^ String.sub hex 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex 4 2) in
      Some (float_of_int r /. 255.0, float_of_int g /. 255.0, float_of_int b /. 255.0)
    else if String.length str >= 4 && String.sub str 0 3 = "rgb" then
      (* RGB format: rgb(r,g,b) *)
      let re = Str.regexp "rgb(\\([0-9]+\\),[ ]*\\([0-9]+\\),[ ]*\\([0-9]+\\))" in
      if Str.string_match re str 0 then
        let r = int_of_string (Str.matched_group 1 str) in
        let g = int_of_string (Str.matched_group 2 str) in
        let b = int_of_string (Str.matched_group 3 str) in
        Some (float_of_int r /. 255.0, float_of_int g /. 255.0, float_of_int b /. 255.0)
      else None
    else None
  in

  (* 박스 파싱 헬퍼: "x,y,w,h" *)
  let parse_box str =
    match String.split_on_char ',' str |> List.map String.trim with
    | [x; y; w; h] ->
        (try Some (float_of_string x, float_of_string y, float_of_string w, float_of_string h)
         with _ -> None)
    | _ -> None
  in

  match compare_type with
  | Some "color" ->
      (match (color1, color2) with
       | (Some c1, Some c2) ->
           (match (parse_color c1, parse_color c2) with
            | (Some rgb1, Some rgb2) ->
                let metrics = Figma_similarity.compute_extended_color_metrics rgb1 rgb2 in
                let result = `Assoc [
                  ("type", `String "color");
                  ("color1", `String c1);
                  ("color2", `String c2);
                  ("oklab_distance", `Float metrics.oklab_distance);
                  ("oklab_similarity", `Float metrics.oklab_similarity);
                  ("ciede2000_distance", `Float metrics.ciede2000_distance);
                  ("ciede2000_similarity", `Float metrics.ciede2000_similarity);
                  ("rgb_euclidean", `Float metrics.rgb_euclidean);
                  ("formatted", `String (Figma_similarity.extended_color_to_string metrics));
                ] in
                Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | _ -> Error "Invalid color format. Use #RRGGBB or rgb(r,g,b)")
       | _ -> Error "Missing color1 or color2 for color comparison")

  | Some "box" ->
      (match (box1, box2) with
       | (Some b1, Some b2) ->
           (match (parse_box b1, parse_box b2) with
            | (Some bbox1, Some bbox2) ->
                let metrics = Figma_similarity.compute_extended_box_metrics bbox1 bbox2 in
                let result = `Assoc [
                  ("type", `String "box");
                  ("box1", `String b1);
                  ("box2", `String b2);
                  ("iou_value", `Float metrics.iou_value);
                  ("giou_value", `Float metrics.giou_value);
                  ("diou_value", `Float metrics.diou_value);
                  ("iou_similarity", `Float metrics.iou_similarity);
                  ("giou_similarity", `Float metrics.giou_similarity);
                  ("diou_similarity", `Float metrics.diou_similarity);
                  ("center_distance", `Float metrics.center_distance);
                  ("formatted", `String (Figma_similarity.extended_box_to_string metrics));
                ] in
                Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | _ -> Error "Invalid box format. Use x,y,w,h")
       | _ -> Error "Missing box1 or box2 for box comparison")

  | Some "full" ->
      let color_result =
        match (color1, color2) with
        | (Some c1, Some c2) ->
            (match (parse_color c1, parse_color c2) with
             | (Some rgb1, Some rgb2) ->
                 let m = Figma_similarity.compute_extended_color_metrics rgb1 rgb2 in
                 Some (`Assoc [
                   ("color1", `String c1);
                   ("color2", `String c2);
                   ("oklab_similarity", `Float m.oklab_similarity);
                   ("ciede2000_similarity", `Float m.ciede2000_similarity);
                   ("formatted", `String (Figma_similarity.extended_color_to_string m));
                 ])
             | _ -> None)
        | _ -> None
      in
      let box_result =
        match (box1, box2) with
        | (Some b1, Some b2) ->
            (match (parse_box b1, parse_box b2) with
             | (Some bbox1, Some bbox2) ->
                 let m = Figma_similarity.compute_extended_box_metrics bbox1 bbox2 in
                 Some (`Assoc [
                   ("box1", `String b1);
                   ("box2", `String b2);
                   ("iou_similarity", `Float m.iou_similarity);
                   ("giou_similarity", `Float m.giou_similarity);
                   ("diou_similarity", `Float m.diou_similarity);
                   ("formatted", `String (Figma_similarity.extended_box_to_string m));
                 ])
             | _ -> None)
        | _ -> None
      in
      let result = `Assoc [
        ("type", `String "full");
        ("color", match color_result with Some r -> r | None -> `Null);
        ("box", match box_result with Some r -> r | None -> `Null);
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))

  | _ -> Error "Invalid type. Use 'color', 'box', or 'full'"

(** figma_compare 핸들러 - P1.1: 통합 비교 도구 *)
let handle_compare args : (Yojson.Safe.t, string) result =
  let mode = get_string_or "mode" "general" args in

  (* P1.1: Mode-based dispatch to appropriate handler *)
  match mode with
  | "regions" -> handle_compare_regions args
  | "elements" -> handle_compare_elements args
  | "evolution" -> handle_evolution_report args
  | "batch" | "general" | _ ->
      (* Original general/batch logic *)
      let file_key = get_string "file_key" args in
      let token = resolve_token args in
      let node_a_id = get_string "node_a_id" args in
      let node_b_id = get_string "node_b_id" args in
      let web_prefix = get_string_or "web_prefix" "Web" args in
      let mobile_prefix = get_string_or "mobile_prefix" "Mobile" args in

      match file_key, token with
      | Some file_key, Some token ->
          (match Figma_effects.Perform.get_file ~token ~file_key () with
           | Ok file_data ->
               (match Yojson.Safe.Util.member "document" file_data with
                | `Null -> Error "Document not found"
                | doc_json ->
                    (match Figma_parser.parse_node doc_json with
                     | Some root ->
                         let all_nodes = Figma_query.collect_nodes ~max_depth:None root in

                         if mode = "batch" then begin
                           (* Batch 모드: Web/Mobile 이름 매칭 *)
                           let web_nodes = List.filter (fun n ->
                             String.length n.Figma_types.name >= String.length web_prefix &&
                             String.sub (String.lowercase_ascii n.Figma_types.name) 0 (String.length web_prefix) =
                             String.lowercase_ascii web_prefix
                           ) all_nodes in
                           let mobile_nodes = List.filter (fun n ->
                             String.length n.Figma_types.name >= String.length mobile_prefix &&
                             String.sub (String.lowercase_ascii n.Figma_types.name) 0 (String.length mobile_prefix) =
                             String.lowercase_ascii mobile_prefix
                           ) all_nodes in

                           let (results, total, avg_sim, critical, major) =
                             Figma_compare.compare_web_mobile ~web_nodes ~mobile_nodes
                           in

                           let summary = Printf.sprintf
                             "=== Web/Mobile 일관성 검사 결과 ===\n매칭된 쌍: %d개\n평균 유사도: %.0f%%\nCritical 차이: %d개\nMajor 차이: %d개\n\n"
                             total (avg_sim *. 100.) critical major
                           in
                           let details = String.concat "\n---\n"
                             (List.map Figma_compare.result_to_string results)
                           in
                           Ok (make_text_content (summary ^ details))
                         end
                         else begin
                           (* General 모드: 특정 노드 쌍 비교 *)
                           match node_a_id, node_b_id with
                           | Some id_a, Some id_b ->
                               let find_node id = List.find_opt (fun n -> n.Figma_types.id = id) all_nodes in
                               (match find_node id_a, find_node id_b with
                                | Some node_a, Some node_b ->
                                    let result = Figma_compare.compare_nodes node_a node_b in
                                    Ok (make_text_content (Figma_compare.result_to_string result))
                                | None, _ -> Error (Printf.sprintf "Node A not found: %s" id_a)
                                | _, None -> Error (Printf.sprintf "Node B not found: %s" id_b))
                           | _ -> Error "General mode requires node_a_id and node_b_id"
                         end
                     | None -> Error "Failed to parse document"))
           | Error err -> Error err)
      | _ -> Error "Missing required parameters: file_key, token"

