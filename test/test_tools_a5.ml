(** Coverage A5 push: mcp_tools.ml handler functions.
    Targets 270 uncovered points across:
    - handle_parse_url (pure)
    - handle_get_me (effect)
    - handle_list_projects (effect)
    - handle_list_files (effect)
    - handle_cache_stats (pure)
    - handle_cache_invalidate (pure)
    - handle_doctor (pure, system-dependent)
    - handle_read_large_result (pure, file I/O)
    - handle_codegen_sync (pure)
    - handle_category (pure dispatch)
    - handle_get_variables (effect)
    - handle_query (effect)
    - handle_search (effect)
    - handle_tree (effect)
    - handle_stats (effect)
    - handle_export_tokens (effect)
    - handle_team_tree (effect)
    - handle_export_team (effect)
*)

open Alcotest

(** Extract JSON from make_text_content wrapper:
    {"content":[{"type":"text","text":"<json-string>"}]} → parsed JSON *)
let unwrap_text_content json =
  let module U = Yojson.Safe.Util in
  match U.member "content" json with
  | `List [`Assoc _ as item] ->
    Yojson.Safe.from_string (U.to_string (U.member "text" item))
  | _ -> json

(* ============================================================
   Group 1: handle_parse_url — pure URL parsing
   ============================================================ *)

let test_parse_url_design_url () =
  let args = `Assoc [("url", `String "https://www.figma.com/design/ABC123/My-File?node-id=1:2")] in
  match Mcp_tools.handle_parse_url args with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "has file_key" true (String.length text > 0);
    check bool "contains ABC123" true
      (try let _ = Str.search_forward (Str.regexp_string "ABC123") text 0 in true with Not_found -> false);
    check bool "contains node_id" true
      (try let _ = Str.search_forward (Str.regexp_string "1:2") text 0 in true with Not_found -> false)
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_parse_url_file_url () =
  let args = `Assoc [("url", `String "https://www.figma.com/file/XYZ789/Some-File")] in
  match Mcp_tools.handle_parse_url args with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "contains XYZ789" true
      (try let _ = Str.search_forward (Str.regexp_string "XYZ789") text 0 in true with Not_found -> false)
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_parse_url_team_url () =
  let args = `Assoc [("url", `String "https://www.figma.com/files/team/TEAM42/project/PROJ99")] in
  match Mcp_tools.handle_parse_url args with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "contains TEAM42" true
      (try let _ = Str.search_forward (Str.regexp_string "TEAM42") text 0 in true with Not_found -> false);
    check bool "contains PROJ99" true
      (try let _ = Str.search_forward (Str.regexp_string "PROJ99") text 0 in true with Not_found -> false)
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_parse_url_proto_url () =
  let args = `Assoc [("url", `String "https://www.figma.com/proto/PROTO1/Prototype?node-id=5:10")] in
  match Mcp_tools.handle_parse_url args with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "contains PROTO1" true
      (try let _ = Str.search_forward (Str.regexp_string "PROTO1") text 0 in true with Not_found -> false);
    check bool "contains 5:10" true
      (try let _ = Str.search_forward (Str.regexp_string "5:10") text 0 in true with Not_found -> false)
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_parse_url_invalid_url () =
  let args = `Assoc [("url", `String "https://example.com/not-figma")] in
  match Mcp_tools.handle_parse_url args with
  | Ok json ->
    (* Still returns Ok with (none) for all fields *)
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "has (none)" true
      (try let _ = Str.search_forward (Str.regexp_string "(none)") text 0 in true with Not_found -> false)
  | Error _ -> fail "Should return Ok with (none) fields"

let test_parse_url_missing_url () =
  let args = `Assoc [] in
  match Mcp_tools.handle_parse_url args with
  | Error msg ->
    check bool "missing url error" true
      (try let _ = Str.search_forward (Str.regexp_string "url") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error for missing url"

let test_parse_url_no_node_id () =
  let args = `Assoc [("url", `String "https://www.figma.com/design/KEY1/File")] in
  match Mcp_tools.handle_parse_url args with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "node_id is (none)" true
      (try let _ = Str.search_forward (Str.regexp_string "node_id: (none)") text 0 in true with Not_found -> false)
  | Error msg -> fail msg

(* ============================================================
   Group 2: handle_cache_stats and handle_cache_invalidate — pure
   ============================================================ *)

let test_cache_stats () =
  match Mcp_tools.handle_cache_stats `Null with
  | Ok json ->
    let stats = unwrap_text_content json in
    (match stats with
     | `Assoc fields ->
       check bool "has l1_entries" true (List.mem_assoc "l1_entries" fields)
     | _ -> ())
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_cache_invalidate_all () =
  match Mcp_tools.handle_cache_invalidate (`Assoc []) with
  | Ok json ->
    let result = unwrap_text_content json in
    let status = Yojson.Safe.Util.(result |> member "status" |> to_string) in
    let message = Yojson.Safe.Util.(result |> member "message" |> to_string) in
    check string "status ok" "ok" status;
    check bool "all invalidated" true
      (try let _ = Str.search_forward (Str.regexp_string "All") message 0 in true with Not_found -> false)
  | Error msg -> fail msg

let test_cache_invalidate_file_key () =
  match Mcp_tools.handle_cache_invalidate (`Assoc [("file_key", `String "test-file-key")]) with
  | Ok json ->
    let result = unwrap_text_content json in
    let message = Yojson.Safe.Util.(result |> member "message" |> to_string) in
    check bool "contains file key" true
      (try let _ = Str.search_forward (Str.regexp_string "test-file-key") message 0 in true with Not_found -> false)
  | Error msg -> fail msg

let test_cache_invalidate_file_and_node () =
  match Mcp_tools.handle_cache_invalidate (`Assoc [("file_key", `String "fk1"); ("node_id", `String "1:2")]) with
  | Ok json ->
    let result = unwrap_text_content json in
    let message = Yojson.Safe.Util.(result |> member "message" |> to_string) in
    check bool "contains fk1/1:2" true
      (try let _ = Str.search_forward (Str.regexp_string "fk1/1:2") message 0 in true with Not_found -> false)
  | Error msg -> fail msg

(* ============================================================
   Group 3: handle_doctor — system checks (results vary)
   ============================================================ *)

let test_doctor () =
  match Mcp_tools.handle_doctor `Null with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    (* Doctor returns JSON with status, checks, hints *)
    let parsed = Yojson.Safe.from_string text in
    let status = Yojson.Safe.Util.(parsed |> member "status" |> to_string) in
    check bool "status is ok or needs_attention" true
      (status = "ok" || status = "needs_attention");
    let checks = Yojson.Safe.Util.(parsed |> member "checks" |> to_list) in
    check bool "has checks" true (List.length checks > 0);
    let hints = Yojson.Safe.Util.(parsed |> member "hints" |> to_list) in
    check bool "has hints" true (List.length hints > 0)
  | Error msg -> fail msg

(* ============================================================
   Group 4: handle_read_large_result — file I/O
   ============================================================ *)

let test_read_large_result_missing_path () =
  match Mcp_tools.handle_read_large_result (`Assoc []) with
  | Error msg ->
    check bool "missing file_path" true
      (try let _ = Str.search_forward (Str.regexp_string "file_path") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_read_large_result_outside_dir () =
  match Mcp_tools.handle_read_large_result (`Assoc [("file_path", `String "/etc/passwd")]) with
  | Error msg ->
    check bool "must be under storage dir" true (String.length msg > 0)
  | Ok _ -> fail "Expected error for path outside storage dir"

let test_read_large_result_nonexistent () =
  (* Create a path under the storage dir that doesn't exist *)
  let storage_dir = Large_response.storage_dir in
  let fake_path = Filename.concat storage_dir "nonexistent-file-12345.txt" in
  match Mcp_tools.handle_read_large_result (`Assoc [("file_path", `String fake_path)]) with
  | Error msg ->
    check bool "file not found or path issue" true (String.length msg > 0)
  | Ok _ -> fail "Expected error"

let test_read_large_result_valid_file () =
  (* Create a temporary file in the storage dir *)
  let storage_dir = Large_response.storage_dir in
  (try Unix.mkdir storage_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat storage_dir "test_tools_a5_tmp.txt" in
  let content = "Hello, this is test content for read_large_result." in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  (match Mcp_tools.handle_read_large_result (`Assoc [("file_path", `String path)]) with
   | Ok json ->
     let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
     let parsed = Yojson.Safe.from_string text in
     let chunk = Yojson.Safe.Util.(parsed |> member "chunk" |> to_string) in
     check string "content matches" content chunk;
     let eof = Yojson.Safe.Util.(parsed |> member "eof" |> to_bool) in
     check bool "eof true for small file" true eof
   | Error msg -> fail msg);
  Sys.remove path

let test_read_large_result_with_offset () =
  let storage_dir = Large_response.storage_dir in
  (try Unix.mkdir storage_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat storage_dir "test_tools_a5_offset.txt" in
  let content = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  (match Mcp_tools.handle_read_large_result (`Assoc [
    ("file_path", `String path);
    ("offset", `Int 10);
    ("limit", `Int 5);
  ]) with
   | Ok json ->
     let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
     let parsed = Yojson.Safe.from_string text in
     let chunk = Yojson.Safe.Util.(parsed |> member "chunk" |> to_string) in
     check string "offset content" "KLMNO" chunk;
     let read_bytes = Yojson.Safe.Util.(parsed |> member "read_bytes" |> to_int) in
     check int "read 5 bytes" 5 read_bytes
   | Error msg -> fail msg);
  Sys.remove path

let test_read_large_result_offset_beyond_eof () =
  let storage_dir = Large_response.storage_dir in
  (try Unix.mkdir storage_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat storage_dir "test_tools_a5_eof.txt" in
  let oc = open_out path in
  output_string oc "short";
  close_out oc;
  (match Mcp_tools.handle_read_large_result (`Assoc [
    ("file_path", `String path);
    ("offset", `Int 999);
  ]) with
   | Error msg ->
     check bool "beyond eof" true
       (try let _ = Str.search_forward (Str.regexp_string "beyond EOF") msg 0 in true with Not_found -> false)
   | Ok _ -> fail "Expected error for offset beyond EOF");
  Sys.remove path

let test_read_large_result_negative_limit () =
  let storage_dir = Large_response.storage_dir in
  (try Unix.mkdir storage_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat storage_dir "test_tools_a5_neglim.txt" in
  let oc = open_out path in
  output_string oc "test data";
  close_out oc;
  (match Mcp_tools.handle_read_large_result (`Assoc [
    ("file_path", `String path);
    ("limit", `Int (-5));
  ]) with
   | Ok json ->
     (* negative limit => defaults to 20000 *)
     let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
     let parsed = Yojson.Safe.from_string text in
     let limit_used = Yojson.Safe.Util.(parsed |> member "limit" |> to_int) in
     check int "defaults to 20000" 20000 limit_used
   | Error msg -> fail msg);
  Sys.remove path

(* ============================================================
   Group 5: handle_codegen_sync — pure JSON processing
   ============================================================ *)

let test_codegen_sync_missing_json () =
  match Mcp_tools.handle_codegen_sync (`Assoc []) with
  | Error msg ->
    check bool "missing json" true
      (try let _ = Str.search_forward (Str.regexp_string "json") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_codegen_sync_invalid_json () =
  match Mcp_tools.handle_codegen_sync (`Assoc [("json", `String "not-json{")]) with
  | Error msg ->
    check bool "parse error" true (String.length msg > 0)
  | Ok _ -> fail "Expected error for invalid JSON"

let test_codegen_sync_raw_format () =
  let json_obj = `Assoc [("type", `String "FRAME"); ("name", `String "Test")] in
  let args = `Assoc [
    ("json", `String (Yojson.Safe.to_string json_obj));
    ("format", `String "raw");
  ] in
  match Mcp_tools.handle_codegen_sync args with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    check bool "contains type" true
      (try let _ = Str.search_forward (Str.regexp_string "FRAME") text 0 in true with Not_found -> false)
  | Error msg -> fail msg

let test_codegen_sync_fidelity_format () =
  let json_obj = `Assoc [
    ("type", `String "FRAME");
    ("name", `String "Test");
    ("children", `List []);
  ] in
  let args = `Assoc [
    ("json", `String (Yojson.Safe.to_string json_obj));
    ("format", `String "fidelity");
  ] in
  match Mcp_tools.handle_codegen_sync args with
  | Ok _ -> () (* fidelity format produces some DSL output *)
  | Error msg -> fail msg

let test_codegen_sync_html_format () =
  let json_obj = `Assoc [
    ("type", `String "FRAME");
    ("name", `String "HtmlTest");
    ("children", `List []);
  ] in
  let args = `Assoc [
    ("json", `String (Yojson.Safe.to_string json_obj));
    ("format", `String "html");
  ] in
  match Mcp_tools.handle_codegen_sync args with
  | Ok _ -> () (* html format produces output or fallback *)
  | Error msg -> fail msg

let test_codegen_sync_default_format () =
  let json_obj = `Assoc [("type", `String "RECTANGLE"); ("name", `String "Rect")] in
  let args = `Assoc [
    ("json", `String (Yojson.Safe.to_string json_obj));
  ] in
  match Mcp_tools.handle_codegen_sync args with
  | Ok _ -> () (* default format is fidelity *)
  | Error msg -> fail msg

(* ============================================================
   Group 6: handle_category — dispatch logic
   ============================================================ *)

let test_category_list_core () =
  match Mcp_tools.handle_category "core" (`Assoc [("mode", `String "list")]) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let cat = Yojson.Safe.Util.(parsed |> member "category" |> to_string) in
    check string "category name" "core" cat;
    let tools = Yojson.Safe.Util.(parsed |> member "tools" |> to_list) in
    check bool "has tools" true (List.length tools > 0)
  | Error msg -> fail msg

let test_category_list_unknown () =
  match Mcp_tools.handle_category "nonexistent_cat" (`Assoc [("mode", `String "list")]) with
  | Error msg ->
    check bool "unknown category" true
      (try let _ = Str.search_forward (Str.regexp_string "Unknown category") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error for unknown category"

let test_category_list_visual () =
  match Mcp_tools.handle_category "visual" (`Assoc []) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let cat = Yojson.Safe.Util.(parsed |> member "category" |> to_string) in
    check string "visual category" "visual" cat
  | Error msg -> fail msg

let test_category_describe_missing_tool () =
  match Mcp_tools.handle_category "core" (`Assoc [("mode", `String "describe")]) with
  | Error msg ->
    check bool "missing tool" true
      (try let _ = Str.search_forward (Str.regexp_string "tool") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_category_describe_tool_not_in_category () =
  match Mcp_tools.handle_category "core" (`Assoc [
    ("mode", `String "describe");
    ("tool", `String "nonexistent_tool");
  ]) with
  | Error msg ->
    check bool "tool not found" true
      (try let _ = Str.search_forward (Str.regexp_string "not found") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_category_describe_valid_tool () =
  match Mcp_tools.handle_category "core" (`Assoc [
    ("mode", `String "describe");
    ("tool", `String "get_file");
  ]) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let name = Yojson.Safe.Util.(parsed |> member "name" |> to_string) in
    check string "tool name" "get_file" name
  | Error msg -> fail msg

let test_category_call_missing_tool () =
  match Mcp_tools.handle_category "core" (`Assoc [
    ("mode", `String "call");
  ]) with
  | Error msg ->
    check bool "missing tool" true
      (try let _ = Str.search_forward (Str.regexp_string "tool") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_category_call_tool_not_found () =
  match Mcp_tools.handle_category "core" (`Assoc [
    ("mode", `String "call");
    ("tool", `String "nonexistent_tool");
    ("args", `Assoc []);
  ]) with
  | Error msg ->
    check bool "not found" true
      (try let _ = Str.search_forward (Str.regexp_string "not found") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_category_invalid_mode () =
  (* Invalid mode now returns Error instead of raising *)
  match Mcp_tools.handle_category "core" (`Assoc [
    ("mode", `String "invalid_mode");
  ]) with
  | Error msg ->
    check bool "invalid mode msg" true
      (try let _ = Str.search_forward (Str.regexp_string "Invalid mode") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "expected Error for invalid mode"

let test_category_auto_list () =
  (* No tool, no args => auto-list *)
  match Mcp_tools.handle_category "team" (`Assoc []) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let cat = Yojson.Safe.Util.(parsed |> member "category" |> to_string) in
    check string "auto-list team" "team" cat
  | Error msg -> fail msg

let test_category_auto_describe () =
  (* tool present, no args => auto-describe *)
  match Mcp_tools.handle_category "core" (`Assoc [
    ("tool", `String "parse_url");
  ]) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let name = Yojson.Safe.Util.(parsed |> member "name" |> to_string) in
    check string "auto-describe parse_url" "parse_url" name
  | Error msg -> fail msg

let test_category_call_missing_args () =
  (* tool present with args => call, but args is missing required fields *)
  match Mcp_tools.handle_category "core" (`Assoc [
    ("mode", `String "call");
    ("tool", `String "parse_url");
  ]) with
  | Error msg ->
    check bool "missing args" true
      (try let _ = Str.search_forward (Str.regexp_string "args") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error for missing args"

let test_category_list_components () =
  match Mcp_tools.handle_category "components" (`Assoc [("mode", `String "list")]) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let cat = Yojson.Safe.Util.(parsed |> member "category" |> to_string) in
    check string "components category" "components" cat
  | Error msg -> fail msg

let test_category_list_export () =
  match Mcp_tools.handle_category "export" (`Assoc [("mode", `String "list")]) with
  | Ok json ->
    let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
    let parsed = Yojson.Safe.from_string text in
    let cat = Yojson.Safe.Util.(parsed |> member "category" |> to_string) in
    check string "export category" "export" cat
  | Error msg -> fail msg

(* ============================================================
   Group 7: Effect-dependent handlers via run_with_mock
   ============================================================ *)

(* Save/restore FIGMA_TOKEN to avoid leaking state *)
let with_figma_token token f =
  let old = Sys.getenv_opt "FIGMA_TOKEN" in
  Unix.putenv "FIGMA_TOKEN" token;
  Fun.protect ~finally:(fun () ->
    match old with
    | Some v -> Unix.putenv "FIGMA_TOKEN" v
    | None ->
      (* Clear env var by setting empty; OCaml has no unsetenv *)
      Unix.putenv "FIGMA_TOKEN" ""
  ) f

let test_get_me_ok () =
  let store = Figma_effects.create_mock_store () in
  store.me := Some (`Assoc [
    ("id", `String "user-123");
    ("email", `String "test@example.com");
    ("handle", `String "testuser");
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_get_me (`Assoc []) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "contains user-123" true
          (try let _ = Str.search_forward (Str.regexp_string "user-123") text 0 in true with Not_found -> false);
        check bool "contains testuser" true
          (try let _ = Str.search_forward (Str.regexp_string "testuser") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_get_me_no_token () =
  let old = Sys.getenv_opt "FIGMA_TOKEN" in
  Unix.putenv "FIGMA_TOKEN" "";
  Fun.protect ~finally:(fun () ->
    match old with
    | Some v -> Unix.putenv "FIGMA_TOKEN" v
    | None -> Unix.putenv "FIGMA_TOKEN" ""
  ) (fun () ->
    match Mcp_tools.handle_get_me (`Assoc []) with
    | Error msg ->
      check bool "missing token" true
        (try let _ = Str.search_forward (Str.regexp_string "token") msg 0 in true with Not_found -> false)
    | Ok _ -> fail "Expected error"
  )

let test_get_me_api_error () =
  let store = Figma_effects.create_mock_store () in
  (* me not set => Perform.get_me returns Error *)
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_get_me (`Assoc []) with
      | Error _ -> ()
      | Ok _ -> fail "Expected error when me not set"
    ))

let test_list_projects_ok () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.projects "team-42" (`Assoc [
    ("projects", `List [
      `Assoc [("id", `String "proj-1"); ("name", `String "Project Alpha")];
      `Assoc [("id", `String "proj-2"); ("name", `String "Project Beta")];
    ])
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_projects (`Assoc [("team_id", `String "team-42")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "found 2 projects" true
          (try let _ = Str.search_forward (Str.regexp_string "2 projects") text 0 in true with Not_found -> false);
        check bool "has Alpha" true
          (try let _ = Str.search_forward (Str.regexp_string "Project Alpha") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_list_projects_missing_params () =
  match Mcp_tools.handle_list_projects (`Assoc []) with
  | Error msg ->
    check bool "missing params" true
      (try let _ = Str.search_forward (Str.regexp_string "Missing") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_list_projects_api_error () =
  let store = Figma_effects.create_mock_store () in
  (* No data seeded for team-99 => Error *)
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_projects (`Assoc [("team_id", `String "team-99")]) with
      | Error _ -> ()
      | Ok _ -> fail "Expected error for missing team"
    ))

let test_list_projects_empty_projects () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.projects "team-empty" (`Assoc [
    ("projects", `List [])
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_projects (`Assoc [("team_id", `String "team-empty")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "found 0 projects" true
          (try let _ = Str.search_forward (Str.regexp_string "0 projects") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_list_projects_no_projects_field () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.projects "team-noproj" (`Assoc [("other", `String "data")]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_projects (`Assoc [("team_id", `String "team-noproj")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "found 0" true
          (try let _ = Str.search_forward (Str.regexp_string "0 projects") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_list_files_ok () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.project_files "proj-1" (`Assoc [
    ("files", `List [
      `Assoc [("key", `String "file-A"); ("name", `String "File Alpha")];
      `Assoc [("key", `String "file-B"); ("name", `String "File Beta")];
      `Assoc [("key", `String "file-C"); ("name", `String "File Gamma")];
    ])
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_files (`Assoc [("project_id", `String "proj-1")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "found 3 files" true
          (try let _ = Str.search_forward (Str.regexp_string "3 files") text 0 in true with Not_found -> false);
        check bool "has File Alpha" true
          (try let _ = Str.search_forward (Str.regexp_string "File Alpha") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_list_files_missing_params () =
  match Mcp_tools.handle_list_files (`Assoc []) with
  | Error msg ->
    check bool "missing params" true
      (try let _ = Str.search_forward (Str.regexp_string "Missing") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_list_files_api_error () =
  let store = Figma_effects.create_mock_store () in
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_files (`Assoc [("project_id", `String "proj-missing")]) with
      | Error _ -> ()
      | Ok _ -> fail "Expected error"
    ))

let test_list_files_no_files_field () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.project_files "proj-nofiles" (`Assoc [("other", `String "data")]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_files (`Assoc [("project_id", `String "proj-nofiles")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "found 0 files" true
          (try let _ = Str.search_forward (Str.regexp_string "0 files") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_list_files_partial_data () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.project_files "proj-partial" (`Assoc [
    ("files", `List [
      `Assoc [("key", `String "file-X")];  (* missing name *)
      `Assoc [("name", `String "File Y")]; (* missing key *)
      `Assoc [("key", `String "file-Z"); ("name", `String "File Z")]; (* complete *)
    ])
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_list_files (`Assoc [("project_id", `String "proj-partial")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        (* Only file-Z has both key and name *)
        check bool "found 1 file" true
          (try let _ = Str.search_forward (Str.regexp_string "1 files") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

(* ============================================================
   Group 8: Effect-dependent handlers — query, search, tree, stats, variables, tokens
   ============================================================ *)

let make_doc_json () =
  `Assoc [
    ("document", `Assoc [
      ("id", `String "0:0");
      ("name", `String "Document");
      ("type", `String "DOCUMENT");
      ("children", `List [
        `Assoc [
          ("id", `String "1:1");
          ("name", `String "Page 1");
          ("type", `String "CANVAS");
          ("children", `List [
            `Assoc [
              ("id", `String "2:1");
              ("name", `String "Frame A");
              ("type", `String "FRAME");
              ("absoluteBoundingBox", `Assoc [
                ("x", `Float 0.0); ("y", `Float 0.0);
                ("width", `Float 200.0); ("height", `Float 100.0);
              ]);
              ("children", `List []);
            ];
          ]);
        ];
      ]);
    ]);
  ]

let test_handle_tree_ok () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "file-tree" (make_doc_json ());
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_tree (`Assoc [("file_key", `String "file-tree")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "has tree output" true (String.length text > 0)
      | Error msg -> fail msg
    ))

let test_handle_tree_missing_params () =
  match Mcp_tools.handle_tree (`Assoc []) with
  | Error msg ->
    check bool "missing params" true
      (try let _ = Str.search_forward (Str.regexp_string "Missing") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_handle_stats_ok () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "file-stats" (make_doc_json ());
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_stats (`Assoc [("file_key", `String "file-stats")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "has stats" true (String.length text > 0)
      | Error msg -> fail msg
    ))

let test_handle_stats_missing_params () =
  match Mcp_tools.handle_stats (`Assoc []) with
  | Error msg ->
    check bool "missing params" true (String.length msg > 0)
  | Ok _ -> fail "Expected error"

let test_handle_get_variables_missing_params () =
  match Mcp_tools.handle_get_variables (`Assoc []) with
  | Error msg ->
    check bool "missing params" true
      (try let _ = Str.search_forward (Str.regexp_string "Missing") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_handle_get_variables_ok () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.variables "file-vars" (`Assoc [
    ("meta", `Assoc [
      ("variableCollections", `Assoc [("coll-1", `Assoc [])]);
      ("variables", `Assoc [
        ("var-1", `Assoc [("name", `String "primary-color")]);
        ("var-2", `Assoc [("name", `String "secondary-color")]);
      ]);
    ]);
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_get_variables (`Assoc [("file_key", `String "file-vars")]) with
      | Ok json ->
        let text = Yojson.Safe.Util.(json |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
        check bool "has collections" true
          (try let _ = Str.search_forward (Str.regexp_string "Collections: 1") text 0 in true with Not_found -> false);
        check bool "has variables" true
          (try let _ = Str.search_forward (Str.regexp_string "Variables: 2") text 0 in true with Not_found -> false)
      | Error msg -> fail msg
    ))

let test_handle_get_variables_raw () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.variables "file-vars-raw" (`Assoc [("data", `String "test")]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_get_variables (`Assoc [
        ("file_key", `String "file-vars-raw");
        ("format", `String "raw");
      ]) with
      | Ok _ -> ()
      | Error msg -> fail msg
    ))

let test_handle_get_variables_resolved () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.variables "file-vars-res" (`Assoc [
    ("meta", `Assoc [
      ("variableCollections", `Assoc []);
      ("variables", `Assoc []);
    ]);
  ]);
  with_figma_token "mock-token" (fun () ->
    Figma_effects.run_with_mock store (fun () ->
      match Mcp_tools.handle_get_variables (`Assoc [
        ("file_key", `String "file-vars-res");
        ("format", `String "resolved");
      ]) with
      | Ok _ -> ()
      | Error msg -> fail msg
    ))

let test_handle_team_tree_missing_team_id () =
  match Mcp_tools.handle_team_tree (`Assoc []) with
  | Error msg ->
    check bool "missing team_id" true
      (try let _ = Str.search_forward (Str.regexp_string "team_id") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_handle_team_tree_missing_token () =
  let old = Sys.getenv_opt "FIGMA_TOKEN" in
  Unix.putenv "FIGMA_TOKEN" "";
  Fun.protect ~finally:(fun () ->
    match old with
    | Some v -> Unix.putenv "FIGMA_TOKEN" v
    | None -> Unix.putenv "FIGMA_TOKEN" ""
  ) (fun () ->
    match Mcp_tools.handle_team_tree (`Assoc [("team_id", `String "t1")]) with
    | Error msg ->
      check bool "missing token" true
        (try let _ = Str.search_forward (Str.regexp_string "token") msg 0 in true with Not_found -> false)
    | Ok _ -> fail "Expected error"
  )

let test_handle_export_team_missing_params () =
  match Mcp_tools.handle_export_team (`Assoc []) with
  | Error msg ->
    check bool "missing team_id" true
      (try let _ = Str.search_forward (Str.regexp_string "team_id") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_handle_export_team_missing_token () =
  let old = Sys.getenv_opt "FIGMA_TOKEN" in
  Unix.putenv "FIGMA_TOKEN" "";
  Fun.protect ~finally:(fun () ->
    match old with
    | Some v -> Unix.putenv "FIGMA_TOKEN" v
    | None -> Unix.putenv "FIGMA_TOKEN" ""
  ) (fun () ->
    match Mcp_tools.handle_export_team (`Assoc [("team_id", `String "t1")]) with
    | Error msg ->
      check bool "missing token" true
        (try let _ = Str.search_forward (Str.regexp_string "token") msg 0 in true with Not_found -> false)
    | Ok _ -> fail "Expected error"
  )

let test_handle_export_team_missing_output_dir () =
  with_figma_token "mock-token" (fun () ->
    match Mcp_tools.handle_export_team (`Assoc [("team_id", `String "t1")]) with
    | Error msg ->
      check bool "missing output_dir" true
        (try let _ = Str.search_forward (Str.regexp_string "output_dir") msg 0 in true with Not_found -> false)
    | Ok _ -> fail "Expected error"
  )

let test_handle_export_tokens_missing_params () =
  match Mcp_tools.handle_export_tokens (`Assoc []) with
  | Error msg ->
    check bool "missing params" true (String.length msg > 0)
  | Ok _ -> fail "Expected error"

let test_handle_query_missing_params () =
  match Mcp_tools.handle_query (`Assoc []) with
  | Error msg ->
    check bool "missing params" true
      (try let _ = Str.search_forward (Str.regexp_string "Missing") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

let test_handle_search_missing_params () =
  match Mcp_tools.handle_search (`Assoc []) with
  | Error msg ->
    check bool "missing params" true
      (try let _ = Str.search_forward (Str.regexp_string "Missing") msg 0 in true with Not_found -> false)
  | Ok _ -> fail "Expected error"

(* ============================================================
   Group 9: is_under_dir and normalize_path helpers
   ============================================================ *)

let test_is_under_dir_true () =
  (* Use /private/tmp on macOS where /tmp is a symlink to /private/tmp *)
  let tmpdir = Filename.get_temp_dir_name () in
  let test_file = Filename.concat tmpdir "test_is_under_dir_a5.txt" in
  let oc = open_out test_file in close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove test_file) (fun () ->
    check bool "is under tmpdir" true (Mcp_tools.is_under_dir ~dir:tmpdir test_file))

let test_is_under_dir_false () =
  (* /usr/bin exists, /etc/passwd is not under it *)
  check bool "not under /usr/bin" false (Mcp_tools.is_under_dir ~dir:"/usr/bin" "/etc/hosts")

let test_is_under_dir_same () =
  let tmpdir = Filename.get_temp_dir_name () in
  check bool "same dir" true (Mcp_tools.is_under_dir ~dir:tmpdir tmpdir)

let test_is_under_dir_nonexistent () =
  check bool "nonexistent" false (Mcp_tools.is_under_dir ~dir:"/nonexistent123" "/nonexistent456/file")

(* ============================================================
   Group 10: is_network_error and string_contains helpers
   ============================================================ *)

let test_string_contains_found () =
  check bool "found" true (Mcp_helpers.string_contains ~haystack:"Hello World" ~needle:"world")

let test_string_contains_not_found () =
  check bool "not found" false (Mcp_helpers.string_contains ~haystack:"Hello World" ~needle:"xyz")

let test_string_contains_empty_sub () =
  check bool "empty sub" false (Mcp_helpers.string_contains ~haystack:"anything" ~needle:"")

let test_is_network_error_epipe () =
  check bool "epipe" true
    (Mcp_tools.is_network_error (Unix.Unix_error (Unix.EPIPE, "", "")))

let test_is_network_error_econnreset () =
  check bool "econnreset" true
    (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ECONNRESET, "", "")))

let test_is_network_error_etimedout () =
  check bool "etimedout" true
    (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ETIMEDOUT, "", "")))

let test_is_network_error_other () =
  check bool "not network" false
    (Mcp_tools.is_network_error (Failure "some other error"))

let test_is_network_error_string_match () =
  check bool "broken pipe string" true
    (Mcp_tools.is_network_error (Failure "broken pipe detected"))

(* ============================================================
   Test runner
   ============================================================ *)

let () =
  run "Mcp_tools A5" [
    ("handle_parse_url", [
      test_case "design URL" `Quick test_parse_url_design_url;
      test_case "file URL" `Quick test_parse_url_file_url;
      test_case "team URL" `Quick test_parse_url_team_url;
      test_case "proto URL" `Quick test_parse_url_proto_url;
      test_case "invalid URL" `Quick test_parse_url_invalid_url;
      test_case "missing url" `Quick test_parse_url_missing_url;
      test_case "no node_id" `Quick test_parse_url_no_node_id;
    ]);
    ("handle_cache", [
      test_case "stats" `Quick test_cache_stats;
      test_case "invalidate all" `Quick test_cache_invalidate_all;
      test_case "invalidate file_key" `Quick test_cache_invalidate_file_key;
      test_case "invalidate file+node" `Quick test_cache_invalidate_file_and_node;
    ]);
    ("handle_doctor", [
      test_case "system check" `Quick test_doctor;
    ]);
    ("handle_read_large_result", [
      test_case "missing path" `Quick test_read_large_result_missing_path;
      test_case "outside dir" `Quick test_read_large_result_outside_dir;
      test_case "nonexistent" `Quick test_read_large_result_nonexistent;
      test_case "valid file" `Quick test_read_large_result_valid_file;
      test_case "with offset" `Quick test_read_large_result_with_offset;
      test_case "offset beyond EOF" `Quick test_read_large_result_offset_beyond_eof;
      test_case "negative limit" `Quick test_read_large_result_negative_limit;
    ]);
    ("handle_codegen_sync", [
      test_case "missing json" `Quick test_codegen_sync_missing_json;
      test_case "invalid json" `Quick test_codegen_sync_invalid_json;
      test_case "raw format" `Quick test_codegen_sync_raw_format;
      test_case "fidelity format" `Quick test_codegen_sync_fidelity_format;
      test_case "html format" `Quick test_codegen_sync_html_format;
      test_case "default format" `Quick test_codegen_sync_default_format;
    ]);
    ("handle_category", [
      test_case "list core" `Quick test_category_list_core;
      test_case "list unknown" `Quick test_category_list_unknown;
      test_case "list visual" `Quick test_category_list_visual;
      test_case "describe missing tool" `Quick test_category_describe_missing_tool;
      test_case "describe not in category" `Quick test_category_describe_tool_not_in_category;
      test_case "describe valid" `Quick test_category_describe_valid_tool;
      test_case "call missing tool" `Quick test_category_call_missing_tool;
      test_case "call not found" `Quick test_category_call_tool_not_found;
      test_case "invalid mode" `Quick test_category_invalid_mode;
      test_case "auto-list" `Quick test_category_auto_list;
      test_case "auto-describe" `Quick test_category_auto_describe;
      test_case "call missing args" `Quick test_category_call_missing_args;
      test_case "list components" `Quick test_category_list_components;
      test_case "list export" `Quick test_category_list_export;
    ]);
    ("handle_get_me", [
      test_case "ok" `Quick test_get_me_ok;
      test_case "no token" `Quick test_get_me_no_token;
      test_case "api error" `Quick test_get_me_api_error;
    ]);
    ("handle_list_projects", [
      test_case "ok" `Quick test_list_projects_ok;
      test_case "missing params" `Quick test_list_projects_missing_params;
      test_case "api error" `Quick test_list_projects_api_error;
      test_case "empty projects" `Quick test_list_projects_empty_projects;
      test_case "no projects field" `Quick test_list_projects_no_projects_field;
    ]);
    ("handle_list_files", [
      test_case "ok" `Quick test_list_files_ok;
      test_case "missing params" `Quick test_list_files_missing_params;
      test_case "api error" `Quick test_list_files_api_error;
      test_case "no files field" `Quick test_list_files_no_files_field;
      test_case "partial data" `Quick test_list_files_partial_data;
    ]);
    ("handle_tree", [
      test_case "ok" `Quick test_handle_tree_ok;
      test_case "missing params" `Quick test_handle_tree_missing_params;
    ]);
    ("handle_stats", [
      test_case "ok" `Quick test_handle_stats_ok;
      test_case "missing params" `Quick test_handle_stats_missing_params;
    ]);
    ("handle_get_variables", [
      test_case "missing params" `Quick test_handle_get_variables_missing_params;
      test_case "summary format" `Quick test_handle_get_variables_ok;
      test_case "raw format" `Quick test_handle_get_variables_raw;
      test_case "resolved format" `Quick test_handle_get_variables_resolved;
    ]);
    ("handle_team_tree", [
      test_case "missing team_id" `Quick test_handle_team_tree_missing_team_id;
      test_case "missing token" `Quick test_handle_team_tree_missing_token;
    ]);
    ("handle_export_team", [
      test_case "missing params" `Quick test_handle_export_team_missing_params;
      test_case "missing token" `Quick test_handle_export_team_missing_token;
      test_case "missing output_dir" `Quick test_handle_export_team_missing_output_dir;
    ]);
    ("handle_export_tokens", [
      test_case "missing params" `Quick test_handle_export_tokens_missing_params;
    ]);
    ("handle_query", [
      test_case "missing params" `Quick test_handle_query_missing_params;
    ]);
    ("handle_search", [
      test_case "missing params" `Quick test_handle_search_missing_params;
    ]);
    ("is_under_dir", [
      test_case "true" `Quick test_is_under_dir_true;
      test_case "false" `Quick test_is_under_dir_false;
      test_case "same" `Quick test_is_under_dir_same;
      test_case "nonexistent" `Quick test_is_under_dir_nonexistent;
    ]);
    ("string_contains / is_network_error", [
      test_case "contains found" `Quick test_string_contains_found;
      test_case "contains not found" `Quick test_string_contains_not_found;
      test_case "contains empty" `Quick test_string_contains_empty_sub;
      test_case "epipe" `Quick test_is_network_error_epipe;
      test_case "econnreset" `Quick test_is_network_error_econnreset;
      test_case "etimedout" `Quick test_is_network_error_etimedout;
      test_case "other" `Quick test_is_network_error_other;
      test_case "string match" `Quick test_is_network_error_string_match;
    ]);
  ]
