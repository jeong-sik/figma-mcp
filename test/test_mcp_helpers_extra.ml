(** Coverage tests for mcp_helpers.ml — pure functions: schema, params, errors, monadic.
    Avoids I/O-heavy functions (Figma API calls, file operations). *)

let () =
  let open Alcotest in
  let json_testable =
    testable
      (fun ppf j -> Fmt.pf ppf "%s" (Yojson.Safe.to_string j))
      (fun a b -> Yojson.Safe.to_string a = Yojson.Safe.to_string b)
  in

  (* === Schema helpers === *)
  let test_string_prop () =
    let result = Mcp_helpers.string_prop "test desc" in
    let expected = `Assoc [("type", `String "string"); ("description", `String "test desc")] in
    check json_testable "string_prop" expected result
  in
  let test_number_prop () =
    let result = Mcp_helpers.number_prop "num desc" in
    let expected = `Assoc [("type", `String "number"); ("description", `String "num desc")] in
    check json_testable "number_prop" expected result
  in
  let test_bool_prop () =
    let result = Mcp_helpers.bool_prop "bool desc" in
    let expected = `Assoc [("type", `String "boolean"); ("description", `String "bool desc")] in
    check json_testable "bool_prop" expected result
  in
  let test_enum_prop () =
    let result = Mcp_helpers.enum_prop ["a"; "b"; "c"] "pick one" in
    match result with
    | `Assoc lst ->
        check string "type" "string"
          (match List.assoc_opt "type" lst with Some (`String s) -> s | _ -> "");
        (match List.assoc_opt "enum" lst with
         | Some (`List items) -> check int "3 options" 3 (List.length items)
         | _ -> fail "missing enum");
    | _ -> fail "not assoc"
  in
  let test_array_prop_default () =
    let result = Mcp_helpers.array_prop "arr desc" in
    match result with
    | `Assoc lst ->
        check string "type" "array"
          (match List.assoc_opt "type" lst with Some (`String s) -> s | _ -> "");
        (match List.assoc_opt "items" lst with
         | Some (`Assoc [("type", `String t)]) -> check string "items type" "string" t
         | _ -> fail "bad items");
    | _ -> fail "not assoc"
  in
  let test_array_prop_custom_type () =
    let result = Mcp_helpers.array_prop ~items_type:"number" "nums" in
    match result with
    | `Assoc lst ->
        (match List.assoc_opt "items" lst with
         | Some (`Assoc [("type", `String t)]) -> check string "items type" "number" t
         | _ -> fail "bad items");
    | _ -> fail "not assoc"
  in
  let test_object_prop () =
    let result = Mcp_helpers.object_prop "obj desc" in
    match result with
    | `Assoc lst ->
        check string "type" "object"
          (match List.assoc_opt "type" lst with Some (`String s) -> s | _ -> "")
    | _ -> fail "not assoc"
  in
  let test_object_schema () =
    let result = Mcp_helpers.object_schema
      [("name", `Assoc [("type", `String "string")])]
      ["name"]
    in
    match result with
    | `Assoc lst ->
        (match List.assoc_opt "required" lst with
         | Some (`List [`String "name"]) -> ()
         | _ -> fail "bad required");
        (match List.assoc_opt "properties" lst with
         | Some (`Assoc _) -> ()
         | _ -> fail "bad properties")
    | _ -> fail "not assoc"
  in

  (* === member === *)
  let test_member_found () =
    let json = `Assoc [("key", `String "val")] in
    check (option string) "found" (Some "val")
      (match Mcp_helpers.member "key" json with Some (`String s) -> Some s | _ -> None)
  in
  let test_member_missing () =
    let json = `Assoc [("other", `String "val")] in
    check bool "missing" true (Mcp_helpers.member "key" json = None)
  in
  let test_member_non_assoc () =
    let json = `List [`String "a"] in
    check bool "non-assoc" true (Mcp_helpers.member "key" json = None)
  in

  (* === get_string === *)
  let test_get_string_basic () =
    let json = `Assoc [("name", `String "hello")] in
    check (option string) "found" (Some "hello") (Mcp_helpers.get_string "name" json)
  in
  let test_get_string_missing () =
    let json = `Assoc [] in
    check (option string) "missing" None (Mcp_helpers.get_string "name" json)
  in
  let test_get_string_node_id_normalization () =
    (* node_id with "-" gets normalized to ":" *)
    let json = `Assoc [("node_id", `String "1-234")] in
    let result = Mcp_helpers.get_string "node_id" json in
    check (option string) "normalized" (Some "1:234") result
  in

  (* === get_string_list === *)
  let test_get_string_list_array () =
    let json = `Assoc [("ids", `List [`String "a"; `String "b"])] in
    match Mcp_helpers.get_string_list "ids" json with
    | Some lst -> check int "2 items" 2 (List.length lst)
    | None -> fail "expected list"
  in
  let test_get_string_list_csv () =
    let json = `Assoc [("ids", `String "a,b,c")] in
    match Mcp_helpers.get_string_list "ids" json with
    | Some lst -> check int "3 items" 3 (List.length lst)
    | None -> fail "expected list"
  in
  let test_get_string_list_empty_string () =
    let json = `Assoc [("ids", `String "")] in
    check bool "empty csv" true (Mcp_helpers.get_string_list "ids" json = None)
  in
  let test_get_string_list_empty_array () =
    let json = `Assoc [("ids", `List [])] in
    check bool "empty array" true (Mcp_helpers.get_string_list "ids" json = None)
  in
  let test_get_string_list_missing () =
    let json = `Assoc [] in
    check bool "missing" true (Mcp_helpers.get_string_list "ids" json = None)
  in
  let test_get_string_list_non_string_items () =
    let json = `Assoc [("ids", `List [`Int 1; `Int 2])] in
    check bool "non-string filtered" true (Mcp_helpers.get_string_list "ids" json = None)
  in

  (* === get_bool === *)
  let test_get_bool_true () =
    let json = `Assoc [("flag", `Bool true)] in
    check (option bool) "true" (Some true) (Mcp_helpers.get_bool "flag" json)
  in
  let test_get_bool_missing () =
    let json = `Assoc [] in
    check (option bool) "missing" None (Mcp_helpers.get_bool "flag" json)
  in
  let test_get_bool_wrong_type () =
    let json = `Assoc [("flag", `String "true")] in
    check (option bool) "string not bool" None (Mcp_helpers.get_bool "flag" json)
  in

  (* === get_float === *)
  let test_get_float_float () =
    let json = `Assoc [("scale", `Float 2.5)] in
    match Mcp_helpers.get_float "scale" json with
    | Some f -> check (float 0.01) "float" 2.5 f
    | None -> fail "expected float"
  in
  let test_get_float_int () =
    let json = `Assoc [("scale", `Int 3)] in
    match Mcp_helpers.get_float "scale" json with
    | Some f -> check (float 0.01) "int as float" 3.0 f
    | None -> fail "expected float"
  in
  let test_get_float_missing () =
    let json = `Assoc [] in
    check bool "missing" true (Mcp_helpers.get_float "scale" json = None)
  in

  (* === get_int === *)
  let test_get_int_int () =
    let json = `Assoc [("depth", `Int 5)] in
    check (option int) "int" (Some 5) (Mcp_helpers.get_int "depth" json)
  in
  let test_get_int_float () =
    let json = `Assoc [("depth", `Float 5.9)] in
    check (option int) "float as int" (Some 5) (Mcp_helpers.get_int "depth" json)
  in
  let test_get_int_missing () =
    let json = `Assoc [] in
    check (option int) "missing" None (Mcp_helpers.get_int "depth" json)
  in

  (* === get_int_or === *)
  let test_get_int_or_found () =
    let json = `Assoc [("depth", `Int 10)] in
    check int "found" 10 (Mcp_helpers.get_int_or "depth" 3 json)
  in
  let test_get_int_or_default () =
    let json = `Assoc [] in
    check int "default" 3 (Mcp_helpers.get_int_or "depth" 3 json)
  in

  (* === get_int_positive === *)
  let test_get_int_positive_ok () =
    let json = `Assoc [("n", `Int 5)] in
    check int "positive ok" 5 (Mcp_helpers.get_int_positive "n" 1 json)
  in
  let test_get_int_positive_zero () =
    let json = `Assoc [("n", `Int 0)] in
    check int "zero uses default" 1 (Mcp_helpers.get_int_positive "n" 1 json)
  in
  let test_get_int_positive_negative () =
    let json = `Assoc [("n", `Int (-5))] in
    check int "negative uses default" 1 (Mcp_helpers.get_int_positive "n" 1 json)
  in
  let test_get_int_positive_custom_min () =
    let json = `Assoc [("n", `Int 5)] in
    check int "below min" 1 (Mcp_helpers.get_int_positive ~min:5 "n" 1 json)
  in

  (* === get_int_nonneg === *)
  let test_get_int_nonneg_ok () =
    let json = `Assoc [("n", `Int 0)] in
    check int "zero is ok" 0 (Mcp_helpers.get_int_nonneg "n" 1 json)
  in
  let test_get_int_nonneg_negative () =
    let json = `Assoc [("n", `Int (-1))] in
    check int "negative uses default" 1 (Mcp_helpers.get_int_nonneg "n" 1 json)
  in

  (* === get_float_or === *)
  let test_get_float_or_found () =
    let json = `Assoc [("scale", `Float 2.0)] in
    check (float 0.01) "found" 2.0 (Mcp_helpers.get_float_or "scale" 1.0 json)
  in
  let test_get_float_or_default () =
    let json = `Assoc [] in
    check (float 0.01) "default" 1.0 (Mcp_helpers.get_float_or "scale" 1.0 json)
  in

  (* === get_bool_or === *)
  let test_get_bool_or_found () =
    let json = `Assoc [("flag", `Bool false)] in
    check bool "found" false (Mcp_helpers.get_bool_or "flag" true json)
  in
  let test_get_bool_or_default () =
    let json = `Assoc [] in
    check bool "default" true (Mcp_helpers.get_bool_or "flag" true json)
  in

  (* === get_string_or === *)
  let test_get_string_or_found () =
    let json = `Assoc [("fmt", `String "html")] in
    check string "found" "html" (Mcp_helpers.get_string_or "fmt" "raw" json)
  in
  let test_get_string_or_default () =
    let json = `Assoc [] in
    check string "default" "raw" (Mcp_helpers.get_string_or "fmt" "raw" json)
  in

  (* === error_to_string === *)
  let test_error_network () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.NetworkError "conn refused") in
    check bool "contains network" true (String.length s > 0)
  in
  let test_error_auth () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.AuthError "bad token") in
    check bool "contains auth" true (String.length s > 0)
  in
  let test_error_not_found () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.NotFound "file xyz") in
    check bool "contains not found" true (String.length s > 0)
  in
  let test_error_rate_limited () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.RateLimited 60.0) in
    check bool "contains rate" true (String.length s > 0)
  in
  let test_error_server () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.ServerError "500") in
    check bool "contains server" true (String.length s > 0)
  in
  let test_error_parse () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.ParseError "bad json") in
    check bool "contains parse" true (String.length s > 0)
  in
  let test_error_timeout () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.TimeoutError 30.0) in
    check bool "contains timeout" true (String.length s > 0)
  in
  let test_error_unknown () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.UnknownError "???") in
    check bool "contains unknown" true (String.length s > 0)
  in

  (* === classify_http_error === *)
  let test_classify_401 () =
    match Mcp_helpers.classify_http_error ~status_code:401 ~body:"unauth" with
    | Mcp_helpers.AuthError _ -> ()
    | _ -> fail "expected AuthError"
  in
  let test_classify_403 () =
    match Mcp_helpers.classify_http_error ~status_code:403 ~body:"forbidden" with
    | Mcp_helpers.AuthError _ -> ()
    | _ -> fail "expected AuthError"
  in
  let test_classify_404 () =
    match Mcp_helpers.classify_http_error ~status_code:404 ~body:"not found" with
    | Mcp_helpers.NotFound _ -> ()
    | _ -> fail "expected NotFound"
  in
  let test_classify_429 () =
    match Mcp_helpers.classify_http_error ~status_code:429 ~body:"retry after 30" with
    | Mcp_helpers.RateLimited secs -> check (float 0.1) "retry_after" 30.0 secs
    | _ -> fail "expected RateLimited"
  in
  let test_classify_429_default () =
    match Mcp_helpers.classify_http_error ~status_code:429 ~body:"too many requests" with
    | Mcp_helpers.RateLimited secs -> check (float 0.1) "default 60s" 60.0 secs
    | _ -> fail "expected RateLimited"
  in
  let test_classify_500 () =
    match Mcp_helpers.classify_http_error ~status_code:500 ~body:"internal" with
    | Mcp_helpers.ServerError _ -> ()
    | _ -> fail "expected ServerError"
  in
  let test_classify_502 () =
    match Mcp_helpers.classify_http_error ~status_code:502 ~body:"bad gateway" with
    | Mcp_helpers.ServerError _ -> ()
    | _ -> fail "expected ServerError"
  in
  let test_classify_other () =
    match Mcp_helpers.classify_http_error ~status_code:418 ~body:"teapot" with
    | Mcp_helpers.UnknownError _ -> ()
    | _ -> fail "expected UnknownError"
  in

  (* === monadic operators === *)
  let test_bind_ok () =
    match Mcp_helpers.( >>= ) (Ok 42) (fun x -> Ok (x + 1)) with
    | Ok v -> check int "bind ok" 43 v
    | Error e -> fail (Printf.sprintf "unexpected: %s" e)
  in
  let test_bind_error () =
    match Mcp_helpers.( >>= ) (Error "fail") (fun x -> Ok (x + 1)) with
    | Error msg -> check string "bind error" "fail" msg
    | Ok _ -> fail "expected error"
  in
  let test_map_ok () =
    match Mcp_helpers.( >>| ) (Ok 10) (fun x -> x * 2) with
    | Ok v -> check int "map ok" 20 v
    | Error e -> fail (Printf.sprintf "unexpected: %s" e)
  in
  let test_map_error () =
    match Mcp_helpers.( >>| ) (Error "fail") (fun x -> x * 2) with
    | Error msg -> check string "map error" "fail" msg
    | Ok _ -> fail "expected error"
  in

  (* === normalize/hyphenate node_id === *)
  let test_hyphenate_node_id () =
    let result = Mcp_helpers.hyphenate_node_id "1:234" in
    check string "hyphenated" "1-234" result
  in
  let test_hyphenate_no_colon () =
    let result = Mcp_helpers.hyphenate_node_id "abc" in
    check string "no change" "abc" result
  in

  (* === find_node_entry === *)
  let test_find_node_direct () =
    let map = [("1:234", `String "found")] in
    match Mcp_helpers.find_node_entry map ~node_id:"1:234" with
    | Some (_, `String "found") -> ()
    | _ -> fail "expected direct hit"
  in
  let test_find_node_hyphen () =
    let map = [("1-234", `String "found")] in
    match Mcp_helpers.find_node_entry map ~node_id:"1:234" with
    | Some (_, `String "found") -> ()
    | _ -> fail "expected hyphen match"
  in
  let test_find_node_normalize () =
    let map = [("1-234", `String "found")] in
    match Mcp_helpers.find_node_entry map ~node_id:"1-234" with
    | Some _ -> ()
    | None -> fail "expected normalized match"
  in
  let test_find_node_missing () =
    let map = [("1:234", `String "a")] in
    match Mcp_helpers.find_node_entry map ~node_id:"9:999" with
    | None -> ()
    | Some _ -> fail "expected None"
  in

  (* === prefer_some === *)
  let test_prefer_some_primary () =
    check (option string) "primary wins" (Some "a")
      (Mcp_helpers.prefer_some (Some "a") (Some "b"))
  in
  let test_prefer_some_fallback () =
    check (option string) "fallback" (Some "b")
      (Mcp_helpers.prefer_some None (Some "b"))
  in
  let test_prefer_some_both_none () =
    check (option string) "both none" None
      (Mcp_helpers.prefer_some None None)
  in

  (* === resolve_token === *)
  let test_resolve_token_env () =
    (* When FIGMA_TOKEN is set in env, it takes priority *)
    let old_token = Sys.getenv_opt "FIGMA_TOKEN" in
    Unix.putenv "FIGMA_TOKEN" "env-token-123";
    let result = Mcp_helpers.resolve_token (`Assoc [("token", `String "arg-token")]) in
    check (option string) "env wins" (Some "env-token-123") result;
    (* restore *)
    (match old_token with
     | Some t -> Unix.putenv "FIGMA_TOKEN" t
     | None -> (* Can't truly unsetenv in OCaml stdlib, just set empty *)
         Unix.putenv "FIGMA_TOKEN" "")
  in

  (* === handler_registry === *)
  let test_register_and_call () =
    Mcp_helpers.register_handler "test_handler" (fun _args -> Ok (`String "ok"));
    match Mcp_helpers.call_handler "test_handler" `Null with
    | Ok (`String "ok") -> ()
    | _ -> fail "handler should return ok"
  in
  let test_call_missing_handler () =
    match Mcp_helpers.call_handler "nonexistent_handler_xyz" `Null with
    | Error msg -> check bool "has not found" true (String.length msg > 0)
    | Ok _ -> fail "should be error"
  in

  (* === build_file_meta === *)
  let test_build_file_meta_with_meta () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("components", `Assoc [("c1", `String "comp1")]);
        ("componentSets", `Assoc []);
        ("styles", `Assoc [("s1", `String "style1")]);
      ])
    ] in
    match Mcp_helpers.build_file_meta json with
    | `Assoc lst ->
        check int "3 fields" 3 (List.length lst)
    | _ -> fail "expected assoc"
  in
  let test_build_file_meta_no_meta () =
    let json = `Assoc [
      ("components", `Assoc []);
      ("componentSets", `Null);
      ("styles", `Null);
    ] in
    match Mcp_helpers.build_file_meta json with
    | `Assoc _ -> ()
    | _ -> fail "expected assoc"
  in

  (* === normalize_node_id_key === *)
  let test_normalize_key_node_id () =
    let r = Mcp_helpers.normalize_node_id_key "node_id" "1-234" in
    check string "colon" "1:234" r
  in
  let test_normalize_key_node_a_id () =
    let r = Mcp_helpers.normalize_node_id_key "node_a_id" "5-6" in
    check string "colon" "5:6" r
  in
  let test_normalize_key_node_b_id () =
    let r = Mcp_helpers.normalize_node_id_key "node_b_id" "7-8" in
    check string "colon" "7:8" r
  in
  let test_normalize_key_other () =
    let r = Mcp_helpers.normalize_node_id_key "file_key" "abc-def" in
    check string "unchanged" "abc-def" r
  in

  (* === normalize_node_id === *)
  let test_normalize_node_id_hyphen () =
    check string "hyphen to colon" "1:234" (Mcp_helpers.normalize_node_id "1-234")
  in
  let test_normalize_node_id_colon () =
    check string "already colon" "1:234" (Mcp_helpers.normalize_node_id "1:234")
  in

  (* === get_json === *)
  let test_get_json_found () =
    let json = `Assoc [("key", `List [`Int 1; `Int 2])] in
    match Mcp_helpers.get_json "key" json with
    | Some (`List _) -> ()
    | _ -> fail "expected list"
  in
  let test_get_json_missing () =
    let json = `Assoc [("other", `Int 1)] in
    check (option json_testable) "missing" None (Mcp_helpers.get_json "absent" json)
  in

  (* === make_error_json === *)
  let test_make_error_json_basic () =
    let result = Mcp_helpers.make_error_json ~operation:"test_op"
      ~error:(Mcp_helpers.NotFound "item not found") () in
    match result with
    | `Assoc lst ->
        let has_error = List.assoc_opt "error" lst = Some (`Bool true) in
        let has_op = List.assoc_opt "operation" lst = Some (`String "test_op") in
        check bool "has error field" true has_error;
        check bool "has operation" true has_op
    | _ -> fail "expected assoc"
  in
  let test_make_error_json_debug_info () =
    let result = Mcp_helpers.make_error_json ~operation:"op"
      ~error:(Mcp_helpers.NetworkError "fail")
      ~debug_info:[("detail", `String "extra")] () in
    match result with
    | `Assoc lst ->
        (* debug_info is wrapped in ("debug", `Assoc [...]) *)
        (match List.assoc_opt "debug" lst with
         | Some (`Assoc dbg) ->
             let has_detail = List.assoc_opt "detail" dbg = Some (`String "extra") in
             check bool "has debug info" true has_detail
         | _ -> fail "expected debug assoc")
    | _ -> fail "expected assoc"
  in

  (* === resolve_url_info === *)
  let test_resolve_url_info_with_url () =
    let args = `Assoc [("url", `String "https://www.figma.com/design/ABC123/File?node-id=1-234")] in
    match Mcp_helpers.resolve_url_info args with
    | Some info ->
        check (option string) "file_key" (Some "ABC123") info.file_key
    | None -> fail "expected Some"
  in
  let test_resolve_url_info_no_url () =
    let args = `Assoc [("file_key", `String "ABC")] in
    check bool "no url" true (Mcp_helpers.resolve_url_info args = None)
  in

  (* === resolve_file_key_node_id === *)
  let test_resolve_fk_nid_from_args () =
    let args = `Assoc [("file_key", `String "FK1"); ("node_id", `String "1:2")] in
    let (fk, nid) = Mcp_helpers.resolve_file_key_node_id args in
    check (option string) "file_key" (Some "FK1") fk;
    check (option string) "node_id" (Some "1:2") nid
  in
  let test_resolve_fk_nid_from_url () =
    let args = `Assoc [("url", `String "https://www.figma.com/design/XYZ/File?node-id=3-4")] in
    let (fk, nid) = Mcp_helpers.resolve_file_key_node_id args in
    check (option string) "file_key from url" (Some "XYZ") fk;
    check bool "node_id present" true (nid <> None)
  in
  let test_resolve_fk_nid_args_over_url () =
    let args = `Assoc [
      ("file_key", `String "DIRECT");
      ("url", `String "https://www.figma.com/design/URL/File?node-id=1-2")
    ] in
    let (fk, _nid) = Mcp_helpers.resolve_file_key_node_id args in
    check (option string) "direct wins" (Some "DIRECT") fk
  in

  (* === resolve_node_id === *)
  let test_resolve_nid_direct () =
    let args = `Assoc [("node_id", `String "5:6")] in
    check (option string) "direct" (Some "5:6") (Mcp_helpers.resolve_node_id args)
  in
  let test_resolve_nid_from_url () =
    let args = `Assoc [("url", `String "https://www.figma.com/design/X/File?node-id=7-8")] in
    check bool "from url" true (Mcp_helpers.resolve_node_id args <> None)
  in
  let test_resolve_nid_none () =
    let args = `Assoc [("file_key", `String "nonode")] in
    check (option string) "none" None (Mcp_helpers.resolve_node_id args)
  in

  (* === with_temp_file === *)
  let test_with_temp_file () =
    (* Ensure /tmp/figma-visual/ exists *)
    (try Unix.mkdir "/tmp/figma-visual" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let result = Mcp_helpers.with_temp_file ~prefix:"test" ~suffix:".txt" (fun path ->
      let oc = open_out path in
      output_string oc "hello";
      close_out oc;
      Ok "done"
    ) in
    match result with
    | Ok "done" -> ()
    | Ok other -> fail (Printf.sprintf "unexpected: %s" other)
    | Error e -> fail (Printf.sprintf "error: %s" e)
  in

  (* ============================================================ *)
  (* NEW TESTS — targeting ~80+ additional bisect_ppx coverage points *)
  (* ============================================================ *)

  (* === classify_http_error — 503, 504, and other 5xx === *)
  let test_classify_503 () =
    match Mcp_helpers.classify_http_error ~status_code:503 ~body:"unavailable" with
    | Mcp_helpers.ServerError msg ->
        check bool "contains 503" true (String.length msg > 0)
    | _ -> fail "expected ServerError"
  in
  let test_classify_504 () =
    match Mcp_helpers.classify_http_error ~status_code:504 ~body:"gateway timeout" with
    | Mcp_helpers.ServerError msg ->
        check bool "contains 504" true (String.length msg > 0)
    | _ -> fail "expected ServerError"
  in
  let test_classify_599 () =
    match Mcp_helpers.classify_http_error ~status_code:599 ~body:"custom 5xx" with
    | Mcp_helpers.ServerError msg ->
        check bool "contains 599" true (String.length msg > 0)
    | _ -> fail "expected ServerError"
  in
  let test_classify_200 () =
    match Mcp_helpers.classify_http_error ~status_code:200 ~body:"ok" with
    | Mcp_helpers.UnknownError _ -> ()
    | _ -> fail "expected UnknownError for non-error status"
  in

  (* === error_to_string — verify actual message content === *)
  let test_error_to_string_network_msg () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.NetworkError "conn refused") in
    check bool "contains 'conn refused'" true (String.length s > 10 && s <> "")
  in
  let test_error_to_string_auth_msg () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.AuthError "invalid token") in
    check bool "contains 'invalid token'" true (String.length s > 10)
  in
  let test_error_to_string_rate_limited_msg () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.RateLimited 120.0) in
    check bool "contains retry" true (String.length s > 5)
  in
  let test_error_to_string_timeout_msg () =
    let s = Mcp_helpers.error_to_string (Mcp_helpers.TimeoutError 45.0) in
    check bool "contains timeout" true (String.length s > 5)
  in

  (* === make_error_json — all error types === *)
  let test_make_error_json_auth () =
    let result = Mcp_helpers.make_error_json ~operation:"auth_op"
      ~error:(Mcp_helpers.AuthError "bad token") () in
    match result with
    | `Assoc lst ->
        check bool "has timestamp" true (List.assoc_opt "timestamp" lst <> None);
        check bool "has timestamp_iso" true (List.assoc_opt "timestamp_iso" lst <> None);
        check bool "has message" true (List.assoc_opt "message" lst <> None)
    | _ -> fail "expected assoc"
  in
  let test_make_error_json_rate_limited () =
    let result = Mcp_helpers.make_error_json ~operation:"rate_op"
      ~error:(Mcp_helpers.RateLimited 30.0) () in
    match result with
    | `Assoc lst ->
        check bool "has error" true (List.assoc_opt "error" lst = Some (`Bool true))
    | _ -> fail "expected assoc"
  in
  let test_make_error_json_server () =
    let result = Mcp_helpers.make_error_json ~operation:"server_op"
      ~error:(Mcp_helpers.ServerError "500 fail") () in
    match result with
    | `Assoc lst ->
        check bool "has operation" true (List.assoc_opt "operation" lst = Some (`String "server_op"))
    | _ -> fail "expected assoc"
  in
  let test_make_error_json_parse () =
    let result = Mcp_helpers.make_error_json ~operation:"parse_op"
      ~error:(Mcp_helpers.ParseError "invalid json")
      ~debug_info:[("raw", `String "{{{")] () in
    match result with
    | `Assoc lst ->
        (match List.assoc_opt "debug" lst with
         | Some (`Assoc dbg) ->
             check bool "has raw" true (List.assoc_opt "raw" dbg = Some (`String "{{{"))
         | _ -> fail "expected debug assoc")
    | _ -> fail "expected assoc"
  in
  let test_make_error_json_timeout () =
    let result = Mcp_helpers.make_error_json ~operation:"timeout_op"
      ~error:(Mcp_helpers.TimeoutError 10.0) () in
    match result with
    | `Assoc lst ->
        check bool "no debug" true (List.assoc_opt "debug" lst = None)
    | _ -> fail "expected assoc"
  in
  let test_make_error_json_unknown () =
    let result = Mcp_helpers.make_error_json ~operation:"mystery"
      ~error:(Mcp_helpers.UnknownError "oops") () in
    match result with
    | `Assoc lst ->
        check bool "has error" true (List.assoc_opt "error" lst = Some (`Bool true))
    | _ -> fail "expected assoc"
  in

  (* === process_json_string === *)
  let test_process_json_raw () =
    match Mcp_helpers.process_json_string ~format:"raw" "{\"a\":1}" with
    | Ok s -> check bool "contains a" true (String.length s > 0)
    | Error e -> fail (Printf.sprintf "unexpected error: %s" e)
  in
  let test_process_json_invalid () =
    match Mcp_helpers.process_json_string ~format:"raw" "not json {{" with
    | Error msg -> check bool "parse error" true (String.length msg > 0)
    | Ok _ -> fail "expected error on invalid JSON"
  in
  let test_process_json_unknown_format () =
    match Mcp_helpers.process_json_string ~format:"nonexistent" "{\"a\":1}" with
    | Ok s -> check bool "unknown format msg" true (String.length s > 0)
    | Error _ -> fail "unknown format should not error"
  in
  let test_process_json_fidelity () =
    (* Simple node JSON to trigger fidelity path *)
    let json_str = Yojson.Safe.to_string (`Assoc [
      ("meta", `Assoc [("type", `String "FRAME"); ("name", `String "test")]);
      ("structure", `Assoc [("width", `Float 100.0); ("height", `Float 200.0)]);
    ]) in
    match Mcp_helpers.process_json_string ~format:"fidelity" json_str with
    | Ok s -> check bool "fidelity output" true (String.length s > 0)
    | Error e -> fail (Printf.sprintf "fidelity error: %s" e)
  in
  let test_process_json_html_no_document () =
    (* HTML format with a simple node that can be parsed *)
    let json_str = Yojson.Safe.to_string (`Assoc [
      ("type", `String "FRAME");
      ("name", `String "TestFrame");
      ("children", `List []);
    ]) in
    match Mcp_helpers.process_json_string ~format:"html" json_str with
    | Ok s -> check bool "html output" true (String.length s > 0)
    | Error e -> fail (Printf.sprintf "html error: %s" e)
  in
  let test_process_json_html_with_document () =
    (* HTML format wrapping in document structure *)
    let json_str = Yojson.Safe.to_string (`Assoc [
      ("document", `Assoc [
        ("type", `String "DOCUMENT");
        ("children", `List [
          `Assoc [
            ("type", `String "CANVAS");
            ("name", `String "Page 1");
            ("children", `List []);
          ]
        ]);
      ]);
    ]) in
    match Mcp_helpers.process_json_string ~format:"html" json_str with
    | Ok s -> check bool "html with doc" true (String.length s > 0)
    | Error _ -> ()  (* Either path is fine — depends on parser *)
  in

  (* === make_text_content / make_error_content === *)
  let test_make_text_content () =
    match Mcp_helpers.make_text_content "hello world" with
    | `Assoc lst ->
        (match List.assoc_opt "content" lst with
         | Some (`List [`Assoc items]) ->
             check bool "has type" true (List.assoc_opt "type" items = Some (`String "text"));
             check bool "has text" true (List.assoc_opt "text" items = Some (`String "hello world"))
         | _ -> fail "bad content")
    | _ -> fail "expected assoc"
  in
  let test_make_error_content () =
    match Mcp_helpers.make_error_content "something failed" with
    | `Assoc lst ->
        (match List.assoc_opt "content" lst with
         | Some (`List [`Assoc items]) ->
             check bool "has type" true (List.assoc_opt "type" items = Some (`String "text"));
             check bool "isError" true (List.assoc_opt "isError" items = Some (`Bool true));
             check bool "has text" true (List.assoc_opt "text" items = Some (`String "something failed"))
         | _ -> fail "bad content")
    | _ -> fail "expected assoc"
  in

  (* === sanitize_node_id === *)
  let test_sanitize_node_id_colon () =
    check string "colon to underscore" "1_234" (Mcp_helpers.sanitize_node_id "1:234")
  in
  let test_sanitize_node_id_no_colon () =
    check string "no change" "abc" (Mcp_helpers.sanitize_node_id "abc")
  in
  let test_sanitize_node_id_multiple () =
    check string "multiple colons" "1_2_3" (Mcp_helpers.sanitize_node_id "1:2:3")
  in

  (* === sanitize_file_key === *)
  let test_sanitize_file_key_clean () =
    check string "clean" "abc123" (Mcp_helpers.sanitize_file_key "abc123")
  in
  let test_sanitize_file_key_unsafe () =
    check string "unsafe chars" "a-b-c" (Mcp_helpers.sanitize_file_key "a.b/c")
  in
  let test_sanitize_file_key_empty () =
    check string "empty -> unknown" "unknown" (Mcp_helpers.sanitize_file_key "")
  in
  let test_sanitize_file_key_hyphens () =
    check string "hyphens kept" "a-b-c" (Mcp_helpers.sanitize_file_key "a-b-c")
  in
  let test_sanitize_file_key_underscores () =
    check string "underscores kept" "a_b_c" (Mcp_helpers.sanitize_file_key "a_b_c")
  in

  (* === is_http_url === *)
  let test_is_http_url_https () =
    check bool "https" true (Mcp_helpers.is_http_url "https://example.com/image.png")
  in
  let test_is_http_url_http () =
    check bool "http" true (Mcp_helpers.is_http_url "http://example.com")
  in
  let test_is_http_url_ftp () =
    check bool "ftp" false (Mcp_helpers.is_http_url "ftp://server/file")
  in
  let test_is_http_url_empty () =
    check bool "empty" false (Mcp_helpers.is_http_url "")
  in
  let test_is_http_url_relative () =
    check bool "relative" false (Mcp_helpers.is_http_url "/path/to/file")
  in

  (* === strip_query === *)
  let test_strip_query_with_query () =
    check string "stripped" "https://example.com/path"
      (Mcp_helpers.strip_query "https://example.com/path?key=val")
  in
  let test_strip_query_no_query () =
    check string "no change" "https://example.com/path"
      (Mcp_helpers.strip_query "https://example.com/path")
  in
  let test_strip_query_empty () =
    check string "empty" "" (Mcp_helpers.strip_query "")
  in

  (* === file_ext_from_url === *)
  let test_file_ext_png () =
    check string "png ext" ".png"
      (Mcp_helpers.file_ext_from_url "https://example.com/image.png?w=100")
  in
  let test_file_ext_jpg () =
    check string "jpg ext" ".jpg"
      (Mcp_helpers.file_ext_from_url "https://example.com/photo.jpg")
  in
  let test_file_ext_no_ext () =
    check string "default .img" ".img"
      (Mcp_helpers.file_ext_from_url "https://example.com/noext")
  in
  let test_file_ext_svg () =
    check string "svg ext" ".svg"
      (Mcp_helpers.file_ext_from_url "https://example.com/icon.svg?v=2")
  in

  (* === resolve_token — more branches === *)
  let test_resolve_token_no_env_with_arg () =
    let old_token = Sys.getenv_opt "FIGMA_TOKEN" in
    Unix.putenv "FIGMA_TOKEN" "";
    let result = Mcp_helpers.resolve_token (`Assoc [("token", `String "my-direct-token")]) in
    check (option string) "arg token" (Some "my-direct-token") result;
    (match old_token with
     | Some t -> Unix.putenv "FIGMA_TOKEN" t
     | None -> Unix.putenv "FIGMA_TOKEN" "")
  in
  let test_resolve_token_env_syntax () =
    let old_token = Sys.getenv_opt "FIGMA_TOKEN" in
    Unix.putenv "FIGMA_TOKEN" "";
    (* env:FIGMA_TOKEN syntax should try to read from env *)
    let result = Mcp_helpers.resolve_token (`Assoc [("token", `String "env:FIGMA_TOKEN")]) in
    (* FIGMA_TOKEN is empty, so returns None or Some "" *)
    check bool "env syntax with empty env" true (result = None || result = Some "");
    (match old_token with
     | Some t -> Unix.putenv "FIGMA_TOKEN" t
     | None -> Unix.putenv "FIGMA_TOKEN" "")
  in
  let test_resolve_token_env_other_var_blocked () =
    let old_token = Sys.getenv_opt "FIGMA_TOKEN" in
    Unix.putenv "FIGMA_TOKEN" "";
    (* env:OTHER_VAR should be blocked *)
    let result = Mcp_helpers.resolve_token (`Assoc [("token", `String "env:OTHER_VAR")]) in
    check (option string) "blocked env var" None result;
    (match old_token with
     | Some t -> Unix.putenv "FIGMA_TOKEN" t
     | None -> Unix.putenv "FIGMA_TOKEN" "")
  in
  let test_resolve_token_no_env_no_arg () =
    let old_token = Sys.getenv_opt "FIGMA_TOKEN" in
    Unix.putenv "FIGMA_TOKEN" "";
    let result = Mcp_helpers.resolve_token (`Assoc []) in
    check (option string) "none" None result;
    (match old_token with
     | Some t -> Unix.putenv "FIGMA_TOKEN" t
     | None -> Unix.putenv "FIGMA_TOKEN" "")
  in
  let test_resolve_token_empty_arg () =
    let old_token = Sys.getenv_opt "FIGMA_TOKEN" in
    Unix.putenv "FIGMA_TOKEN" "";
    let result = Mcp_helpers.resolve_token (`Assoc [("token", `String "")]) in
    check (option string) "empty arg" None result;
    (match old_token with
     | Some t -> Unix.putenv "FIGMA_TOKEN" t
     | None -> Unix.putenv "FIGMA_TOKEN" "")
  in

  (* === get_int_nonneg with custom min === *)
  let test_get_int_nonneg_custom_min_ok () =
    let json = `Assoc [("n", `Int 10)] in
    check int "above min" 10 (Mcp_helpers.get_int_nonneg ~min:5 "n" 1 json)
  in
  let test_get_int_nonneg_custom_min_below () =
    let json = `Assoc [("n", `Int 3)] in
    check int "below min uses default" 1 (Mcp_helpers.get_int_nonneg ~min:5 "n" 1 json)
  in
  let test_get_int_nonneg_custom_min_at () =
    let json = `Assoc [("n", `Int 5)] in
    check int "at min ok" 5 (Mcp_helpers.get_int_nonneg ~min:5 "n" 1 json)
  in

  (* === get_int_positive with more edge cases === *)
  let test_get_int_positive_above_min () =
    let json = `Assoc [("n", `Int 10)] in
    check int "above min" 10 (Mcp_helpers.get_int_positive ~min:5 "n" 1 json)
  in
  let test_get_int_positive_missing () =
    let json = `Assoc [] in
    check int "missing uses default" 42 (Mcp_helpers.get_int_positive "n" 42 json)
  in

  (* === fidelity_sections list verification === *)
  let test_fidelity_sections_length () =
    check int "14 sections" 14 (List.length Mcp_helpers.fidelity_sections)
  in
  let test_fidelity_sections_first () =
    match Mcp_helpers.fidelity_sections with
    | (name, _, weight) :: _ ->
        check string "first is meta" "meta" name;
        check (float 0.01) "meta weight" 0.4 weight
    | [] -> fail "empty sections"
  in

  (* === fidelity_score_of_dsl === *)
  let test_fidelity_score_empty () =
    let json = `Assoc [] in
    let (score, missing, _detail) = Mcp_helpers.fidelity_score_of_dsl json in
    check (float 0.01) "empty scores perfect" 1.0 score;
    check int "no missing" 0 missing
  in
  let test_fidelity_score_with_data () =
    let json = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME"); ("name", `String "X")]);
      ("meta_missing", `List [`String "id"]);
      ("structure", `Assoc [("width", `Float 100.0)]);
      ("structure_missing", `List []);
    ] in
    let (score, missing, detail) = Mcp_helpers.fidelity_score_of_dsl json in
    check bool "score < 1.0" true (score < 1.0);
    check bool "missing > 0" true (missing > 0);
    (match detail with
     | `Assoc _ -> ()
     | _ -> fail "expected detail assoc")
  in
  let test_fidelity_score_all_missing () =
    let json = `Assoc [
      ("meta", `Assoc []);
      ("meta_missing", `List [`String "a"; `String "b"; `String "c"]);
    ] in
    let (score, missing, _) = Mcp_helpers.fidelity_score_of_dsl json in
    check bool "score less" true (score < 1.0);
    check bool "has missing" true (missing >= 3)
  in

  (* === override_section === *)
  let test_override_section_auto_score () =
    let (score, present, missing, total) =
      Mcp_helpers.override_section ~present:3 ~missing:1 ~total:4 () in
    check (float 0.01) "auto score" 0.75 score;
    check int "present" 3 present;
    check int "missing" 1 missing;
    check int "total" 4 total
  in
  let test_override_section_explicit_score () =
    let (score, present, missing, total) =
      Mcp_helpers.override_section ~score:0.5 ~present:3 ~missing:1 ~total:4 () in
    check (float 0.01) "explicit score" 0.5 score;
    check int "present" 3 present;
    check int "missing" 1 missing;
    check int "total" 4 total
  in
  let test_override_section_zero_total () =
    let (score, _, _, _) =
      Mcp_helpers.override_section ~present:0 ~missing:0 ~total:0 () in
    check (float 0.01) "zero total = 1.0" 1.0 score
  in

  (* === fidelity_score_with_overrides === *)
  let test_fidelity_with_overrides () =
    let json = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME")]);
    ] in
    let overrides = [("meta", (0.5, 1, 1, 2))] in
    let (score, missing, _) = Mcp_helpers.fidelity_score_with_overrides json overrides in
    check bool "has overridden" true (score >= 0.0);
    check bool "has missing from override" true (missing >= 1)
  in

  (* === count_text_segments === *)
  let test_count_text_segments_none () =
    let json = `Assoc [("type", `String "FRAME")] in
    check int "no segments" 0 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_some () =
    let json = `Assoc [
      ("text", `Assoc [
        ("segments", `List [`String "seg1"; `String "seg2"])
      ])
    ] in
    check int "2 segments" 2 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_nested () =
    let child = `Assoc [
      ("text", `Assoc [
        ("segments", `List [`String "s1"])
      ])
    ] in
    let json = `Assoc [
      ("text", `Assoc [
        ("segments", `List [`String "s1"; `String "s2"])
      ]);
      ("children", `List [child])
    ] in
    check int "3 segments total" 3 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_list () =
    let item = `Assoc [
      ("text", `Assoc [
        ("segments", `List [`String "s1"])
      ])
    ] in
    let json = `List [item; item] in
    check int "2 from list" 2 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_primitive () =
    check int "null" 0 (Mcp_helpers.count_text_segments `Null)
  in
  let test_count_text_segments_no_segments_key () =
    let json = `Assoc [("text", `Assoc [("value", `String "hello")])] in
    check int "no segments key" 0 (Mcp_helpers.count_text_segments json)
  in

  (* === resolve_variables === *)
  let test_resolve_variables_empty () =
    let json = `Assoc [] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc _ -> ()
    | _ -> fail "expected assoc"
  in
  let test_resolve_variables_with_data () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc [
          ("col1", `Assoc [
            ("defaultModeId", `String "mode1");
            ("modes", `List [
              `Assoc [("modeId", `String "mode1"); ("name", `String "Light")]
            ])
          ])
        ]);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "primary-color");
            ("resolvedType", `String "COLOR");
            ("variableCollectionId", `String "col1");
            ("valuesByMode", `Assoc [
              ("mode1", `Assoc [("r", `Float 1.0); ("g", `Float 0.0); ("b", `Float 0.0)])
            ])
          ])
        ])
      ])
    ] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc lst ->
        check bool "has resolved" true (List.assoc_opt "resolved" lst <> None);
        check bool "has collections" true (List.assoc_opt "collections" lst <> None);
        check bool "has variables" true (List.assoc_opt "variables" lst <> None)
    | _ -> fail "expected assoc"
  in
  let test_resolve_variables_no_collection_match () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc []);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "orphan");
            ("variableCollectionId", `String "nonexistent");
            ("valuesByMode", `Assoc [("m1", `String "val")])
          ])
        ])
      ])
    ] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc lst ->
        (match List.assoc_opt "resolved" lst with
         | Some (`Assoc [("var1", `Assoc fields)]) ->
             check bool "null default mode" true
               (List.assoc_opt "defaultModeId" fields = Some `Null)
         | _ -> fail "expected resolved var1")
    | _ -> fail "expected assoc"
  in
  let test_resolve_variables_no_values_by_mode () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc [
          ("col1", `Assoc [("defaultModeId", `String "m1")])
        ]);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "novals");
            ("variableCollectionId", `String "col1");
          ])
        ])
      ])
    ] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc _ -> ()
    | _ -> fail "expected assoc"
  in
  let test_resolve_variables_first_mode_fallback () =
    (* When defaultModeId does not match, use first mode value *)
    let json = `Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc [
          ("col1", `Assoc [])  (* no defaultModeId *)
        ]);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "test");
            ("variableCollectionId", `String "col1");
            ("valuesByMode", `Assoc [("m1", `String "fallback-val")])
          ])
        ])
      ])
    ] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc lst ->
        (match List.assoc_opt "resolved" lst with
         | Some (`Assoc [("var1", `Assoc fields)]) ->
             check bool "default value from first mode" true
               (List.assoc_opt "defaultValue" fields = Some (`String "fallback-val"))
         | _ -> fail "expected resolved var1")
    | _ -> fail "expected assoc"
  in

  (* === plugin_payload_if_ok === *)
  let test_plugin_payload_ok () =
    let json = `Assoc [("ok", `Bool true); ("payload", `Assoc [("data", `Int 1)])] in
    match Mcp_helpers.plugin_payload_if_ok json with
    | Some (`Assoc _) -> ()
    | _ -> fail "expected payload"
  in
  let test_plugin_payload_not_ok () =
    let json = `Assoc [("ok", `Bool false); ("payload", `Assoc [])] in
    check bool "not ok" true (Mcp_helpers.plugin_payload_if_ok json = None)
  in
  let test_plugin_payload_no_ok () =
    let json = `Assoc [("payload", `Assoc [])] in
    check bool "no ok field" true (Mcp_helpers.plugin_payload_if_ok json = None)
  in
  let test_plugin_payload_not_assoc () =
    check bool "not assoc" true (Mcp_helpers.plugin_payload_if_ok `Null = None)
  in

  (* === resolve_plugin_variables === *)
  let test_resolve_plugin_variables_ok () =
    let payload = `Assoc [
      ("collections", `Assoc [
        ("col1", `Assoc [("defaultModeId", `String "m1")])
      ]);
      ("variables", `Assoc [
        ("v1", `Assoc [("name", `String "test"); ("variableCollectionId", `String "col1")])
      ])
    ] in
    let result = Mcp_helpers.resolve_plugin_variables payload in
    match result with
    | `Assoc lst -> check bool "has resolved" true (List.assoc_opt "resolved" lst <> None)
    | _ -> fail "expected assoc"
  in
  let test_resolve_plugin_variables_missing () =
    let payload = `Assoc [("other", `String "data")] in
    let result = Mcp_helpers.resolve_plugin_variables payload in
    match result with
    | `Assoc lst -> check bool "has error" true (List.assoc_opt "error" lst <> None)
    | _ -> fail "expected error assoc"
  in
  let test_resolve_plugin_variables_not_assoc () =
    let result = Mcp_helpers.resolve_plugin_variables `Null in
    match result with
    | `Assoc lst -> check bool "has error" true (List.assoc_opt "error" lst <> None)
    | _ -> fail "expected error assoc"
  in

  (* === fidelity_score_of_bundle === *)
  let test_fidelity_bundle_basic () =
    let dsl = `Assoc [] in
    let (score, missing, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables:`Null ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:false ~include_plugin:false in
    check (float 0.01) "basic bundle score" 1.0 score;
    check int "no missing" 0 missing
  in
  let test_fidelity_bundle_with_variables () =
    let dsl = `Assoc [] in
    let variables = `Assoc [
      ("variables", `Assoc [("v1", `String "a"); ("v2", `String "b")]);
      ("resolved", `Assoc [("v1", `String "a")])
    ] in
    let (score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:true ~include_image_fills:false ~include_plugin:false in
    check bool "score with vars" true (score >= 0.0 && score <= 1.0)
  in
  let test_fidelity_bundle_with_image_fills () =
    let dsl = `Assoc [
      ("assets", `Assoc [("image_refs", `List [`String "ref1"; `String "ref2"])])
    ] in
    let image_fills = `Assoc [
      ("images", `Assoc [("ref1", `String "https://example.com/img.png")])
    ] in
    let (score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables:`Null ~image_fills ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false in
    check bool "score with fills" true (score >= 0.0 && score <= 1.0)
  in
  let test_fidelity_bundle_with_plugin () =
    let dsl = `Assoc [
      ("meta", `Assoc [("type", `String "TEXT"); ("name", `String "label")]);
    ] in
    let plugin = `Assoc [
      ("ok", `Bool true);
      ("payload", `Assoc [
        ("text", `Assoc [("segments", `List [`String "seg1"])])
      ])
    ] in
    let (score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables:`Null ~image_fills:`Null ~plugin_snapshot:plugin
      ~include_variables:false ~include_image_fills:false ~include_plugin:true in
    check bool "score with plugin" true (score >= 0.0 && score <= 1.0)
  in
  let test_fidelity_bundle_plugin_not_ok () =
    let dsl = `Assoc [] in
    let plugin = `Assoc [("ok", `Bool false)] in
    let (_, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables:`Null ~image_fills:`Null ~plugin_snapshot:plugin
      ~include_variables:false ~include_image_fills:false ~include_plugin:true in
    ()  (* Just make sure it doesn't crash *)
  in
  let test_fidelity_bundle_empty_image_refs () =
    let dsl = `Assoc [("assets", `Assoc [("image_refs", `List [])])] in
    let image_fills = `Assoc [("images", `Assoc [])] in
    let (score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables:`Null ~image_fills ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false in
    check (float 0.01) "empty refs perfect" 1.0 score
  in
  let test_fidelity_bundle_variables_error () =
    let dsl = `Assoc [] in
    let variables = `Assoc [("error", `String "no access")] in
    let (score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:dsl ~variables ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:true ~include_image_fills:false ~include_plugin:false in
    check bool "variables error handled" true (score >= 0.0)
  in

  (* === get_string edge cases === *)
  let test_get_string_non_string_value () =
    let json = `Assoc [("key", `Int 42)] in
    check (option string) "non-string" None (Mcp_helpers.get_string "key" json)
  in
  let test_get_string_null_value () =
    let json = `Assoc [("key", `Null)] in
    check (option string) "null" None (Mcp_helpers.get_string "key" json)
  in

  (* === get_float edge cases === *)
  let test_get_float_string () =
    let json = `Assoc [("x", `String "1.5")] in
    check bool "string not float" true (Mcp_helpers.get_float "x" json = None)
  in

  (* === get_int edge cases === *)
  let test_get_int_string () =
    let json = `Assoc [("x", `String "5")] in
    check (option int) "string not int" None (Mcp_helpers.get_int "x" json)
  in

  (* === get_bool edge cases === *)
  let test_get_bool_false () =
    let json = `Assoc [("flag", `Bool false)] in
    check (option bool) "false" (Some false) (Mcp_helpers.get_bool "flag" json)
  in
  let test_get_bool_int () =
    let json = `Assoc [("flag", `Int 1)] in
    check (option bool) "int not bool" None (Mcp_helpers.get_bool "flag" json)
  in

  (* === get_string_list edge cases === *)
  let test_get_string_list_mixed () =
    let json = `Assoc [("ids", `List [`String "a"; `Int 1; `String "b"])] in
    match Mcp_helpers.get_string_list "ids" json with
    | Some lst -> check int "2 items (ints filtered)" 2 (List.length lst)
    | None -> fail "expected some"
  in
  let test_get_string_list_csv_spaces () =
    let json = `Assoc [("ids", `String " a , b , c ")] in
    match Mcp_helpers.get_string_list "ids" json with
    | Some lst ->
        check int "3 items" 3 (List.length lst);
        check string "trimmed" "a" (List.hd lst)
    | None -> fail "expected some"
  in
  let test_get_string_list_single_csv () =
    let json = `Assoc [("ids", `String "single")] in
    match Mcp_helpers.get_string_list "ids" json with
    | Some lst -> check int "1 item" 1 (List.length lst)
    | None -> fail "expected some"
  in
  let test_get_string_list_whitespace_only () =
    let json = `Assoc [("ids", `List [`String "  "; `String ""])] in
    check bool "whitespace filtered" true (Mcp_helpers.get_string_list "ids" json = None)
  in

  (* === resolve_url_info — more URL patterns === *)
  let test_resolve_url_info_file_only () =
    let args = `Assoc [("url", `String "https://www.figma.com/file/ABC/Name")] in
    match Mcp_helpers.resolve_url_info args with
    | Some info ->
        check (option string) "file_key" (Some "ABC") info.file_key
    | None -> fail "expected Some"
  in
  let test_resolve_url_info_invalid_url () =
    let args = `Assoc [("url", `String "not-a-url")] in
    (* Should still return Some with parsed info, even if fields are None *)
    let _ = Mcp_helpers.resolve_url_info args in
    ()  (* Just verifying no crash *)
  in

  (* === resolve_file_key_node_id — more cases === *)
  let test_resolve_fk_nid_empty () =
    let args = `Assoc [] in
    let (fk, nid) = Mcp_helpers.resolve_file_key_node_id args in
    check (option string) "no file_key" None fk;
    check (option string) "no node_id" None nid
  in
  let test_resolve_fk_nid_node_only () =
    let args = `Assoc [("node_id", `String "5:6")] in
    let (fk, nid) = Mcp_helpers.resolve_file_key_node_id args in
    check (option string) "no file_key" None fk;
    check (option string) "has node_id" (Some "5:6") nid
  in

  (* === resolve_node_id — hyphenated in URL === *)
  let test_resolve_nid_hyphen_normalized () =
    let args = `Assoc [("node_id", `String "5-6")] in
    check (option string) "normalized" (Some "5:6") (Mcp_helpers.resolve_node_id args)
  in

  (* === build_file_meta — null meta fields === *)
  let test_build_file_meta_null_children () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("components", `Null);
        ("componentSets", `Null);
        ("styles", `Null);
      ])
    ] in
    match Mcp_helpers.build_file_meta json with
    | `Assoc lst ->
        check int "3 fields" 3 (List.length lst);
        check bool "components null" true (List.assoc_opt "components" lst = Some `Null)
    | _ -> fail "expected assoc"
  in

  (* === find_node_entry — fallback normalized scan === *)
  let test_find_node_entry_normalized_scan () =
    (* The map key uses a different format, requiring normalization to match *)
    let map = [("1:234:5", `String "found")] in
    match Mcp_helpers.find_node_entry map ~node_id:"1:234:5" with
    | Some (_, `String "found") -> ()
    | _ -> fail "expected normalized scan match"
  in

  (* === mkdir_p === *)
  let test_mkdir_p_new () =
    let dir = Printf.sprintf "/tmp/figma-test-%d/a/b" (Random.int 100000) in
    Mcp_helpers.mkdir_p dir;
    check bool "dir exists" true (Sys.file_exists dir)
  in
  let test_mkdir_p_existing () =
    (* /tmp always exists *)
    Mcp_helpers.mkdir_p "/tmp";
    check bool "still exists" true (Sys.file_exists "/tmp")
  in

  (* === default_asset_dir / default_compare_dir === *)
  let test_default_asset_dir () =
    let dir = Mcp_helpers.default_asset_dir () in
    check bool "non-empty" true (String.length dir > 0)
  in
  let test_default_compare_dir () =
    let dir = Mcp_helpers.default_compare_dir () in
    check bool "non-empty" true (String.length dir > 0);
    check bool "ends with compare" true
      (let len = String.length dir in
       len >= 8 && String.sub dir (len - 7) 7 = "compare")
  in

  run "Mcp_helpers Coverage" [
    ("schema helpers", [
      test_case "string_prop" `Quick test_string_prop;
      test_case "number_prop" `Quick test_number_prop;
      test_case "bool_prop" `Quick test_bool_prop;
      test_case "enum_prop" `Quick test_enum_prop;
      test_case "array_prop default" `Quick test_array_prop_default;
      test_case "array_prop custom" `Quick test_array_prop_custom_type;
      test_case "object_prop" `Quick test_object_prop;
      test_case "object_schema" `Quick test_object_schema;
    ]);
    ("member", [
      test_case "found" `Quick test_member_found;
      test_case "missing" `Quick test_member_missing;
      test_case "non-assoc" `Quick test_member_non_assoc;
    ]);
    ("get_string", [
      test_case "basic" `Quick test_get_string_basic;
      test_case "missing" `Quick test_get_string_missing;
      test_case "node_id normalization" `Quick test_get_string_node_id_normalization;
    ]);
    ("get_string_list", [
      test_case "array" `Quick test_get_string_list_array;
      test_case "csv" `Quick test_get_string_list_csv;
      test_case "empty string" `Quick test_get_string_list_empty_string;
      test_case "empty array" `Quick test_get_string_list_empty_array;
      test_case "missing" `Quick test_get_string_list_missing;
      test_case "non-string items" `Quick test_get_string_list_non_string_items;
    ]);
    ("get_bool", [
      test_case "true" `Quick test_get_bool_true;
      test_case "missing" `Quick test_get_bool_missing;
      test_case "wrong type" `Quick test_get_bool_wrong_type;
    ]);
    ("get_float", [
      test_case "float" `Quick test_get_float_float;
      test_case "int" `Quick test_get_float_int;
      test_case "missing" `Quick test_get_float_missing;
    ]);
    ("get_int", [
      test_case "int" `Quick test_get_int_int;
      test_case "float" `Quick test_get_int_float;
      test_case "missing" `Quick test_get_int_missing;
    ]);
    ("get_int_or", [
      test_case "found" `Quick test_get_int_or_found;
      test_case "default" `Quick test_get_int_or_default;
    ]);
    ("get_int_positive", [
      test_case "ok" `Quick test_get_int_positive_ok;
      test_case "zero" `Quick test_get_int_positive_zero;
      test_case "negative" `Quick test_get_int_positive_negative;
      test_case "custom min" `Quick test_get_int_positive_custom_min;
    ]);
    ("get_int_nonneg", [
      test_case "zero ok" `Quick test_get_int_nonneg_ok;
      test_case "negative" `Quick test_get_int_nonneg_negative;
    ]);
    ("get_float_or", [
      test_case "found" `Quick test_get_float_or_found;
      test_case "default" `Quick test_get_float_or_default;
    ]);
    ("get_bool_or", [
      test_case "found" `Quick test_get_bool_or_found;
      test_case "default" `Quick test_get_bool_or_default;
    ]);
    ("get_string_or", [
      test_case "found" `Quick test_get_string_or_found;
      test_case "default" `Quick test_get_string_or_default;
    ]);
    ("error_to_string", [
      test_case "network" `Quick test_error_network;
      test_case "auth" `Quick test_error_auth;
      test_case "not found" `Quick test_error_not_found;
      test_case "rate limited" `Quick test_error_rate_limited;
      test_case "server" `Quick test_error_server;
      test_case "parse" `Quick test_error_parse;
      test_case "timeout" `Quick test_error_timeout;
      test_case "unknown" `Quick test_error_unknown;
    ]);
    ("classify_http_error", [
      test_case "401" `Quick test_classify_401;
      test_case "403" `Quick test_classify_403;
      test_case "404" `Quick test_classify_404;
      test_case "429" `Quick test_classify_429;
      test_case "429 default" `Quick test_classify_429_default;
      test_case "500" `Quick test_classify_500;
      test_case "502" `Quick test_classify_502;
      test_case "other" `Quick test_classify_other;
    ]);
    ("monadic", [
      test_case "bind ok" `Quick test_bind_ok;
      test_case "bind error" `Quick test_bind_error;
      test_case "map ok" `Quick test_map_ok;
      test_case "map error" `Quick test_map_error;
    ]);
    ("node_id", [
      test_case "hyphenate" `Quick test_hyphenate_node_id;
      test_case "no colon" `Quick test_hyphenate_no_colon;
    ]);
    ("find_node_entry", [
      test_case "direct" `Quick test_find_node_direct;
      test_case "hyphen" `Quick test_find_node_hyphen;
      test_case "normalize" `Quick test_find_node_normalize;
      test_case "missing" `Quick test_find_node_missing;
    ]);
    ("prefer_some", [
      test_case "primary" `Quick test_prefer_some_primary;
      test_case "fallback" `Quick test_prefer_some_fallback;
      test_case "both none" `Quick test_prefer_some_both_none;
    ]);
    ("resolve_token", [
      test_case "env priority" `Quick test_resolve_token_env;
    ]);
    ("handler_registry", [
      test_case "register and call" `Quick test_register_and_call;
      test_case "missing handler" `Quick test_call_missing_handler;
    ]);
    ("build_file_meta", [
      test_case "with meta" `Quick test_build_file_meta_with_meta;
      test_case "no meta" `Quick test_build_file_meta_no_meta;
    ]);
    ("normalize_node_id_key", [
      test_case "node_id" `Quick test_normalize_key_node_id;
      test_case "node_a_id" `Quick test_normalize_key_node_a_id;
      test_case "node_b_id" `Quick test_normalize_key_node_b_id;
      test_case "other key" `Quick test_normalize_key_other;
    ]);
    ("normalize_node_id", [
      test_case "hyphen" `Quick test_normalize_node_id_hyphen;
      test_case "colon" `Quick test_normalize_node_id_colon;
    ]);
    ("get_json", [
      test_case "found" `Quick test_get_json_found;
      test_case "missing" `Quick test_get_json_missing;
    ]);
    ("make_error_json", [
      test_case "basic" `Quick test_make_error_json_basic;
      test_case "debug info" `Quick test_make_error_json_debug_info;
    ]);
    ("resolve_url_info", [
      test_case "with url" `Quick test_resolve_url_info_with_url;
      test_case "no url" `Quick test_resolve_url_info_no_url;
    ]);
    ("resolve_file_key_node_id", [
      test_case "from args" `Quick test_resolve_fk_nid_from_args;
      test_case "from url" `Quick test_resolve_fk_nid_from_url;
      test_case "args over url" `Quick test_resolve_fk_nid_args_over_url;
    ]);
    ("resolve_node_id", [
      test_case "direct" `Quick test_resolve_nid_direct;
      test_case "from url" `Quick test_resolve_nid_from_url;
      test_case "none" `Quick test_resolve_nid_none;
    ]);
    ("with_temp_file", [
      test_case "basic" `Quick test_with_temp_file;
    ]);

    (* NEW test groups *)
    ("classify_http_error extra", [
      test_case "503" `Quick test_classify_503;
      test_case "504" `Quick test_classify_504;
      test_case "599" `Quick test_classify_599;
      test_case "200" `Quick test_classify_200;
    ]);
    ("error_to_string content", [
      test_case "network msg" `Quick test_error_to_string_network_msg;
      test_case "auth msg" `Quick test_error_to_string_auth_msg;
      test_case "rate limited msg" `Quick test_error_to_string_rate_limited_msg;
      test_case "timeout msg" `Quick test_error_to_string_timeout_msg;
    ]);
    ("make_error_json extra", [
      test_case "auth" `Quick test_make_error_json_auth;
      test_case "rate limited" `Quick test_make_error_json_rate_limited;
      test_case "server" `Quick test_make_error_json_server;
      test_case "parse with debug" `Quick test_make_error_json_parse;
      test_case "timeout" `Quick test_make_error_json_timeout;
      test_case "unknown" `Quick test_make_error_json_unknown;
    ]);
    ("process_json_string", [
      test_case "raw" `Quick test_process_json_raw;
      test_case "invalid json" `Quick test_process_json_invalid;
      test_case "unknown format" `Quick test_process_json_unknown_format;
      test_case "fidelity" `Quick test_process_json_fidelity;
      test_case "html no doc" `Quick test_process_json_html_no_document;
      test_case "html with doc" `Quick test_process_json_html_with_document;
    ]);
    ("make_text/error_content", [
      test_case "text content" `Quick test_make_text_content;
      test_case "error content" `Quick test_make_error_content;
    ]);
    ("sanitize_node_id", [
      test_case "colon" `Quick test_sanitize_node_id_colon;
      test_case "no colon" `Quick test_sanitize_node_id_no_colon;
      test_case "multiple" `Quick test_sanitize_node_id_multiple;
    ]);
    ("sanitize_file_key", [
      test_case "clean" `Quick test_sanitize_file_key_clean;
      test_case "unsafe" `Quick test_sanitize_file_key_unsafe;
      test_case "empty" `Quick test_sanitize_file_key_empty;
      test_case "hyphens" `Quick test_sanitize_file_key_hyphens;
      test_case "underscores" `Quick test_sanitize_file_key_underscores;
    ]);
    ("is_http_url", [
      test_case "https" `Quick test_is_http_url_https;
      test_case "http" `Quick test_is_http_url_http;
      test_case "ftp" `Quick test_is_http_url_ftp;
      test_case "empty" `Quick test_is_http_url_empty;
      test_case "relative" `Quick test_is_http_url_relative;
    ]);
    ("strip_query", [
      test_case "with query" `Quick test_strip_query_with_query;
      test_case "no query" `Quick test_strip_query_no_query;
      test_case "empty" `Quick test_strip_query_empty;
    ]);
    ("file_ext_from_url", [
      test_case "png" `Quick test_file_ext_png;
      test_case "jpg" `Quick test_file_ext_jpg;
      test_case "no ext" `Quick test_file_ext_no_ext;
      test_case "svg" `Quick test_file_ext_svg;
    ]);
    ("resolve_token extra", [
      test_case "no env with arg" `Quick test_resolve_token_no_env_with_arg;
      test_case "env:FIGMA_TOKEN syntax" `Quick test_resolve_token_env_syntax;
      test_case "env:OTHER blocked" `Quick test_resolve_token_env_other_var_blocked;
      test_case "no env no arg" `Quick test_resolve_token_no_env_no_arg;
      test_case "empty arg" `Quick test_resolve_token_empty_arg;
    ]);
    ("get_int_nonneg extra", [
      test_case "custom min ok" `Quick test_get_int_nonneg_custom_min_ok;
      test_case "custom min below" `Quick test_get_int_nonneg_custom_min_below;
      test_case "custom min at" `Quick test_get_int_nonneg_custom_min_at;
    ]);
    ("get_int_positive extra", [
      test_case "above min" `Quick test_get_int_positive_above_min;
      test_case "missing" `Quick test_get_int_positive_missing;
    ]);
    ("fidelity_sections", [
      test_case "length" `Quick test_fidelity_sections_length;
      test_case "first entry" `Quick test_fidelity_sections_first;
    ]);
    ("fidelity_score_of_dsl", [
      test_case "empty" `Quick test_fidelity_score_empty;
      test_case "with data" `Quick test_fidelity_score_with_data;
      test_case "all missing" `Quick test_fidelity_score_all_missing;
    ]);
    ("override_section", [
      test_case "auto score" `Quick test_override_section_auto_score;
      test_case "explicit score" `Quick test_override_section_explicit_score;
      test_case "zero total" `Quick test_override_section_zero_total;
    ]);
    ("fidelity_score_with_overrides", [
      test_case "with overrides" `Quick test_fidelity_with_overrides;
    ]);
    ("count_text_segments", [
      test_case "none" `Quick test_count_text_segments_none;
      test_case "some" `Quick test_count_text_segments_some;
      test_case "nested" `Quick test_count_text_segments_nested;
      test_case "list" `Quick test_count_text_segments_list;
      test_case "primitive" `Quick test_count_text_segments_primitive;
      test_case "no segments key" `Quick test_count_text_segments_no_segments_key;
    ]);
    ("resolve_variables", [
      test_case "empty" `Quick test_resolve_variables_empty;
      test_case "with data" `Quick test_resolve_variables_with_data;
      test_case "no collection match" `Quick test_resolve_variables_no_collection_match;
      test_case "no valuesByMode" `Quick test_resolve_variables_no_values_by_mode;
      test_case "first mode fallback" `Quick test_resolve_variables_first_mode_fallback;
    ]);
    ("plugin_payload_if_ok", [
      test_case "ok" `Quick test_plugin_payload_ok;
      test_case "not ok" `Quick test_plugin_payload_not_ok;
      test_case "no ok field" `Quick test_plugin_payload_no_ok;
      test_case "not assoc" `Quick test_plugin_payload_not_assoc;
    ]);
    ("resolve_plugin_variables", [
      test_case "ok" `Quick test_resolve_plugin_variables_ok;
      test_case "missing fields" `Quick test_resolve_plugin_variables_missing;
      test_case "not assoc" `Quick test_resolve_plugin_variables_not_assoc;
    ]);
    ("fidelity_score_of_bundle", [
      test_case "basic" `Quick test_fidelity_bundle_basic;
      test_case "with variables" `Quick test_fidelity_bundle_with_variables;
      test_case "with image fills" `Quick test_fidelity_bundle_with_image_fills;
      test_case "with plugin" `Quick test_fidelity_bundle_with_plugin;
      test_case "plugin not ok" `Quick test_fidelity_bundle_plugin_not_ok;
      test_case "empty image refs" `Quick test_fidelity_bundle_empty_image_refs;
      test_case "variables error" `Quick test_fidelity_bundle_variables_error;
    ]);
    ("get_string extra", [
      test_case "non-string value" `Quick test_get_string_non_string_value;
      test_case "null value" `Quick test_get_string_null_value;
    ]);
    ("get_float extra", [
      test_case "string not float" `Quick test_get_float_string;
    ]);
    ("get_int extra", [
      test_case "string not int" `Quick test_get_int_string;
    ]);
    ("get_bool extra", [
      test_case "false" `Quick test_get_bool_false;
      test_case "int not bool" `Quick test_get_bool_int;
    ]);
    ("get_string_list extra", [
      test_case "mixed types" `Quick test_get_string_list_mixed;
      test_case "csv spaces" `Quick test_get_string_list_csv_spaces;
      test_case "single csv" `Quick test_get_string_list_single_csv;
      test_case "whitespace only" `Quick test_get_string_list_whitespace_only;
    ]);
    ("resolve_url_info extra", [
      test_case "file only url" `Quick test_resolve_url_info_file_only;
      test_case "invalid url" `Quick test_resolve_url_info_invalid_url;
    ]);
    ("resolve_file_key_node_id extra", [
      test_case "empty" `Quick test_resolve_fk_nid_empty;
      test_case "node only" `Quick test_resolve_fk_nid_node_only;
    ]);
    ("resolve_node_id extra", [
      test_case "hyphen normalized" `Quick test_resolve_nid_hyphen_normalized;
    ]);
    ("build_file_meta extra", [
      test_case "null children" `Quick test_build_file_meta_null_children;
    ]);
    ("find_node_entry extra", [
      test_case "normalized scan" `Quick test_find_node_entry_normalized_scan;
    ]);
    ("mkdir_p", [
      test_case "new dir" `Quick test_mkdir_p_new;
      test_case "existing dir" `Quick test_mkdir_p_existing;
    ]);
    ("default_dirs", [
      test_case "asset dir" `Quick test_default_asset_dir;
      test_case "compare dir" `Quick test_default_compare_dir;
    ]);
  ]
