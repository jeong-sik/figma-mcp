(** Coverage Wave 5: mcp_protocol_eio.ml — CORS module + pure helpers.
    All functions under test are pure (no I/O, no Eio runtime needed). *)

open Figma_protocol_eio

(* ── Alcotest testables ───────────────────────────────────── *)

let origin_value_pp fmt = function
  | Cors.Origin_null -> Format.fprintf fmt "Origin_null"
  | Cors.Origin { scheme; host; port } ->
      Format.fprintf fmt "Origin(%s, %s, %s)" scheme host
        (match port with Some p -> string_of_int p | None -> "None")

let origin_value_eq a b =
  match (a, b) with
  | Cors.Origin_null, Cors.Origin_null -> true
  | Cors.Origin a, Cors.Origin b ->
      a.scheme = b.scheme && a.host = b.host && a.port = b.port
  | _ -> false

let origin_value_testable =
  Alcotest.testable origin_value_pp origin_value_eq

let origin_pattern_pp fmt = function
  | Cors.Any -> Format.fprintf fmt "Any"
  | Cors.Null -> Format.fprintf fmt "Null"
  | Cors.Exact { scheme; host; port } ->
      Format.fprintf fmt "Exact(%s, %s, %s)" scheme host
        (match port with Some p -> string_of_int p | None -> "None")
  | Cors.Any_port { scheme; host } ->
      Format.fprintf fmt "Any_port(%s, %s)" scheme host

let origin_pattern_eq a b =
  match (a, b) with
  | Cors.Any, Cors.Any | Cors.Null, Cors.Null -> true
  | Cors.Exact a, Cors.Exact b ->
      a.scheme = b.scheme && a.host = b.host && a.port = b.port
  | Cors.Any_port a, Cors.Any_port b ->
      a.scheme = b.scheme && a.host = b.host
  | _ -> false

let origin_pattern_testable =
  Alcotest.testable origin_pattern_pp origin_pattern_eq

let trio =
  Alcotest.testable
    (fun fmt (s, h, p) ->
      Format.fprintf fmt "(%s, %s, %s)" s h
        (match p with Some p -> string_of_int p | None -> "None"))
    (fun (s1, h1, p1) (s2, h2, p2) -> s1 = s2 && h1 = h2 && p1 = p2)

let classify_pp fmt = function
  | `Request -> Format.pp_print_string fmt "Request"
  | `Response -> Format.pp_print_string fmt "Response"
  | `Notification -> Format.pp_print_string fmt "Notification"
  | `Unknown -> Format.pp_print_string fmt "Unknown"

let classify_eq a b =
  match (a, b) with
  | `Request, `Request
  | `Response, `Response
  | `Notification, `Notification
  | `Unknown, `Unknown -> true
  | _ -> false

let classify_testable =
  Alcotest.testable classify_pp classify_eq

(* ── Test suites ──────────────────────────────────────────── *)

(* 1. default_port_for_scheme *)
let test_default_port_http () =
  Alcotest.(check int) "http" 80 (Cors.default_port_for_scheme "http")

let test_default_port_https () =
  Alcotest.(check int) "https" 443 (Cors.default_port_for_scheme "https")

let test_default_port_ftp () =
  Alcotest.(check int) "ftp" 0 (Cors.default_port_for_scheme "ftp")

let test_default_port_empty () =
  Alcotest.(check int) "empty" 0 (Cors.default_port_for_scheme "")

let test_default_port_ws () =
  Alcotest.(check int) "ws" 0 (Cors.default_port_for_scheme "ws")

(* 2. normalize_lower *)
let test_normalize_lower_mixed () =
  Alcotest.(check string) "lower" "hello" (Cors.normalize_lower "HeLLo")

let test_normalize_lower_already () =
  Alcotest.(check string) "already" "abc" (Cors.normalize_lower "abc")

let test_normalize_lower_empty () =
  Alcotest.(check string) "empty" "" (Cors.normalize_lower "")

(* 3. parse_origin_components *)
let test_poc_http () =
  Alcotest.(check (option trio)) "http://example.com"
    (Some ("http", "example.com", None))
    (Cors.parse_origin_components "http://example.com")

let test_poc_https () =
  Alcotest.(check (option trio)) "https://example.com"
    (Some ("https", "example.com", None))
    (Cors.parse_origin_components "https://example.com")

let test_poc_with_port () =
  Alcotest.(check (option trio)) "port 3000"
    (Some ("http", "localhost", Some 3000))
    (Cors.parse_origin_components "http://localhost:3000")

let test_poc_with_path () =
  Alcotest.(check (option trio)) "path rejected"
    None
    (Cors.parse_origin_components "http://example.com/foo")

let test_poc_with_query () =
  Alcotest.(check (option trio)) "query rejected"
    None
    (Cors.parse_origin_components "http://example.com?q=1")

let test_poc_with_fragment () =
  Alcotest.(check (option trio)) "fragment rejected"
    None
    (Cors.parse_origin_components "http://example.com#frag")

let test_poc_ftp () =
  Alcotest.(check (option trio)) "ftp rejected"
    None
    (Cors.parse_origin_components "ftp://example.com")

let test_poc_no_scheme () =
  Alcotest.(check (option trio)) "no scheme"
    None
    (Cors.parse_origin_components "example.com")

let test_poc_empty () =
  Alcotest.(check (option trio)) "empty string"
    None
    (Cors.parse_origin_components "")

let test_poc_uppercase () =
  Alcotest.(check (option trio)) "uppercase normalized"
    (Some ("https", "example.com", None))
    (Cors.parse_origin_components "HTTPS://EXAMPLE.COM")

let test_poc_trailing_slash () =
  (* Uri.path on "http://example.com/" is "/" which the code allows *)
  Alcotest.(check (option trio)) "trailing slash"
    (Some ("http", "example.com", None))
    (Cors.parse_origin_components "http://example.com/")

let test_poc_ws_scheme () =
  Alcotest.(check (option trio)) "ws rejected"
    None
    (Cors.parse_origin_components "ws://example.com")

let test_poc_https_with_port () =
  Alcotest.(check (option trio)) "https with port 8443"
    (Some ("https", "example.com", Some 8443))
    (Cors.parse_origin_components "https://example.com:8443")

(* 4. parse_origin_value *)
let test_pov_null () =
  Alcotest.(check (option origin_value_testable)) "null"
    (Some Cors.Origin_null)
    (Cors.parse_origin_value "null")

let test_pov_null_trimmed () =
  Alcotest.(check (option origin_value_testable)) "null trimmed"
    (Some Cors.Origin_null)
    (Cors.parse_origin_value "  null  ")

let test_pov_valid_http () =
  Alcotest.(check (option origin_value_testable)) "valid http"
    (Some (Cors.Origin { scheme = "http"; host = "localhost"; port = Some 3000 }))
    (Cors.parse_origin_value "http://localhost:3000")

let test_pov_valid_https () =
  Alcotest.(check (option origin_value_testable)) "valid https"
    (Some (Cors.Origin { scheme = "https"; host = "example.com"; port = None }))
    (Cors.parse_origin_value "https://example.com")

let test_pov_invalid () =
  Alcotest.(check (option origin_value_testable)) "invalid"
    None
    (Cors.parse_origin_value "ftp://example.com")

let test_pov_empty () =
  Alcotest.(check (option origin_value_testable)) "empty"
    None
    (Cors.parse_origin_value "")

let test_pov_with_path () =
  Alcotest.(check (option origin_value_testable)) "path rejected"
    None
    (Cors.parse_origin_value "https://example.com/path")

(* 5. parse_pattern *)
let test_pp_wildcard () =
  Alcotest.(check (option origin_pattern_testable)) "wildcard"
    (Some Cors.Any)
    (Cors.parse_pattern "*")

let test_pp_null () =
  Alcotest.(check (option origin_pattern_testable)) "null"
    (Some Cors.Null)
    (Cors.parse_pattern "null")

let test_pp_exact () =
  Alcotest.(check (option origin_pattern_testable)) "exact"
    (Some (Cors.Exact { scheme = "https"; host = "example.com"; port = None }))
    (Cors.parse_pattern "https://example.com")

let test_pp_exact_with_port () =
  Alcotest.(check (option origin_pattern_testable)) "exact with port"
    (Some (Cors.Exact { scheme = "http"; host = "localhost"; port = Some 3000 }))
    (Cors.parse_pattern "http://localhost:3000")

let test_pp_any_port_colon_star () =
  Alcotest.(check (option origin_pattern_testable)) "any_port :*"
    (Some (Cors.Any_port { scheme = "http"; host = "localhost" }))
    (Cors.parse_pattern "http://localhost:*")

let test_pp_any_port_star () =
  (* "http://localhost*" — trailing * without colon *)
  Alcotest.(check (option origin_pattern_testable)) "any_port trailing *"
    (Some (Cors.Any_port { scheme = "http"; host = "localhost" }))
    (Cors.parse_pattern "http://localhost*")

let test_pp_invalid_base () =
  (* "ftp://example.com:*" — ftp not allowed *)
  Alcotest.(check (option origin_pattern_testable)) "invalid base scheme"
    None
    (Cors.parse_pattern "ftp://example.com:*")

let test_pp_trimmed () =
  Alcotest.(check (option origin_pattern_testable)) "trimmed"
    (Some Cors.Any)
    (Cors.parse_pattern "  *  ")

let test_pp_null_trimmed () =
  Alcotest.(check (option origin_pattern_testable)) "null trimmed"
    (Some Cors.Null)
    (Cors.parse_pattern "  null  ")

let test_pp_empty () =
  Alcotest.(check (option origin_pattern_testable)) "empty"
    None
    (Cors.parse_pattern "")

let test_pp_invalid_base_star () =
  (* "garbage*" — not a valid origin base *)
  Alcotest.(check (option origin_pattern_testable)) "garbage star"
    None
    (Cors.parse_pattern "garbage*")

(* 6. normalized_port *)
let test_np_some () =
  Alcotest.(check int) "explicit port" 3000 (Cors.normalized_port "http" (Some 3000))

let test_np_none_http () =
  Alcotest.(check int) "default http" 80 (Cors.normalized_port "http" None)

let test_np_none_https () =
  Alcotest.(check int) "default https" 443 (Cors.normalized_port "https" None)

let test_np_none_unknown () =
  Alcotest.(check int) "default unknown" 0 (Cors.normalized_port "ftp" None)

(* 7. matches_pattern — cross-product *)
let mk_origin scheme host port = Cors.Origin { scheme; host; port }

let test_mp_any_matches_origin () =
  Alcotest.(check bool) "any origin"
    true
    (Cors.matches_pattern (mk_origin "http" "x.com" None) Cors.Any)

let test_mp_any_matches_null () =
  Alcotest.(check bool) "any null"
    true
    (Cors.matches_pattern Cors.Origin_null Cors.Any)

let test_mp_null_matches_null () =
  Alcotest.(check bool) "null null"
    true
    (Cors.matches_pattern Cors.Origin_null Cors.Null)

let test_mp_null_rejects_origin () =
  Alcotest.(check bool) "null vs origin"
    false
    (Cors.matches_pattern (mk_origin "http" "x.com" None) Cors.Null)

let test_mp_null_rejects_exact () =
  (* Origin_null vs Exact: should be false *)
  Alcotest.(check bool) "null vs exact"
    false
    (Cors.matches_pattern Cors.Origin_null
       (Cors.Exact { scheme = "http"; host = "x.com"; port = None }))

let test_mp_null_rejects_any_port () =
  (* Origin_null vs Any_port: should be false *)
  Alcotest.(check bool) "null vs any_port"
    false
    (Cors.matches_pattern Cors.Origin_null
       (Cors.Any_port { scheme = "http"; host = "x.com" }))

let test_mp_exact_same () =
  Alcotest.(check bool) "exact same"
    true
    (Cors.matches_pattern
       (mk_origin "https" "x.com" None)
       (Cors.Exact { scheme = "https"; host = "x.com"; port = None }))

let test_mp_exact_port_match () =
  (* Both explicit port 3000 *)
  Alcotest.(check bool) "exact port match"
    true
    (Cors.matches_pattern
       (mk_origin "http" "localhost" (Some 3000))
       (Cors.Exact { scheme = "http"; host = "localhost"; port = Some 3000 }))

let test_mp_exact_port_mismatch () =
  Alcotest.(check bool) "exact port mismatch"
    false
    (Cors.matches_pattern
       (mk_origin "http" "localhost" (Some 3000))
       (Cors.Exact { scheme = "http"; host = "localhost"; port = Some 4000 }))

let test_mp_exact_default_vs_explicit () =
  (* Origin port=None (default 80), Exact port=Some 80 -> match *)
  Alcotest.(check bool) "default vs explicit 80"
    true
    (Cors.matches_pattern
       (mk_origin "http" "example.com" None)
       (Cors.Exact { scheme = "http"; host = "example.com"; port = Some 80 }))

let test_mp_exact_scheme_mismatch () =
  Alcotest.(check bool) "scheme mismatch"
    false
    (Cors.matches_pattern
       (mk_origin "http" "example.com" None)
       (Cors.Exact { scheme = "https"; host = "example.com"; port = None }))

let test_mp_exact_host_mismatch () =
  Alcotest.(check bool) "host mismatch"
    false
    (Cors.matches_pattern
       (mk_origin "http" "a.com" None)
       (Cors.Exact { scheme = "http"; host = "b.com"; port = None }))

let test_mp_any_port_match () =
  Alcotest.(check bool) "any_port match"
    true
    (Cors.matches_pattern
       (mk_origin "http" "localhost" (Some 9999))
       (Cors.Any_port { scheme = "http"; host = "localhost" }))

let test_mp_any_port_diff_scheme () =
  Alcotest.(check bool) "any_port scheme mismatch"
    false
    (Cors.matches_pattern
       (mk_origin "https" "localhost" None)
       (Cors.Any_port { scheme = "http"; host = "localhost" }))

let test_mp_any_port_diff_host () =
  Alcotest.(check bool) "any_port host mismatch"
    false
    (Cors.matches_pattern
       (mk_origin "http" "other.com" None)
       (Cors.Any_port { scheme = "http"; host = "localhost" }))

(* 8. origin_allowed — uses env vars *)
let with_env vars f =
  let old = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) vars in
  List.iter (fun (k, v) -> Unix.putenv k v) vars;
  let finally () =
    List.iter (fun (k, prev) ->
      match prev with
      | Some v -> Unix.putenv k v
      | None ->
          (* OCaml has no unsetenv; set to empty to "clear" *)
          (try Unix.putenv k "" with _ -> ()))
    old
  in
  match f () with
  | x -> finally (); x
  | exception e -> finally (); raise e

let test_origin_allowed_permissive () =
  with_env [("FIGMA_MCP_CORS_MODE", "permissive")] (fun () ->
    (* In permissive mode, origin_allowed is not called (always allowed) *)
    (* But if we call it directly, it still checks the list *)
    (* Actually origin_allowed reads allowed_origins, not mode.
       Let's set origins to *, which matches everything *)
    ());
  (* Test with restrict mode and matching origin *)
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "*") ]
    (fun () ->
      Alcotest.(check bool) "wildcard allows all"
        true
        (Cors.origin_allowed "https://any.com"))

let test_origin_allowed_restrict_match () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "https://example.com") ]
    (fun () ->
      Alcotest.(check bool) "exact match"
        true
        (Cors.origin_allowed "https://example.com"))

let test_origin_allowed_restrict_no_match () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "https://example.com") ]
    (fun () ->
      Alcotest.(check bool) "no match"
        false
        (Cors.origin_allowed "https://other.com"))

let test_origin_allowed_null () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "null") ]
    (fun () ->
      Alcotest.(check bool) "null allowed"
        true
        (Cors.origin_allowed "null"))

let test_origin_allowed_invalid_origin () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "*") ]
    (fun () ->
      Alcotest.(check bool) "invalid origin"
        false
        (Cors.origin_allowed "not-a-url"))

let test_origin_allowed_port_wildcard () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "http://localhost:*") ]
    (fun () ->
      Alcotest.(check bool) "port wildcard"
        true
        (Cors.origin_allowed "http://localhost:5173"))

(* 9. headers_for_origin_opt — depends on env vars *)
let test_headers_permissive () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "permissive");
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", "false") ]
    (fun () ->
      let headers =
        Cors.headers_for_origin_opt None ~include_methods:false ~include_headers:false
      in
      Alcotest.(check bool) "has allow-origin *"
        true
        (List.assoc_opt "access-control-allow-origin" headers = Some "*");
      (* Wildcard origin should NOT have Vary header *)
      Alcotest.(check bool) "no vary"
        true
        (not (List.mem_assoc "vary" headers)))

let test_headers_restrict_allowed () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "https://example.com");
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", "false") ]
    (fun () ->
      let headers =
        Cors.headers_for_origin_opt (Some "https://example.com")
          ~include_methods:true ~include_headers:false
      in
      Alcotest.(check bool) "has origin"
        true
        (List.assoc_opt "access-control-allow-origin" headers =
         Some "https://example.com");
      Alcotest.(check bool) "has vary"
        true
        (List.assoc_opt "vary" headers = Some "Origin");
      Alcotest.(check bool) "has methods"
        true
        (List.mem_assoc "access-control-allow-methods" headers))

let test_headers_restrict_denied () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "https://example.com");
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", "false") ]
    (fun () ->
      let headers =
        Cors.headers_for_origin_opt (Some "https://evil.com")
          ~include_methods:false ~include_headers:false
      in
      Alcotest.(check int) "empty headers" 0 (List.length headers))

let test_headers_with_methods_and_headers () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "permissive");
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", "false") ]
    (fun () ->
      let headers =
        Cors.headers_for_origin_opt (Some "https://example.com")
          ~include_methods:true ~include_headers:true
      in
      Alcotest.(check bool) "has methods"
        true
        (List.mem_assoc "access-control-allow-methods" headers);
      Alcotest.(check bool) "has headers"
        true
        (List.mem_assoc "access-control-allow-headers" headers))

let test_headers_private_network () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "permissive");
      ("FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK", "true") ]
    (fun () ->
      let headers =
        Cors.headers_for_origin_opt (Some "https://example.com")
          ~include_methods:false ~include_headers:false
      in
      Alcotest.(check bool) "has private network"
        true
        (List.assoc_opt "access-control-allow-private-network" headers =
         Some "true"))

let test_headers_no_origin_restrict () =
  with_env
    [ ("FIGMA_MCP_CORS_MODE", "restrict");
      ("FIGMA_MCP_CORS_ALLOWED_ORIGINS", "https://example.com") ]
    (fun () ->
      let headers =
        Cors.headers_for_origin_opt None ~include_methods:false ~include_headers:false
      in
      Alcotest.(check int) "no origin => empty" 0 (List.length headers))

(* 10. parse_json *)
let test_parse_json_valid () =
  match parse_json {|{"a":1}|} with
  | Ok json ->
      Alcotest.(check string) "round trip"
        {|{"a":1}|}
        (Yojson.Safe.to_string json)
  | Error _ -> Alcotest.fail "expected Ok"

let test_parse_json_empty () =
  match parse_json "" with
  | Ok `Null -> ()
  | _ -> Alcotest.fail "expected Ok Null"

let test_parse_json_whitespace () =
  match parse_json "   " with
  | Ok `Null -> ()
  | _ -> Alcotest.fail "expected Ok Null"

let test_parse_json_invalid () =
  match parse_json "{bad json" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

(* 11. get_string_field *)
let test_gsf_present () =
  let json = `Assoc [("name", `String "hello")] in
  Alcotest.(check (option string)) "found" (Some "hello") (get_string_field "name" json)

let test_gsf_missing () =
  let json = `Assoc [("other", `String "x")] in
  Alcotest.(check (option string)) "missing" None (get_string_field "name" json)

let test_gsf_wrong_type () =
  let json = `Assoc [("name", `Int 42)] in
  Alcotest.(check (option string)) "wrong type" None (get_string_field "name" json)

let test_gsf_not_assoc () =
  Alcotest.(check (option string)) "not assoc" None (get_string_field "name" (`String "x"))

(* 12. get_int_field *)
let test_gif_int () =
  let json = `Assoc [("n", `Int 42)] in
  Alcotest.(check (option int)) "int" (Some 42) (get_int_field "n" json)

let test_gif_float () =
  let json = `Assoc [("n", `Float 3.7)] in
  Alcotest.(check (option int)) "float" (Some 3) (get_int_field "n" json)

let test_gif_missing () =
  let json = `Assoc [] in
  Alcotest.(check (option int)) "missing" None (get_int_field "n" json)

let test_gif_wrong_type () =
  let json = `Assoc [("n", `String "42")] in
  Alcotest.(check (option int)) "wrong type" None (get_int_field "n" json)

let test_gif_not_assoc () =
  Alcotest.(check (option int)) "not assoc" None (get_int_field "n" `Null)

(* 13. get_bool_field *)
let test_gbf_true () =
  let json = `Assoc [("b", `Bool true)] in
  Alcotest.(check (option bool)) "true" (Some true) (get_bool_field "b" json)

let test_gbf_false () =
  let json = `Assoc [("b", `Bool false)] in
  Alcotest.(check (option bool)) "false" (Some false) (get_bool_field "b" json)

let test_gbf_missing () =
  Alcotest.(check (option bool)) "missing" None (get_bool_field "b" (`Assoc []))

let test_gbf_wrong_type () =
  let json = `Assoc [("b", `String "true")] in
  Alcotest.(check (option bool)) "wrong type" None (get_bool_field "b" json)

let test_gbf_not_assoc () =
  Alcotest.(check (option bool)) "not assoc" None (get_bool_field "b" (`List []))

(* 14. get_payload_field *)
let test_gpf_present () =
  let inner = `List [`Int 1; `Int 2] in
  let json = `Assoc [("data", inner)] in
  match get_payload_field "data" json with
  | Some v -> Alcotest.(check string) "payload" (Yojson.Safe.to_string inner) (Yojson.Safe.to_string v)
  | None -> Alcotest.fail "expected Some"

let test_gpf_missing () =
  Alcotest.(check bool) "missing" true
    (get_payload_field "data" (`Assoc []) = None)

let test_gpf_not_assoc () =
  Alcotest.(check bool) "not assoc" true
    (get_payload_field "x" (`Int 1) = None)

(* 15. clamp_poll_ms *)
let test_clamp_poll_normal () =
  Alcotest.(check int) "normal" 5000 (clamp_poll_ms 5000)

let test_clamp_poll_negative () =
  Alcotest.(check int) "negative clamped to 0" 0 (clamp_poll_ms (-100))

let test_clamp_poll_zero () =
  Alcotest.(check int) "zero" 0 (clamp_poll_ms 0)

let test_clamp_poll_huge () =
  let max_ms = Figma_config.Plugin.poll_max_ms in
  Alcotest.(check int) "huge clamped" max_ms (clamp_poll_ms (max_ms + 9999))

let test_clamp_poll_exact_max () =
  let max_ms = Figma_config.Plugin.poll_max_ms in
  Alcotest.(check int) "exact max" max_ms (clamp_poll_ms max_ms)

(* 16. clamp_max_commands *)
let test_clamp_cmd_normal () =
  Alcotest.(check int) "normal" 10 (clamp_max_commands 10)

let test_clamp_cmd_zero () =
  Alcotest.(check int) "zero clamped to 1" 1 (clamp_max_commands 0)

let test_clamp_cmd_negative () =
  Alcotest.(check int) "negative clamped to 1" 1 (clamp_max_commands (-5))

let test_clamp_cmd_huge () =
  let max_c = Figma_config.Plugin.max_commands in
  Alcotest.(check int) "huge clamped" max_c (clamp_max_commands (max_c + 100))

let test_clamp_cmd_one () =
  Alcotest.(check int) "one" 1 (clamp_max_commands 1)

(* 17. compact_json_string *)
let test_compact_valid () =
  let pretty = {|{
  "a": 1,
  "b": 2
}|} in
  Alcotest.(check string) "compacted" {|{"a":1,"b":2}|} (compact_json_string pretty)

let test_compact_already () =
  let s = {|{"x":1}|} in
  Alcotest.(check string) "already compact" s (compact_json_string s)

let test_compact_invalid () =
  let s = "not json at all" in
  Alcotest.(check string) "invalid returned as-is" s (compact_json_string s)

let test_compact_empty () =
  (* empty string is not valid JSON, returned as-is *)
  Alcotest.(check string) "empty" "" (compact_json_string "")

(* 18. format_sse_data *)
let test_fsd_empty () =
  Alcotest.(check string) "empty" "data: " (format_sse_data "")

let test_fsd_single_line () =
  Alcotest.(check string) "single" "data: hello" (format_sse_data "hello")

let test_fsd_json_single () =
  (* JSON gets compacted first, then single line *)
  let s = {|{"a":1}|} in
  Alcotest.(check string) "json single" ("data: " ^ s) (format_sse_data s)

let test_fsd_multiline_non_json () =
  let s = "line1\nline2\nline3" in
  Alcotest.(check string) "multiline non-json"
    "data: line1\ndata: line2\ndata: line3"
    (format_sse_data s)

let test_fsd_json_pretty () =
  (* Pretty JSON gets compacted to single line *)
  let s = "{\n  \"a\": 1\n}" in
  Alcotest.(check string) "pretty json compacted"
    {|data: {"a":1}|}
    (format_sse_data s)

(* 19. classify_message *)
let test_cm_request () =
  let body = {|{"jsonrpc":"2.0","method":"test","id":1}|} in
  Alcotest.(check classify_testable) "request" `Request (classify_message body)

let test_cm_notification_no_id () =
  let body = {|{"jsonrpc":"2.0","method":"test"}|} in
  Alcotest.(check classify_testable) "notification no id"
    `Notification (classify_message body)

let test_cm_notification_null_id () =
  let body = {|{"jsonrpc":"2.0","method":"test","id":null}|} in
  Alcotest.(check classify_testable) "notification null id"
    `Notification (classify_message body)

let test_cm_response_result () =
  let body = {|{"jsonrpc":"2.0","id":1,"result":"ok"}|} in
  Alcotest.(check classify_testable) "response result"
    `Response (classify_message body)

let test_cm_response_error () =
  let body = {|{"jsonrpc":"2.0","id":1,"error":{"code":-1,"message":"fail"}}|} in
  Alcotest.(check classify_testable) "response error"
    `Response (classify_message body)

let test_cm_invalid_json () =
  Alcotest.(check classify_testable) "invalid json"
    `Unknown (classify_message "{bad")

let test_cm_not_object () =
  Alcotest.(check classify_testable) "not object"
    `Unknown (classify_message "[1,2,3]")

let test_cm_no_method_no_result () =
  let body = {|{"id":1}|} in
  Alcotest.(check classify_testable) "no method no result"
    `Unknown (classify_message body)

let test_cm_no_method_no_id () =
  let body = {|{"result":"ok"}|} in
  Alcotest.(check classify_testable) "result but no id"
    `Unknown (classify_message body)

let test_cm_empty () =
  Alcotest.(check classify_testable) "empty string"
    `Unknown (classify_message "")

(* 20. hex_of_bytes *)
let test_hex_empty () =
  Alcotest.(check string) "empty" "" (hex_of_bytes (Bytes.of_string ""))

let test_hex_single () =
  Alcotest.(check string) "0xff" "ff" (hex_of_bytes (Bytes.of_string "\xff"))

let test_hex_hello () =
  (* "Hi" = 0x48 0x69 *)
  Alcotest.(check string) "Hi" "4869" (hex_of_bytes (Bytes.of_string "Hi"))

let test_hex_zeros () =
  Alcotest.(check string) "zeros" "0000" (hex_of_bytes (Bytes.of_string "\x00\x00"))

let test_hex_all_nibbles () =
  (* 0xab 0xcd 0xef *)
  Alcotest.(check string) "abcdef" "abcdef"
    (hex_of_bytes (Bytes.of_string "\xab\xcd\xef"))

(* ── Runner ───────────────────────────────────────────────── *)

let () =
  Alcotest.run "Protocol CORS W5"
    [
      ( "normalize_lower",
        [
          Alcotest.test_case "mixed case" `Quick test_normalize_lower_mixed;
          Alcotest.test_case "already lower" `Quick test_normalize_lower_already;
          Alcotest.test_case "empty" `Quick test_normalize_lower_empty;
        ] );
      ( "default_port",
        [
          Alcotest.test_case "http" `Quick test_default_port_http;
          Alcotest.test_case "https" `Quick test_default_port_https;
          Alcotest.test_case "ftp" `Quick test_default_port_ftp;
          Alcotest.test_case "empty" `Quick test_default_port_empty;
          Alcotest.test_case "ws" `Quick test_default_port_ws;
        ] );
      ( "parse_origin_components",
        [
          Alcotest.test_case "http" `Quick test_poc_http;
          Alcotest.test_case "https" `Quick test_poc_https;
          Alcotest.test_case "with port" `Quick test_poc_with_port;
          Alcotest.test_case "with path" `Quick test_poc_with_path;
          Alcotest.test_case "with query" `Quick test_poc_with_query;
          Alcotest.test_case "with fragment" `Quick test_poc_with_fragment;
          Alcotest.test_case "ftp" `Quick test_poc_ftp;
          Alcotest.test_case "no scheme" `Quick test_poc_no_scheme;
          Alcotest.test_case "empty" `Quick test_poc_empty;
          Alcotest.test_case "uppercase" `Quick test_poc_uppercase;
          Alcotest.test_case "trailing slash" `Quick test_poc_trailing_slash;
          Alcotest.test_case "ws scheme" `Quick test_poc_ws_scheme;
          Alcotest.test_case "https with port" `Quick test_poc_https_with_port;
        ] );
      ( "parse_origin_value",
        [
          Alcotest.test_case "null" `Quick test_pov_null;
          Alcotest.test_case "null trimmed" `Quick test_pov_null_trimmed;
          Alcotest.test_case "valid http" `Quick test_pov_valid_http;
          Alcotest.test_case "valid https" `Quick test_pov_valid_https;
          Alcotest.test_case "invalid" `Quick test_pov_invalid;
          Alcotest.test_case "empty" `Quick test_pov_empty;
          Alcotest.test_case "with path" `Quick test_pov_with_path;
        ] );
      ( "parse_pattern",
        [
          Alcotest.test_case "wildcard" `Quick test_pp_wildcard;
          Alcotest.test_case "null" `Quick test_pp_null;
          Alcotest.test_case "exact" `Quick test_pp_exact;
          Alcotest.test_case "exact with port" `Quick test_pp_exact_with_port;
          Alcotest.test_case "any_port :*" `Quick test_pp_any_port_colon_star;
          Alcotest.test_case "any_port trailing *" `Quick test_pp_any_port_star;
          Alcotest.test_case "invalid base" `Quick test_pp_invalid_base;
          Alcotest.test_case "trimmed" `Quick test_pp_trimmed;
          Alcotest.test_case "null trimmed" `Quick test_pp_null_trimmed;
          Alcotest.test_case "empty" `Quick test_pp_empty;
          Alcotest.test_case "garbage*" `Quick test_pp_invalid_base_star;
        ] );
      ( "normalized_port",
        [
          Alcotest.test_case "explicit" `Quick test_np_some;
          Alcotest.test_case "default http" `Quick test_np_none_http;
          Alcotest.test_case "default https" `Quick test_np_none_https;
          Alcotest.test_case "default unknown" `Quick test_np_none_unknown;
        ] );
      ( "matches_pattern",
        [
          Alcotest.test_case "any matches origin" `Quick test_mp_any_matches_origin;
          Alcotest.test_case "any matches null" `Quick test_mp_any_matches_null;
          Alcotest.test_case "null matches null" `Quick test_mp_null_matches_null;
          Alcotest.test_case "null rejects origin" `Quick test_mp_null_rejects_origin;
          Alcotest.test_case "null rejects exact" `Quick test_mp_null_rejects_exact;
          Alcotest.test_case "null rejects any_port" `Quick test_mp_null_rejects_any_port;
          Alcotest.test_case "exact same" `Quick test_mp_exact_same;
          Alcotest.test_case "exact port match" `Quick test_mp_exact_port_match;
          Alcotest.test_case "exact port mismatch" `Quick test_mp_exact_port_mismatch;
          Alcotest.test_case "default vs explicit 80" `Quick test_mp_exact_default_vs_explicit;
          Alcotest.test_case "scheme mismatch" `Quick test_mp_exact_scheme_mismatch;
          Alcotest.test_case "host mismatch" `Quick test_mp_exact_host_mismatch;
          Alcotest.test_case "any_port match" `Quick test_mp_any_port_match;
          Alcotest.test_case "any_port scheme mismatch" `Quick test_mp_any_port_diff_scheme;
          Alcotest.test_case "any_port host mismatch" `Quick test_mp_any_port_diff_host;
        ] );
      ( "origin_allowed",
        [
          Alcotest.test_case "wildcard" `Quick test_origin_allowed_permissive;
          Alcotest.test_case "restrict match" `Quick test_origin_allowed_restrict_match;
          Alcotest.test_case "restrict no match" `Quick test_origin_allowed_restrict_no_match;
          Alcotest.test_case "null" `Quick test_origin_allowed_null;
          Alcotest.test_case "invalid origin" `Quick test_origin_allowed_invalid_origin;
          Alcotest.test_case "port wildcard" `Quick test_origin_allowed_port_wildcard;
        ] );
      ( "headers_for_origin_opt",
        [
          Alcotest.test_case "permissive" `Quick test_headers_permissive;
          Alcotest.test_case "restrict allowed" `Quick test_headers_restrict_allowed;
          Alcotest.test_case "restrict denied" `Quick test_headers_restrict_denied;
          Alcotest.test_case "methods and headers" `Quick test_headers_with_methods_and_headers;
          Alcotest.test_case "private network" `Quick test_headers_private_network;
          Alcotest.test_case "no origin restrict" `Quick test_headers_no_origin_restrict;
        ] );
      ( "parse_json",
        [
          Alcotest.test_case "valid" `Quick test_parse_json_valid;
          Alcotest.test_case "empty" `Quick test_parse_json_empty;
          Alcotest.test_case "whitespace" `Quick test_parse_json_whitespace;
          Alcotest.test_case "invalid" `Quick test_parse_json_invalid;
        ] );
      ( "get_field_helpers",
        [
          Alcotest.test_case "string present" `Quick test_gsf_present;
          Alcotest.test_case "string missing" `Quick test_gsf_missing;
          Alcotest.test_case "string wrong type" `Quick test_gsf_wrong_type;
          Alcotest.test_case "string not assoc" `Quick test_gsf_not_assoc;
          Alcotest.test_case "int" `Quick test_gif_int;
          Alcotest.test_case "int from float" `Quick test_gif_float;
          Alcotest.test_case "int missing" `Quick test_gif_missing;
          Alcotest.test_case "int wrong type" `Quick test_gif_wrong_type;
          Alcotest.test_case "int not assoc" `Quick test_gif_not_assoc;
          Alcotest.test_case "bool true" `Quick test_gbf_true;
          Alcotest.test_case "bool false" `Quick test_gbf_false;
          Alcotest.test_case "bool missing" `Quick test_gbf_missing;
          Alcotest.test_case "bool wrong type" `Quick test_gbf_wrong_type;
          Alcotest.test_case "bool not assoc" `Quick test_gbf_not_assoc;
          Alcotest.test_case "payload present" `Quick test_gpf_present;
          Alcotest.test_case "payload missing" `Quick test_gpf_missing;
          Alcotest.test_case "payload not assoc" `Quick test_gpf_not_assoc;
        ] );
      ( "clamp_helpers",
        [
          Alcotest.test_case "poll normal" `Quick test_clamp_poll_normal;
          Alcotest.test_case "poll negative" `Quick test_clamp_poll_negative;
          Alcotest.test_case "poll zero" `Quick test_clamp_poll_zero;
          Alcotest.test_case "poll huge" `Quick test_clamp_poll_huge;
          Alcotest.test_case "poll exact max" `Quick test_clamp_poll_exact_max;
          Alcotest.test_case "cmd normal" `Quick test_clamp_cmd_normal;
          Alcotest.test_case "cmd zero" `Quick test_clamp_cmd_zero;
          Alcotest.test_case "cmd negative" `Quick test_clamp_cmd_negative;
          Alcotest.test_case "cmd huge" `Quick test_clamp_cmd_huge;
          Alcotest.test_case "cmd one" `Quick test_clamp_cmd_one;
        ] );
      ( "compact_json_string",
        [
          Alcotest.test_case "valid" `Quick test_compact_valid;
          Alcotest.test_case "already compact" `Quick test_compact_already;
          Alcotest.test_case "invalid" `Quick test_compact_invalid;
          Alcotest.test_case "empty" `Quick test_compact_empty;
        ] );
      ( "format_sse_data",
        [
          Alcotest.test_case "empty" `Quick test_fsd_empty;
          Alcotest.test_case "single line" `Quick test_fsd_single_line;
          Alcotest.test_case "json single" `Quick test_fsd_json_single;
          Alcotest.test_case "multiline non-json" `Quick test_fsd_multiline_non_json;
          Alcotest.test_case "json pretty" `Quick test_fsd_json_pretty;
        ] );
      ( "classify_message",
        [
          Alcotest.test_case "request" `Quick test_cm_request;
          Alcotest.test_case "notification no id" `Quick test_cm_notification_no_id;
          Alcotest.test_case "notification null id" `Quick test_cm_notification_null_id;
          Alcotest.test_case "response result" `Quick test_cm_response_result;
          Alcotest.test_case "response error" `Quick test_cm_response_error;
          Alcotest.test_case "invalid json" `Quick test_cm_invalid_json;
          Alcotest.test_case "not object" `Quick test_cm_not_object;
          Alcotest.test_case "no method no result" `Quick test_cm_no_method_no_result;
          Alcotest.test_case "result but no id" `Quick test_cm_no_method_no_id;
          Alcotest.test_case "empty string" `Quick test_cm_empty;
        ] );
      ( "hex_of_bytes",
        [
          Alcotest.test_case "empty" `Quick test_hex_empty;
          Alcotest.test_case "0xff" `Quick test_hex_single;
          Alcotest.test_case "Hi" `Quick test_hex_hello;
          Alcotest.test_case "zeros" `Quick test_hex_zeros;
          Alcotest.test_case "abcdef" `Quick test_hex_all_nibbles;
        ] );
    ]
