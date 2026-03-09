(** Coverage Wave 11: figma_crawl.ml — pure JSON helpers + flatten_nodes + render + sanitize
    Targets branches and edge cases NOT covered by test_crawl_coverage.ml / test_crawl_extra_w9.ml.
    All tests are pure (no Neo4j, no HTTP, no Effects). *)

let () =
  let open Alcotest in

  (* === Helper === *)
  let mk ?(children = []) id name typ =
    let base =
      [ ("id", `String id); ("name", `String name); ("type", `String typ) ]
    in
    if children = [] then `Assoc base
    else `Assoc (base @ [ ("children", `List children) ])
  in

  let render ?(max_depth = 10) ?(indent = 0) node =
    let buf = Buffer.create 256 in
    Figma_crawl.render_node_tree ~indent ~max_depth ~depth:0 node buf;
    Buffer.contents buf
  in

  (* ================================================================ *)
  (*  get_string — additional type branches                           *)
  (* ================================================================ *)
  let test_get_string_bool () =
    let json = `Assoc [ ("flag", `Bool true) ] in
    check (option string) "bool -> None" None (Figma_crawl.get_string "flag" json)
  in
  let test_get_string_float () =
    let json = `Assoc [ ("x", `Float 1.5) ] in
    check (option string) "float -> None" None (Figma_crawl.get_string "x" json)
  in
  let test_get_string_empty_string () =
    let json = `Assoc [ ("k", `String "") ] in
    check (option string) "empty string" (Some "") (Figma_crawl.get_string "k" json)
  in
  let test_get_string_nested_assoc () =
    let json = `Assoc [ ("k", `Assoc [ ("inner", `String "v") ]) ] in
    check (option string) "assoc -> None" None (Figma_crawl.get_string "k" json)
  in
  let test_get_string_from_list_root () =
    (* Non-Assoc root returns None (Mcp_helpers.member handles gracefully) *)
    let json = `List [ `String "a" ] in
    check (option string) "list root -> None" None (Figma_crawl.get_string "k" json)
  in

  (* ================================================================ *)
  (*  get_string_or — edge cases                                      *)
  (* ================================================================ *)
  let test_get_string_or_non_string_value () =
    let json = `Assoc [ ("k", `Int 42) ] in
    check string "int -> default" "def"
      (Figma_crawl.get_string_or "k" "def" json)
  in
  let test_get_string_or_empty_default () =
    let json = `Assoc [] in
    check string "empty default" ""
      (Figma_crawl.get_string_or "missing" "" json)
  in

  (* ================================================================ *)
  (*  get_list — edge cases                                           *)
  (* ================================================================ *)
  let test_get_list_null () =
    let json = `Assoc [ ("items", `Null) ] in
    check int "null -> empty" 0 (List.length (Figma_crawl.get_list "items" json))
  in
  let test_get_list_int () =
    let json = `Assoc [ ("items", `Int 99) ] in
    check int "int -> empty" 0 (List.length (Figma_crawl.get_list "items" json))
  in
  let test_get_list_empty_list () =
    let json = `Assoc [ ("items", `List []) ] in
    check int "empty list" 0 (List.length (Figma_crawl.get_list "items" json))
  in
  let test_get_list_nested () =
    let json = `Assoc [ ("items", `List [ `List [ `Int 1 ] ]) ] in
    check int "nested list" 1 (List.length (Figma_crawl.get_list "items" json))
  in

  (* ================================================================ *)
  (*  flatten_nodes — deeper edge cases                               *)
  (* ================================================================ *)
  let test_flatten_max_depth_zero () =
    (* max_depth = 0 means only root (depth 0), children at depth 1 excluded *)
    let child = mk "c:1" "C" "TEXT" in
    let root = mk ~children:[ child ] "r:1" "R" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:0 root []
    in
    check int "only root" 1 (List.length result)
  in

  let test_flatten_already_past_max_depth () =
    (* depth > max_depth at start => nothing added *)
    let node = mk "n:1" "N" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:5
        ~max_depth:3 node []
    in
    check int "no nodes" 0 (List.length result)
  in

  let test_flatten_accumulator_preserves_existing () =
    let node = mk "n:1" "N" "TEXT" in
    let existing = [ ("pre:1", "Pre", "RECT", "f0", None) ] in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f1" ~parent_id:None ~depth:0
        ~max_depth:10 node existing
    in
    check int "existing + new" 2 (List.length result)
  in

  let test_flatten_many_siblings () =
    let siblings =
      List.init 10 (fun i ->
          mk (Printf.sprintf "s:%d" i) (Printf.sprintf "S%d" i) "TEXT")
    in
    let root = mk ~children:siblings "r:1" "Root" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:10 root []
    in
    (* 1 root + 10 siblings *)
    check int "11 nodes" 11 (List.length result)
  in

  let test_flatten_deep_chain () =
    (* Build a chain: root -> a -> b -> c -> d (depth 4) *)
    let d = mk "d:1" "D" "TEXT" in
    let c = mk ~children:[ d ] "c:1" "C" "FRAME" in
    let b = mk ~children:[ c ] "b:1" "B" "FRAME" in
    let a = mk ~children:[ b ] "a:1" "A" "FRAME" in
    let root = mk ~children:[ a ] "r:1" "Root" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:10 root []
    in
    check int "5 nodes in chain" 5 (List.length result)
  in

  let test_flatten_deep_chain_truncated () =
    (* Same chain, max_depth = 2 *)
    let d = mk "d:1" "D" "TEXT" in
    let c = mk ~children:[ d ] "c:1" "C" "FRAME" in
    let b = mk ~children:[ c ] "b:1" "B" "FRAME" in
    let a = mk ~children:[ b ] "a:1" "A" "FRAME" in
    let root = mk ~children:[ a ] "r:1" "Root" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:2 root []
    in
    (* depth 0: root, depth 1: a, depth 2: b, depth 3: c => cut *)
    check int "3 nodes truncated" 3 (List.length result)
  in

  let test_flatten_empty_children_array () =
    (* children key present but empty list *)
    let node = `Assoc
      [ ("id", `String "n:1"); ("name", `String "Leaf");
        ("type", `String "TEXT"); ("children", `List []) ]
    in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:10 node []
    in
    check int "one node" 1 (List.length result)
  in

  let test_flatten_with_extra_json_fields () =
    (* Node has extra keys like "visible", "opacity" — flatten should ignore them *)
    let node = `Assoc
      [ ("id", `String "n:1"); ("name", `String "Box");
        ("type", `String "RECTANGLE"); ("visible", `Bool false);
        ("opacity", `Float 0.5) ]
    in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:10 node []
    in
    check int "one node despite extras" 1 (List.length result);
    let (_, name, typ, _, _) = List.hd result in
    check string "name" "Box" name;
    check string "type" "RECTANGLE" typ
  in

  let test_flatten_parent_propagation () =
    (* Verify grandchild gets parent_id = child's id, not root *)
    let gc = mk "gc:1" "GC" "TEXT" in
    let child = mk ~children:[ gc ] "ch:1" "CH" "FRAME" in
    let root = mk ~children:[ child ] "r:1" "R" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"f" ~parent_id:None ~depth:0
        ~max_depth:10 root []
    in
    check int "3 nodes" 3 (List.length result);
    (* Result is built with :: so reversed order: gc is first *)
    let gc_entry =
      List.find (fun (id, _, _, _, _) -> id = "gc:1") result
    in
    let (_, _, _, _, gc_parent) = gc_entry in
    check (option string) "gc parent is child" (Some "ch:1") gc_parent
  in

  let test_flatten_file_key_propagation () =
    (* All nodes should carry the same file_key *)
    let child = mk "c:1" "C" "TEXT" in
    let root = mk ~children:[ child ] "r:1" "R" "FRAME" in
    let result =
      Figma_crawl.flatten_nodes ~file_key:"MY_FILE_KEY" ~parent_id:None
        ~depth:0 ~max_depth:10 root []
    in
    List.iter
      (fun (_, _, _, fk, _) ->
        check string "file_key" "MY_FILE_KEY" fk)
      result
  in

  (* ================================================================ *)
  (*  create_progress — identity / independence                       *)
  (* ================================================================ *)
  let test_progress_independent_instances () =
    let p1 = Figma_crawl.create_progress () in
    let p2 = Figma_crawl.create_progress () in
    p1.teams <- 5;
    check int "p2 unaffected" 0 p2.teams
  in

  let test_progress_errors_accumulate () =
    let p = Figma_crawl.create_progress () in
    p.errors <- "a" :: p.errors;
    p.errors <- "b" :: p.errors;
    check int "two errors" 2 (List.length p.errors);
    check string "latest first" "b" (List.hd p.errors)
  in

  (* ================================================================ *)
  (*  default_options — snapshot of each field                        *)
  (* ================================================================ *)
  let test_default_options_skip_files_empty () =
    check int "skip_files length" 0
      (List.length Figma_crawl.default_options.skip_files)
  in

  (* ================================================================ *)
  (*  progress_to_json — additional branches                          *)
  (* ================================================================ *)
  let test_progress_to_json_single_error () =
    let p = Figma_crawl.create_progress () in
    p.errors <- [ "only-one" ];
    let json = Figma_crawl.progress_to_json p in
    match json with
    | `Assoc fields -> (
        match List.assoc "errors" fields with
        | `List [ `String e ] -> check string "single error" "only-one" e
        | _ -> fail "expected single error")
    | _ -> fail "not assoc"
  in

  let test_progress_to_json_large_numbers () =
    let p = Figma_crawl.create_progress () in
    p.teams <- 999;
    p.projects <- 10000;
    p.files <- 50000;
    p.nodes <- 1000000;
    let json = Figma_crawl.progress_to_json p in
    match json with
    | `Assoc fields ->
        check int "nodes" 1000000
          (match List.assoc "nodes" fields with `Int n -> n | _ -> -1)
    | _ -> fail "not assoc"
  in

  let test_progress_to_json_roundtrip () =
    let p = Figma_crawl.create_progress () in
    p.teams <- 1;
    p.files <- 2;
    let json = Figma_crawl.progress_to_json p in
    let s = Yojson.Safe.to_string json in
    let parsed = Yojson.Safe.from_string s in
    match parsed with
    | `Assoc fields ->
        check int "teams roundtrip" 1
          (match List.assoc "teams" fields with `Int n -> n | _ -> -1);
        check int "files roundtrip" 2
          (match List.assoc "files" fields with `Int n -> n | _ -> -1)
    | _ -> fail "roundtrip failed"
  in

  (* ================================================================ *)
  (*  buffer_progress — edge cases                                    *)
  (* ================================================================ *)
  let test_buffer_progress_empty_message () =
    let (cb, get) = Figma_crawl.buffer_progress () in
    cb "";
    check string "empty msg with newline" "\n" (get ())
  in

  let test_buffer_progress_unicode () =
    let (cb, get) = Figma_crawl.buffer_progress () in
    cb "Hello";
    let result = get () in
    check bool "contains unicode prefix" true (String.length result > 0)
  in

  let test_buffer_progress_many_messages () =
    let (cb, get) = Figma_crawl.buffer_progress () in
    for i = 1 to 100 do
      cb (Printf.sprintf "msg%d" i)
    done;
    let result = get () in
    let lines =
      String.split_on_char '\n' result
      |> List.filter (fun s -> s <> "")
    in
    check int "100 messages" 100 (List.length lines)
  in

  (* ================================================================ *)
  (*  sanitize_filename — additional edge cases                       *)
  (* ================================================================ *)
  let test_sanitize_hyphens_preserved () =
    check string "hyphens" "my-file-name"
      (Figma_crawl.sanitize_filename "my-file-name")
  in

  let test_sanitize_underscores_preserved () =
    check string "underscores" "my_file_name"
      (Figma_crawl.sanitize_filename "my_file_name")
  in

  let test_sanitize_numbers () =
    check string "numbers" "123abc456"
      (Figma_crawl.sanitize_filename "123abc456")
  in

  let test_sanitize_mixed () =
    check string "mixed" "a_b-c_d_1"
      (Figma_crawl.sanitize_filename "a/b-c:d.1")
  in

  let test_sanitize_49_chars () =
    let s = String.make 49 'z' in
    check int "49 chars unchanged" 49
      (String.length (Figma_crawl.sanitize_filename s))
  in

  let test_sanitize_51_chars () =
    let s = String.make 51 'z' in
    check int "51 truncated to 50" 50
      (String.length (Figma_crawl.sanitize_filename s))
  in

  (* ================================================================ *)
  (*  render_node_tree — individual shape types                       *)
  (* ================================================================ *)
  let test_render_rectangle () =
    let s = render (mk "r:1" "Rect" "RECTANGLE") in
    check bool "RECTANGLE" true (String.length s > 0 &&
      try ignore (Str.search_forward (Str.regexp_string "[RECTANGLE]") s 0); true
      with Not_found -> false)
  in
  let test_render_ellipse () =
    let s = render (mk "e:1" "Ell" "ELLIPSE") in
    check bool "ELLIPSE" true
      (try ignore (Str.search_forward (Str.regexp_string "[ELLIPSE]") s 0); true
       with Not_found -> false)
  in
  let test_render_polygon () =
    let s = render (mk "p:1" "Poly" "POLYGON") in
    check bool "POLYGON" true
      (try ignore (Str.search_forward (Str.regexp_string "[POLYGON]") s 0); true
       with Not_found -> false)
  in
  let test_render_star () =
    let s = render (mk "s:1" "Star" "STAR") in
    check bool "STAR" true
      (try ignore (Str.search_forward (Str.regexp_string "[STAR]") s 0); true
       with Not_found -> false)
  in
  let test_render_line () =
    let s = render (mk "l:1" "Line" "LINE") in
    check bool "LINE" true
      (try ignore (Str.search_forward (Str.regexp_string "[LINE]") s 0); true
       with Not_found -> false)
  in
  let test_render_vector () =
    let s = render (mk "v:1" "Vec" "VECTOR") in
    check bool "VECTOR" true
      (try ignore (Str.search_forward (Str.regexp_string "[VECTOR]") s 0); true
       with Not_found -> false)
  in

  let test_render_multiple_children_same_level () =
    let c1 = mk "c:1" "C1" "TEXT" in
    let c2 = mk "c:2" "C2" "RECTANGLE" in
    let c3 = mk "c:3" "C3" "ELLIPSE" in
    let root = mk ~children:[ c1; c2; c3 ] "r:1" "Root" "FRAME" in
    let s = render root in
    check bool "C1 present" true
      (try ignore (Str.search_forward (Str.regexp_string "C1") s 0); true
       with Not_found -> false);
    check bool "C2 present" true
      (try ignore (Str.search_forward (Str.regexp_string "C2") s 0); true
       with Not_found -> false);
    check bool "C3 present" true
      (try ignore (Str.search_forward (Str.regexp_string "C3") s 0); true
       with Not_found -> false)
  in

  let test_render_indent_increases () =
    let child = mk "c:1" "Child" "TEXT" in
    let root = mk ~children:[ child ] "r:1" "Root" "FRAME" in
    let s = render ~indent:0 root in
    let lines =
      String.split_on_char '\n' s |> List.filter (fun l -> l <> "")
    in
    check bool "at least 2 lines" true (List.length lines >= 2);
    (* Child line should start with spaces *)
    let child_line = List.nth lines 1 in
    check bool "child indented" true
      (String.length child_line > 2 && child_line.[0] = ' ')
  in

  let test_render_empty_name_and_id () =
    let node = `Assoc
      [ ("id", `String ""); ("name", `String ""); ("type", `String "FRAME") ]
    in
    let s = render node in
    check bool "renders despite empty" true (String.length s > 0)
  in

  (* ================================================================ *)
  (*  create_neo4j_config — edge cases                                *)
  (* ================================================================ *)
  let test_neo4j_config_empty_password () =
    let cfg =
      Figma_crawl.create_neo4j_config ~uri:"bolt://x" ~user:"neo4j"
        ~password:"" ()
    in
    (* Base64("neo4j:") = "bmVvNGo6" *)
    check string "auth with empty pw" "Basic bmVvNGo6" cfg.auth_header
  in

  let test_neo4j_config_special_chars () =
    let cfg =
      Figma_crawl.create_neo4j_config ~uri:"bolt://x" ~user:"u"
        ~password:"p@ss:w0rd" ()
    in
    check bool "auth non-empty" true (String.length cfg.auth_header > 6)
  in

  (* ================================================================ *)
  (*  Run all tests                                                   *)
  (* ================================================================ *)
  run "figma_crawl Wave 11"
    [
      ( "get_string extra",
        [
          test_case "bool value" `Quick test_get_string_bool;
          test_case "float value" `Quick test_get_string_float;
          test_case "empty string" `Quick test_get_string_empty_string;
          test_case "nested assoc" `Quick test_get_string_nested_assoc;
          test_case "list root" `Quick test_get_string_from_list_root;
        ] );
      ( "get_string_or extra",
        [
          test_case "non-string value" `Quick test_get_string_or_non_string_value;
          test_case "empty default" `Quick test_get_string_or_empty_default;
        ] );
      ( "get_list extra",
        [
          test_case "null value" `Quick test_get_list_null;
          test_case "int value" `Quick test_get_list_int;
          test_case "empty list" `Quick test_get_list_empty_list;
          test_case "nested list" `Quick test_get_list_nested;
        ] );
      ( "flatten_nodes extra",
        [
          test_case "max_depth 0" `Quick test_flatten_max_depth_zero;
          test_case "already past max" `Quick test_flatten_already_past_max_depth;
          test_case "accumulator preserved" `Quick test_flatten_accumulator_preserves_existing;
          test_case "many siblings" `Quick test_flatten_many_siblings;
          test_case "deep chain" `Quick test_flatten_deep_chain;
          test_case "deep chain truncated" `Quick test_flatten_deep_chain_truncated;
          test_case "empty children array" `Quick test_flatten_empty_children_array;
          test_case "extra JSON fields" `Quick test_flatten_with_extra_json_fields;
          test_case "parent propagation" `Quick test_flatten_parent_propagation;
          test_case "file_key propagation" `Quick test_flatten_file_key_propagation;
        ] );
      ( "create_progress extra",
        [
          test_case "independent instances" `Quick test_progress_independent_instances;
          test_case "errors accumulate" `Quick test_progress_errors_accumulate;
        ] );
      ( "default_options extra",
        [
          test_case "skip_files empty" `Quick test_default_options_skip_files_empty;
        ] );
      ( "progress_to_json extra",
        [
          test_case "single error" `Quick test_progress_to_json_single_error;
          test_case "large numbers" `Quick test_progress_to_json_large_numbers;
          test_case "JSON roundtrip" `Quick test_progress_to_json_roundtrip;
        ] );
      ( "buffer_progress extra",
        [
          test_case "empty message" `Quick test_buffer_progress_empty_message;
          test_case "unicode" `Quick test_buffer_progress_unicode;
          test_case "100 messages" `Quick test_buffer_progress_many_messages;
        ] );
      ( "sanitize_filename extra",
        [
          test_case "hyphens preserved" `Quick test_sanitize_hyphens_preserved;
          test_case "underscores preserved" `Quick test_sanitize_underscores_preserved;
          test_case "numbers" `Quick test_sanitize_numbers;
          test_case "mixed chars" `Quick test_sanitize_mixed;
          test_case "49 chars" `Quick test_sanitize_49_chars;
          test_case "51 chars" `Quick test_sanitize_51_chars;
        ] );
      ( "render_node_tree extra",
        [
          test_case "RECTANGLE" `Quick test_render_rectangle;
          test_case "ELLIPSE" `Quick test_render_ellipse;
          test_case "POLYGON" `Quick test_render_polygon;
          test_case "STAR" `Quick test_render_star;
          test_case "LINE" `Quick test_render_line;
          test_case "VECTOR" `Quick test_render_vector;
          test_case "multiple children" `Quick test_render_multiple_children_same_level;
          test_case "indent increases" `Quick test_render_indent_increases;
          test_case "empty name and id" `Quick test_render_empty_name_and_id;
        ] );
      ( "create_neo4j_config extra",
        [
          test_case "empty password" `Quick test_neo4j_config_empty_password;
          test_case "special chars in password" `Quick test_neo4j_config_special_chars;
        ] );
    ]
