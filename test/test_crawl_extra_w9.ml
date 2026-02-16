(** Extra coverage tests for figma_crawl.ml — Wave 9
    Targets: progress_to_json, buffer_progress, render_node_tree,
    sanitize_filename, mkdir_p, export_node_to_fs, stdout_progress.
    Focus on pure functions and type-branch coverage. *)

open Printf

let () =
  let open Alcotest in

  (* ============== progress_to_json ============== *)
  let test_progress_to_json_empty () =
    let p = Figma_crawl.create_progress () in
    let json = Figma_crawl.progress_to_json p in
    let s = Yojson.Safe.to_string json in
    check bool "has teams" true (String.length s > 0);
    match json with
    | `Assoc fields ->
        check int "teams" 0 (match List.assoc "teams" fields with `Int n -> n | _ -> -1);
        check int "projects" 0 (match List.assoc "projects" fields with `Int n -> n | _ -> -1);
        check int "files" 0 (match List.assoc "files" fields with `Int n -> n | _ -> -1);
        check int "nodes" 0 (match List.assoc "nodes" fields with `Int n -> n | _ -> -1);
        (match List.assoc "errors" fields with
         | `List l -> check int "errors empty" 0 (List.length l)
         | _ -> fail "errors not list")
    | _ -> fail "not assoc"
  in

  let test_progress_to_json_with_data () =
    let p = Figma_crawl.create_progress () in
    p.teams <- 2;
    p.projects <- 5;
    p.files <- 15;
    p.nodes <- 200;
    p.errors <- ["err1"; "err2"];
    let json = Figma_crawl.progress_to_json p in
    match json with
    | `Assoc fields ->
        check int "teams" 2 (match List.assoc "teams" fields with `Int n -> n | _ -> -1);
        check int "projects" 5 (match List.assoc "projects" fields with `Int n -> n | _ -> -1);
        check int "files" 15 (match List.assoc "files" fields with `Int n -> n | _ -> -1);
        check int "nodes" 200 (match List.assoc "nodes" fields with `Int n -> n | _ -> -1);
        (match List.assoc "errors" fields with
         | `List l ->
             check int "2 errors" 2 (List.length l);
             check string "err1" "err1"
               (match List.nth l 0 with `String s -> s | _ -> "");
             check string "err2" "err2"
               (match List.nth l 1 with `String s -> s | _ -> "")
         | _ -> fail "errors not list")
    | _ -> fail "not assoc"
  in

  (* ============== buffer_progress ============== *)
  let test_buffer_progress_empty () =
    let (_callback, get) = Figma_crawl.buffer_progress () in
    check string "empty" "" (get ())
  in

  let test_buffer_progress_messages () =
    let (callback, get) = Figma_crawl.buffer_progress () in
    callback "msg1";
    callback "msg2";
    let result = get () in
    check bool "has msg1" true (String.length result > 0);
    check bool "contains msg1" true
      (try let _ = Str.search_forward (Str.regexp_string "msg1") result 0 in true
       with Not_found -> false);
    check bool "contains msg2" true
      (try let _ = Str.search_forward (Str.regexp_string "msg2") result 0 in true
       with Not_found -> false)
  in

  let test_buffer_progress_newlines () =
    let (callback, get) = Figma_crawl.buffer_progress () in
    callback "line1";
    callback "line2";
    let result = get () in
    (* Each message gets a newline appended *)
    check string "result" "line1\nline2\n" result
  in

  (* ============== sanitize_filename ============== *)
  let test_sanitize_basic () =
    check string "alpha" "hello" (Figma_crawl.sanitize_filename "hello")
  in

  let test_sanitize_spaces () =
    check string "spaces" "hello_world" (Figma_crawl.sanitize_filename "hello world")
  in

  let test_sanitize_special_chars () =
    check string "special" "a_b_c_d" (Figma_crawl.sanitize_filename "a/b\\c:d")
  in

  let test_sanitize_dots () =
    check string "dots" "file_json" (Figma_crawl.sanitize_filename "file.json")
  in

  let test_sanitize_korean () =
    (* Korean chars are allowed by the regex [가-힣] *)
    let result = Figma_crawl.sanitize_filename "메인화면" in
    check bool "korean preserved" true (String.length result > 0);
    check bool "no underscore only" true (result <> "____")
  in

  let test_sanitize_long_name () =
    let long = String.make 80 'a' in
    let result = Figma_crawl.sanitize_filename long in
    check bool "truncated" true (String.length result <= 50)
  in

  let test_sanitize_exactly_50 () =
    let s = String.make 50 'x' in
    check int "exact 50" 50 (String.length (Figma_crawl.sanitize_filename s))
  in

  let test_sanitize_empty () =
    check string "empty" "" (Figma_crawl.sanitize_filename "")
  in

  let test_sanitize_all_special () =
    let result = Figma_crawl.sanitize_filename "!@#$%^&*()" in
    (* All chars replaced with _ *)
    check bool "all underscores" true
      (String.for_all (fun c -> c = '_') result)
  in

  (* ============== render_node_tree ============== *)
  (* This function has 14 branches based on node_type *)
  let make_node ?(children=[]) id name typ =
    let base = [
      ("id", `String id);
      ("name", `String name);
      ("type", `String typ);
    ] in
    let fields = if children <> [] then
      base @ [("children", `List children)]
    else base in
    `Assoc fields
  in

  let render_one ?(max_depth=10) node =
    let buf = Buffer.create 256 in
    Figma_crawl.render_node_tree ~indent:0 ~max_depth ~depth:0 node buf;
    Buffer.contents buf
  in

  let test_render_document () =
    let node = make_node "0:0" "Doc" "DOCUMENT" in
    let result = render_one node in
    check bool "has doc icon" true
      (try let _ = Str.search_forward (Str.regexp_string "Doc") result 0 in true
       with Not_found -> false);
    check bool "has DOCUMENT" true
      (try let _ = Str.search_forward (Str.regexp_string "[DOCUMENT]") result 0 in true
       with Not_found -> false)
  in

  let test_render_canvas () =
    let node = make_node "0:1" "Page1" "CANVAS" in
    let result = render_one node in
    check bool "has CANVAS" true
      (try let _ = Str.search_forward (Str.regexp_string "[CANVAS]") result 0 in true
       with Not_found -> false)
  in

  let test_render_page () =
    let node = make_node "0:2" "Page2" "PAGE" in
    let result = render_one node in
    check bool "has PAGE" true
      (try let _ = Str.search_forward (Str.regexp_string "[PAGE]") result 0 in true
       with Not_found -> false)
  in

  let test_render_frame () =
    let node = make_node "1:1" "MainFrame" "FRAME" in
    let result = render_one node in
    check bool "has FRAME" true
      (try let _ = Str.search_forward (Str.regexp_string "[FRAME]") result 0 in true
       with Not_found -> false)
  in

  let test_render_group () =
    let node = make_node "1:2" "Group1" "GROUP" in
    let result = render_one node in
    check bool "has GROUP" true
      (try let _ = Str.search_forward (Str.regexp_string "[GROUP]") result 0 in true
       with Not_found -> false)
  in

  let test_render_component () =
    let node = make_node "1:3" "Btn" "COMPONENT" in
    let result = render_one node in
    check bool "has COMPONENT" true
      (try let _ = Str.search_forward (Str.regexp_string "[COMPONENT]") result 0 in true
       with Not_found -> false)
  in

  let test_render_component_set () =
    let node = make_node "1:4" "BtnSet" "COMPONENT_SET" in
    let result = render_one node in
    check bool "has COMPONENT_SET" true
      (try let _ = Str.search_forward (Str.regexp_string "[COMPONENT_SET]") result 0 in true
       with Not_found -> false)
  in

  let test_render_instance () =
    let node = make_node "1:5" "BtnInst" "INSTANCE" in
    let result = render_one node in
    check bool "has INSTANCE" true
      (try let _ = Str.search_forward (Str.regexp_string "[INSTANCE]") result 0 in true
       with Not_found -> false)
  in

  let test_render_text () =
    let node = make_node "1:6" "Label" "TEXT" in
    let result = render_one node in
    check bool "has TEXT" true
      (try let _ = Str.search_forward (Str.regexp_string "[TEXT]") result 0 in true
       with Not_found -> false)
  in

  let test_render_shapes () =
    (* RECTANGLE, ELLIPSE, POLYGON, STAR, LINE, VECTOR all map to the same icon *)
    let shapes = ["RECTANGLE"; "ELLIPSE"; "POLYGON"; "STAR"; "LINE"; "VECTOR"] in
    List.iter (fun typ ->
      let node = make_node "s:1" "Shape" typ in
      let result = render_one node in
      check bool (sprintf "has [%s]" typ) true
        (try let _ = Str.search_forward (Str.regexp_string (sprintf "[%s]" typ)) result 0 in true
         with Not_found -> false)
    ) shapes
  in

  let test_render_boolean_op () =
    let node = make_node "b:1" "Bool" "BOOLEAN_OPERATION" in
    let result = render_one node in
    check bool "has BOOLEAN_OPERATION" true
      (try let _ = Str.search_forward (Str.regexp_string "[BOOLEAN_OPERATION]") result 0 in true
       with Not_found -> false)
  in

  let test_render_section () =
    let node = make_node "sec:1" "Sec" "SECTION" in
    let result = render_one node in
    check bool "has SECTION" true
      (try let _ = Str.search_forward (Str.regexp_string "[SECTION]") result 0 in true
       with Not_found -> false)
  in

  let test_render_unknown_type () =
    let node = make_node "u:1" "Unknown" "SOMETHING_ELSE" in
    let result = render_one node in
    check bool "has bullet" true
      (try let _ = Str.search_forward (Str.regexp "•") result 0 in true
       with Not_found -> false)
  in

  let test_render_with_children () =
    let child = make_node "2:1" "Child" "TEXT" in
    let root = make_node ~children:[child] "1:1" "Root" "FRAME" in
    let result = render_one root in
    check bool "has Root" true
      (try let _ = Str.search_forward (Str.regexp_string "Root") result 0 in true
       with Not_found -> false);
    check bool "has Child" true
      (try let _ = Str.search_forward (Str.regexp_string "Child") result 0 in true
       with Not_found -> false)
  in

  let test_render_depth_limit () =
    let deep_child = make_node "3:1" "Deep" "TEXT" in
    let child = make_node ~children:[deep_child] "2:1" "Mid" "FRAME" in
    let root = make_node ~children:[child] "1:1" "Top" "DOCUMENT" in
    let result = render_one ~max_depth:1 root in
    check bool "has Top" true
      (try let _ = Str.search_forward (Str.regexp_string "Top") result 0 in true
       with Not_found -> false);
    check bool "has Mid" true
      (try let _ = Str.search_forward (Str.regexp_string "Mid") result 0 in true
       with Not_found -> false);
    (* Deep should be excluded because depth 2 > max_depth 1 *)
    check bool "no Deep" true
      (try let _ = Str.search_forward (Str.regexp_string "Deep") result 0 in false
       with Not_found -> true)
  in

  let test_render_indent () =
    let child = make_node "2:1" "Child" "TEXT" in
    let root = make_node ~children:[child] "1:1" "Root" "FRAME" in
    let buf = Buffer.create 256 in
    Figma_crawl.render_node_tree ~indent:2 ~max_depth:10 ~depth:0 root buf;
    let result = Buffer.contents buf in
    (* indent=2 means 4 spaces prefix for root *)
    let lines = String.split_on_char '\n' result in
    let first_line = List.hd lines in
    check bool "root indented" true (String.length first_line >= 4)
  in

  let test_render_empty_node () =
    let node = `Assoc [] in
    let result = render_one node in
    (* Should handle missing fields gracefully *)
    check bool "non-empty output" true (String.length result > 0)
  in

  let test_render_max_depth_zero () =
    let node = make_node "1:1" "Root" "FRAME" in
    let result = render_one ~max_depth:0 node in
    check bool "renders at depth 0" true (String.length result > 0)
  in

  let test_render_depth_exceeds_max () =
    let node = make_node "1:1" "Root" "FRAME" in
    let buf = Buffer.create 256 in
    (* depth > max_depth should produce nothing *)
    Figma_crawl.render_node_tree ~indent:0 ~max_depth:2 ~depth:5 node buf;
    check string "empty for deep" "" (Buffer.contents buf)
  in

  (* ============== stdout_progress ============== *)
  let test_stdout_progress () =
    (* Just ensure it doesn't crash — output goes to stdout *)
    Figma_crawl.stdout_progress "test message"
  in

  (* ============== mkdir_p ============== *)
  let test_mkdir_p_existing () =
    (* Current dir always exists *)
    Figma_crawl.mkdir_p (Sys.getcwd ())
  in

  let test_mkdir_p_nested () =
    let tmpdir = Filename.get_temp_dir_name () in
    let path = Filename.concat tmpdir
      (sprintf "figma_crawl_test_%d/sub/deep" (Unix.getpid ())) in
    Figma_crawl.mkdir_p path;
    check bool "created" true (Sys.file_exists path);
    (* cleanup *)
    ignore (Sys.command (sprintf "rm -rf %s/figma_crawl_test_%d"
      (Filename.quote tmpdir) (Unix.getpid ())))
  in

  let test_mkdir_p_already_exists () =
    let tmpdir = Filename.get_temp_dir_name () in
    let path = Filename.concat tmpdir
      (sprintf "figma_crawl_test2_%d" (Unix.getpid ())) in
    Figma_crawl.mkdir_p path;
    (* Call again — should not error on EEXIST *)
    Figma_crawl.mkdir_p path;
    check bool "still exists" true (Sys.file_exists path);
    ignore (Sys.command (sprintf "rm -rf %s" (Filename.quote path)))
  in

  (* ============== Run tests ============== *)
  run "figma_crawl extra (w9)" [
    ("progress_to_json", [
      test_case "empty progress" `Quick test_progress_to_json_empty;
      test_case "with data" `Quick test_progress_to_json_with_data;
    ]);
    ("buffer_progress", [
      test_case "empty" `Quick test_buffer_progress_empty;
      test_case "messages" `Quick test_buffer_progress_messages;
      test_case "newlines" `Quick test_buffer_progress_newlines;
    ]);
    ("sanitize_filename", [
      test_case "basic" `Quick test_sanitize_basic;
      test_case "spaces" `Quick test_sanitize_spaces;
      test_case "special chars" `Quick test_sanitize_special_chars;
      test_case "dots" `Quick test_sanitize_dots;
      test_case "korean" `Quick test_sanitize_korean;
      test_case "long name" `Quick test_sanitize_long_name;
      test_case "exactly 50" `Quick test_sanitize_exactly_50;
      test_case "empty" `Quick test_sanitize_empty;
      test_case "all special" `Quick test_sanitize_all_special;
    ]);
    ("render_node_tree", [
      test_case "DOCUMENT" `Quick test_render_document;
      test_case "CANVAS" `Quick test_render_canvas;
      test_case "PAGE" `Quick test_render_page;
      test_case "FRAME" `Quick test_render_frame;
      test_case "GROUP" `Quick test_render_group;
      test_case "COMPONENT" `Quick test_render_component;
      test_case "COMPONENT_SET" `Quick test_render_component_set;
      test_case "INSTANCE" `Quick test_render_instance;
      test_case "TEXT" `Quick test_render_text;
      test_case "shapes" `Quick test_render_shapes;
      test_case "BOOLEAN_OPERATION" `Quick test_render_boolean_op;
      test_case "SECTION" `Quick test_render_section;
      test_case "unknown type" `Quick test_render_unknown_type;
      test_case "with children" `Quick test_render_with_children;
      test_case "depth limit" `Quick test_render_depth_limit;
      test_case "indent" `Quick test_render_indent;
      test_case "empty node" `Quick test_render_empty_node;
      test_case "max depth 0" `Quick test_render_max_depth_zero;
      test_case "depth exceeds max" `Quick test_render_depth_exceeds_max;
    ]);
    ("stdout_progress", [
      test_case "basic" `Quick test_stdout_progress;
    ]);
    ("mkdir_p", [
      test_case "existing dir" `Quick test_mkdir_p_existing;
      test_case "nested creation" `Quick test_mkdir_p_nested;
      test_case "already exists" `Quick test_mkdir_p_already_exists;
    ]);
  ]
