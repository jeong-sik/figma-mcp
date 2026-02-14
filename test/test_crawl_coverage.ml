(** Coverage tests for figma_crawl.ml — pure functions.
    Targets 0/371 coverage points: create_progress, default_options,
    create_neo4j_config, get_string, get_string_or, get_list, flatten_nodes *)

let () =
  let open Alcotest in

  (* ============== create_progress ============== *)
  let test_progress_initial () =
    let p = Figma_crawl.create_progress () in
    check int "teams" 0 p.teams;
    check int "projects" 0 p.projects;
    check int "files" 0 p.files;
    check int "nodes" 0 p.nodes;
    check (list string) "errors" [] p.errors
  in
  let test_progress_mutable () =
    let p = Figma_crawl.create_progress () in
    p.teams <- 3;
    p.files <- 10;
    p.errors <- ["err1"];
    check int "teams set" 3 p.teams;
    check int "files set" 10 p.files;
    check (list string) "errors set" ["err1"] p.errors
  in

  (* ============== default_options ============== *)
  let test_default_options () =
    let o = Figma_crawl.default_options in
    check int "max_depth" 10 o.max_depth;
    check bool "include_hidden" false o.include_hidden;
    check int "batch_size" 100 o.batch_size;
    check int "rate_limit_ms" 100 o.rate_limit_ms;
    check (list string) "skip_files" [] o.skip_files
  in

  (* ============== create_neo4j_config ============== *)
  let test_neo4j_config_basic () =
    let cfg = Figma_crawl.create_neo4j_config
      ~uri:"http://localhost:7474" ~user:"neo4j" ~password:"pass" () in
    check string "uri" "http://localhost:7474" cfg.uri;
    check string "database" "neo4j" cfg.database;
    check bool "has Basic" true
      (String.length cfg.auth_header > 6 &&
       String.sub cfg.auth_header 0 6 = "Basic ")
  in
  let test_neo4j_config_custom_db () =
    let cfg = Figma_crawl.create_neo4j_config
      ~uri:"bolt://db:7687" ~database:"mydb" ~user:"u" ~password:"p" () in
    check string "custom db" "mydb" cfg.database
  in
  let test_neo4j_config_auth_encoding () =
    let cfg = Figma_crawl.create_neo4j_config
      ~uri:"x" ~user:"neo4j" ~password:"test" () in
    (* Base64("neo4j:test") = "bmVvNGo6dGVzdA==" *)
    check string "auth" "Basic bmVvNGo6dGVzdA==" cfg.auth_header
  in

  (* ============== create_neo4j_config_from_env ============== *)
  let test_neo4j_config_from_env () =
    (* Uses defaults when env vars are not set *)
    let cfg = Figma_crawl.create_neo4j_config_from_env () in
    check bool "has uri" true (String.length cfg.uri > 0);
    check bool "has database" true (String.length cfg.database > 0);
    check bool "has auth" true (String.length cfg.auth_header > 0)
  in

  (* ============== get_string ============== *)
  let test_get_string_found () =
    let json = `Assoc [("name", `String "hello")] in
    check (option string) "found" (Some "hello") (Figma_crawl.get_string "name" json)
  in
  let test_get_string_missing () =
    let json = `Assoc [("other", `String "x")] in
    check (option string) "missing" None (Figma_crawl.get_string "name" json)
  in
  let test_get_string_not_string () =
    let json = `Assoc [("name", `Int 42)] in
    check (option string) "not string" None (Figma_crawl.get_string "name" json)
  in
  let test_get_string_null () =
    let json = `Assoc [("name", `Null)] in
    check (option string) "null" None (Figma_crawl.get_string "name" json)
  in

  (* ============== get_string_or ============== *)
  let test_get_string_or_found () =
    let json = `Assoc [("k", `String "v")] in
    check string "found" "v" (Figma_crawl.get_string_or "k" "default" json)
  in
  let test_get_string_or_default () =
    let json = `Assoc [] in
    check string "default" "fallback" (Figma_crawl.get_string_or "k" "fallback" json)
  in

  (* ============== get_list ============== *)
  let test_get_list_found () =
    let json = `Assoc [("items", `List [`Int 1; `Int 2])] in
    let result = Figma_crawl.get_list "items" json in
    check int "length" 2 (List.length result)
  in
  let test_get_list_missing () =
    let json = `Assoc [] in
    check (list string) "empty" [] (List.map Yojson.Safe.to_string (Figma_crawl.get_list "items" json))
  in
  let test_get_list_not_list () =
    let json = `Assoc [("items", `String "not a list")] in
    check int "empty" 0 (List.length (Figma_crawl.get_list "items" json))
  in

  (* ============== flatten_nodes ============== *)
  let test_flatten_single () =
    let node = `Assoc [
      ("id", `String "1:1");
      ("name", `String "Root");
      ("type", `String "FRAME");
    ] in
    let result = Figma_crawl.flatten_nodes
      ~file_key:"f1" ~parent_id:None ~depth:0 ~max_depth:10 node [] in
    check int "one node" 1 (List.length result);
    let (id, name, typ, fk, pid) = List.hd result in
    check string "id" "1:1" id;
    check string "name" "Root" name;
    check string "type" "FRAME" typ;
    check string "file_key" "f1" fk;
    check (option string) "parent" None pid
  in
  let test_flatten_with_children () =
    let child1 = `Assoc [
      ("id", `String "2:1"); ("name", `String "Child1"); ("type", `String "TEXT");
    ] in
    let child2 = `Assoc [
      ("id", `String "2:2"); ("name", `String "Child2"); ("type", `String "RECT");
    ] in
    let root = `Assoc [
      ("id", `String "1:1"); ("name", `String "Root"); ("type", `String "FRAME");
      ("children", `List [child1; child2]);
    ] in
    let result = Figma_crawl.flatten_nodes
      ~file_key:"f1" ~parent_id:None ~depth:0 ~max_depth:10 root [] in
    check int "three nodes" 3 (List.length result);
    (* Children have parent_id set *)
    let child_entries = List.filter (fun (_, _, _, _, pid) -> pid <> None) result in
    check int "two children" 2 (List.length child_entries);
    let (_, _, _, _, pid) = List.hd child_entries in
    check (option string) "parent is root" (Some "1:1") pid
  in
  let test_flatten_depth_limit () =
    let deep_child = `Assoc [
      ("id", `String "3:1"); ("name", `String "Deep"); ("type", `String "TEXT");
    ] in
    let mid = `Assoc [
      ("id", `String "2:1"); ("name", `String "Mid"); ("type", `String "FRAME");
      ("children", `List [deep_child]);
    ] in
    let root = `Assoc [
      ("id", `String "1:1"); ("name", `String "Root"); ("type", `String "FRAME");
      ("children", `List [mid]);
    ] in
    let result = Figma_crawl.flatten_nodes
      ~file_key:"f1" ~parent_id:None ~depth:0 ~max_depth:1 root [] in
    (* depth 0: root, depth 1: mid, depth 2: exceeds max_depth=1 *)
    check int "only root + mid" 2 (List.length result)
  in
  let test_flatten_no_children () =
    let node = `Assoc [
      ("id", `String "1:1"); ("name", `String "Leaf"); ("type", `String "TEXT");
    ] in
    let result = Figma_crawl.flatten_nodes
      ~file_key:"f2" ~parent_id:(Some "0:0") ~depth:0 ~max_depth:5 node [] in
    check int "one node" 1 (List.length result);
    let (_, _, _, _, pid) = List.hd result in
    check (option string) "has parent" (Some "0:0") pid
  in
  let test_flatten_missing_fields () =
    (* Node with no id/name/type fields → uses empty defaults *)
    let node = `Assoc [] in
    let result = Figma_crawl.flatten_nodes
      ~file_key:"f1" ~parent_id:None ~depth:0 ~max_depth:10 node [] in
    check int "one node" 1 (List.length result);
    let (id, name, typ, _, _) = List.hd result in
    check string "empty id" "" id;
    check string "empty name" "" name;
    check string "empty type" "" typ
  in

  run "Figma_crawl Coverage" [
    ("create_progress", [
      test_case "initial" `Quick test_progress_initial;
      test_case "mutable" `Quick test_progress_mutable;
    ]);
    ("default_options", [
      test_case "values" `Quick test_default_options;
    ]);
    ("create_neo4j_config", [
      test_case "basic" `Quick test_neo4j_config_basic;
      test_case "custom db" `Quick test_neo4j_config_custom_db;
      test_case "auth encoding" `Quick test_neo4j_config_auth_encoding;
    ]);
    ("create_neo4j_config_from_env", [
      test_case "defaults" `Quick test_neo4j_config_from_env;
    ]);
    ("get_string", [
      test_case "found" `Quick test_get_string_found;
      test_case "missing" `Quick test_get_string_missing;
      test_case "not string" `Quick test_get_string_not_string;
      test_case "null" `Quick test_get_string_null;
    ]);
    ("get_string_or", [
      test_case "found" `Quick test_get_string_or_found;
      test_case "default" `Quick test_get_string_or_default;
    ]);
    ("get_list", [
      test_case "found" `Quick test_get_list_found;
      test_case "missing" `Quick test_get_list_missing;
      test_case "not list" `Quick test_get_list_not_list;
    ]);
    ("flatten_nodes", [
      test_case "single" `Quick test_flatten_single;
      test_case "with children" `Quick test_flatten_with_children;
      test_case "depth limit" `Quick test_flatten_depth_limit;
      test_case "no children" `Quick test_flatten_no_children;
      test_case "missing fields" `Quick test_flatten_missing_fields;
    ]);
  ]
