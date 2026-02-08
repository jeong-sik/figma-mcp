(** Tests for figma_search handler (name/text search).

    Goals:
    - Multi-word queries should work (tokenized matching), unlike naive substring.
    - Handler should return JSON array payload (in text content) for robust clients.
    - Token resolution should prefer server-side FIGMA_TOKEN (no request override).
*)

open Alcotest

let read_fixture filename =
  let path = "fixtures/" ^ filename in
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let text_of_tool_result (json : Yojson.Safe.t) : string =
  match Yojson.Safe.Util.member "content" json with
  | `List (first :: _) -> (
      match Yojson.Safe.Util.member "text" first with
      | `String s -> s
      | _ -> "")
  | _ -> ""

let json_list_of_text s : Yojson.Safe.t list =
  match Yojson.Safe.from_string s with
  | `List items -> items
  | _ -> []

let with_env name value f =
  let old = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match old with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f

let test_resolve_token_env_wins () =
  with_env "FIGMA_TOKEN" "envtok" (fun () ->
    let args = `Assoc [("token", `String "reqtok")] in
    match Mcp_tools.resolve_token args with
    | Some t -> check string "env token wins" "envtok" t
    | None -> fail "expected Some token")

let test_handle_search_multiword_name_query () =
  (* Ensure request token is used even if FIGMA_TOKEN is unset by the runner. *)
  with_env "FIGMA_TOKEN" "" (fun () ->
    let store = Figma_effects.create_mock_store () in
    let doc_json = Yojson.Safe.from_string (read_fixture "simple_frame.json") in
    Hashtbl.add store.files "TESTFILEKEY" (`Assoc [("document", doc_json)]);

    let args =
      `Assoc [
        ("file_key", `String "TESTFILEKEY");
        ("token", `String "tok");
        ("query", `String "card button");
        ("search_in", `String "name");
      ]
    in
    let result =
      Figma_effects.run_with_mock store (fun () -> Mcp_tools.handle_search args)
    in
    match result with
    | Error msg -> fail msg
    | Ok json ->
        let text = text_of_tool_result json in
        let items = json_list_of_text text in
        check int "has 2 results" 2 (List.length items);
        let names =
          List.map (fun it -> Yojson.Safe.Util.(it |> member "name" |> to_string)) items
        in
        (* Deterministic tie-break: score same → name asc *)
        check (list string) "names" ["Button"; "Card"] names)

let test_handle_search_text_only () =
  with_env "FIGMA_TOKEN" "" (fun () ->
    let store = Figma_effects.create_mock_store () in
    let doc_json = Yojson.Safe.from_string (read_fixture "simple_frame.json") in
    Hashtbl.add store.files "TESTFILEKEY2" (`Assoc [("document", doc_json)]);

    let args =
      `Assoc [
        ("file_key", `String "TESTFILEKEY2");
        ("token", `String "tok");
        ("query", `String "hello world");
        ("search_in", `String "text");
      ]
    in
    let result =
      Figma_effects.run_with_mock store (fun () -> Mcp_tools.handle_search args)
    in
    match result with
    | Error msg -> fail msg
    | Ok json ->
        let items = json_list_of_text (text_of_tool_result json) in
        check bool "non-empty" true (List.length items >= 1);
        let first = List.hd items in
        let name = Yojson.Safe.Util.(first |> member "name" |> to_string) in
        let matched_in = Yojson.Safe.Util.(first |> member "matched_in" |> to_string) in
        let chars = Yojson.Safe.Util.(first |> member "characters" |> to_string) in
        check string "title node" "Title" name;
        check string "matched_in" "text" matched_in;
        check bool "chars contains Hello" true (String.length chars > 0 && String.contains chars 'H'))

let test_handle_search_empty_query_returns_empty_array () =
  with_env "FIGMA_TOKEN" "" (fun () ->
    let store = Figma_effects.create_mock_store () in
    let doc_json = Yojson.Safe.from_string (read_fixture "simple_frame.json") in
    Hashtbl.add store.files "TESTFILEKEY3" (`Assoc [("document", doc_json)]);

    let args =
      `Assoc [
        ("file_key", `String "TESTFILEKEY3");
        ("token", `String "tok");
        ("query", `String "   ");
        ("search_in", `String "name");
      ]
    in
    let result =
      Figma_effects.run_with_mock store (fun () -> Mcp_tools.handle_search args)
    in
    match result with
    | Error msg -> fail msg
    | Ok json ->
        let text = text_of_tool_result json |> String.trim in
        check string "empty json array" "[]" text)

let () =
  run "figma_search handler"
    [
      ( "token",
        [
          "resolve_token_env_wins", `Quick, test_resolve_token_env_wins;
        ] );
      ( "search",
        [
          "multiword_name_query", `Quick, test_handle_search_multiword_name_query;
          "text_only", `Quick, test_handle_search_text_only;
          "empty_query", `Quick, test_handle_search_empty_query_returns_empty_array;
        ] );
    ]

