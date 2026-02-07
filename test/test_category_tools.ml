(** Category tool behavior tests.
    Goal: keep tool surface small while preserving discoverability (schemas). *)

open Alcotest

let contains_substring ~needle haystack =
  let n = String.length needle in
  let h = String.length haystack in
  if n = 0 then true
  else
    let rec loop i =
      if i + n > h then false
      else if String.sub haystack i n = needle then true
      else loop (i + 1)
    in
    loop 0

let extract_text_content (j : Yojson.Safe.t) : string =
  let open Yojson.Safe.Util in
  j |> member "content" |> index 0 |> member "text" |> to_string

let parse_text_json (j : Yojson.Safe.t) : Yojson.Safe.t =
  Yojson.Safe.from_string (extract_text_content j)

let test_visual_list_includes_semantic () =
  match Mcp_tools.handle_category "visual" (`Assoc []) with
  | Error msg -> fail msg
  | Ok resp ->
      let open Yojson.Safe.Util in
      let j = parse_text_json resp in
      let tools = j |> member "tools" |> to_list in
      let names = List.map (fun t -> t |> member "name" |> to_string) tools in
      check bool "has verify_semantic" true (List.mem "verify_semantic" names);
      check bool "has verify_visual" true (List.mem "verify_visual" names)

let test_visual_describe_returns_schema () =
  match Mcp_tools.handle_category "visual" (`Assoc [("tool", `String "verify_semantic")]) with
  | Error msg -> fail msg
  | Ok resp ->
      let open Yojson.Safe.Util in
      let j = parse_text_json resp in
      let schema = j |> member "input_schema" in
      match schema with
      | `Assoc _ -> ()
      | _ -> fail "Expected input_schema to be an object"

let test_wrong_category_call_fails_fast () =
  (* Previously this would route to the handler (and fail without Eio ctx).
     We now fail fast with a category error, which is more actionable. *)
  match Mcp_tools.handle_category "core" (`Assoc [
    ("tool", `String "verify_visual");
    ("args", `Assoc []);
  ]) with
  | Ok _ -> fail "Expected error"
  | Error msg ->
      check bool "category error" true (contains_substring ~needle:"not found in category" msg)

let () =
  run "category_tools" [
    ("list", [ test_case "visual includes verify_semantic" `Quick test_visual_list_includes_semantic ]);
    ("describe", [ test_case "describe returns schema" `Quick test_visual_describe_returns_schema ]);
    ("call", [ test_case "wrong category fails fast" `Quick test_wrong_category_call_fails_fast ]);
  ]
