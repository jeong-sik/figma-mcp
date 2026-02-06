open Alcotest

module CC = Figma_code_connect

let load_mapping path =
  let json = Yojson.Safe.from_file path in
  let mapping, parse_errors = CC.parse_json json in
  (* Fixtures should be schema-correct; validation should handle semantic errors. *)
  check (list string) "parse errors" []
    (List.map (fun d -> d.CC.message) parse_errors);
  mapping

let validate_messages mapping =
  CC.validate mapping |> List.map (fun d -> d.CC.message)

let test_valid_mapping () =
  let mapping = load_mapping "fixtures/code_connect/valid.json" in
  let errors = validate_messages mapping in
  check (list string) "no errors" [] errors

let test_duplicate_id () =
  let mapping = load_mapping "fixtures/code_connect/invalid_duplicate_id.json" in
  let errors = validate_messages mapping in
  check bool "has duplicate" true (List.exists (fun s -> String.equal s "duplicate id: dup") errors)

let test_missing_export () =
  let mapping = load_mapping "fixtures/code_connect/invalid_missing_export.json" in
  let errors = validate_messages mapping in
  check bool "has missing export" true
    (List.exists (fun s -> String.equal s "missing code.export: no.export") errors)

let test_exact_node_id_match () =
  let mapping = load_mapping "fixtures/code_connect/valid.json" in
  let score, _, comp =
    List.hd
      (CC.choose ~limit:1 ~query_name:"Button / Primary" ~query_variant:[ ("size", "md") ]
         ~query_node_id:(Some "123:456") ~query_component_key:None mapping.components)
  in
  check (float 1e-6) "score" 1.0 score;
  check string "id" "button.primary" comp.CC.id

let test_component_key_match () =
  let mapping = load_mapping "fixtures/code_connect/valid.json" in
  let score, _, comp =
    List.hd
      (CC.choose ~limit:1 ~query_name:"Button / Secondary" ~query_variant:[] ~query_node_id:None
         ~query_component_key:(Some "ck_456") mapping.components)
  in
  check (float 1e-6) "score" 0.95 score;
  check string "id" "button.secondary" comp.CC.id

let test_name_normalization () =
  let mapping = load_mapping "fixtures/code_connect/valid.json" in
  let score, _, comp =
    List.hd
      (CC.choose ~limit:1 ~query_name:"button-primary" ~query_variant:[] ~query_node_id:None
         ~query_component_key:None mapping.components)
  in
  check (float 1e-6) "score" 0.85 score;
  check string "id" "button.primary" comp.CC.id

let test_variant_bonus_cap () =
  let mapping = load_mapping "fixtures/code_connect/valid.json" in
  let score, _, _ =
    List.hd
      (CC.choose ~limit:1 ~query_name:"Button / Secondary"
         ~query_variant:
           [ ("size", "md"); ("state", "default"); ("extra", "x"); ("x", "y"); ("y", "z"); ("z", "q") ]
         ~query_node_id:None ~query_component_key:None mapping.components)
  in
  check bool "score cap" true (score <= 0.90)

let test_tie_breaks () =
  let comp_a =
    {
      CC.id = "a";
      figma = { CC.node_id = None; component_key = None; name = "Card"; variant = [] };
      code = { CC.package = None; file = None; export = "Card"; props = [] };
      aliases = [];
      tags = [];
    }
  in
  let comp_b =
    {
      CC.id = "b";
      figma = { CC.node_id = None; component_key = None; name = "Card"; variant = [] };
      code = { CC.package = None; file = Some "src/card.tsx"; export = "Card"; props = [] };
      aliases = [];
      tags = [];
    }
  in
  let scored =
    CC.choose ~limit:2 ~query_name:"Card" ~query_variant:[] ~query_node_id:None ~query_component_key:None
      [ comp_a; comp_b ]
  in
  let _, _, first = List.hd scored in
  check string "prefer file" "b" first.CC.id

let test_limit_behavior () =
  let mapping = load_mapping "fixtures/code_connect/valid.json" in
  let scored =
    CC.choose ~limit:2 ~query_name:"Button" ~query_variant:[] ~query_node_id:None ~query_component_key:None
      mapping.components
  in
  check int "limit" 2 (List.length scored)

let () =
  run "code_connect"
    [
      ( "mapping",
        [
          test_case "valid mapping" `Quick test_valid_mapping;
          test_case "duplicate id" `Quick test_duplicate_id;
          test_case "missing export" `Quick test_missing_export;
        ] );
      ( "matching",
        [
          test_case "exact node_id" `Quick test_exact_node_id_match;
          test_case "component key" `Quick test_component_key_match;
          test_case "name normalization" `Quick test_name_normalization;
          test_case "variant bonus cap" `Quick test_variant_bonus_cap;
          test_case "tie breaks" `Quick test_tie_breaks;
          test_case "limit behavior" `Quick test_limit_behavior;
        ] );
    ]

