open Alcotest

let contains ~needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

let sample_node =
  `Assoc
    [
      ("id", `String "1:1");
      ("name", `String "Root");
      ("type", `String "FRAME");
      ( "absoluteBoundingBox",
        `Assoc
          [
            ("x", `Int 0);
            ("y", `Int 0);
            ("width", `Int 320);
            ("height", `Int 200);
          ] );
      ( "children",
        `List
          [
            `Assoc
              [
                ("id", `String "1:2");
                ("name", `String "Title");
                ("type", `String "TEXT");
                ("x", `Int 24);
                ("y", `Int 32);
                ("width", `Int 120);
                ("height", `Int 24);
              ];
            `Assoc
              [
                ("id", `String "1:3");
                ("name", `String "Body");
                ("type", `String "FRAME");
                ("x", `Int 24);
                ("y", `Int 72);
                ("width", `Int 272);
                ("height", `Int 80);
              ];
          ] );
    ]

let test_metadata_xml_shape () =
  let xml = Mcp_v2_handlers.metadata_xml_of_node ~depth:2 sample_node in
  check bool "root tag" true (contains ~needle:"<node id=\"1:1\"" xml);
  check bool "child tag" true (contains ~needle:"name=\"Title\"" xml);
  check bool "bbox field" true (contains ~needle:"width=\"320\"" xml)

let test_metadata_xml_truncation () =
  let xml = Mcp_v2_handlers.metadata_xml_of_node ~depth:2 ~max_children:1 sample_node in
  check bool "truncation marker" true (contains ~needle:"truncated:" xml)

let () =
  run "v2-metadata"
    [
      ("metadata", [
         test_case "xml shape" `Quick test_metadata_xml_shape;
         test_case "xml truncation" `Quick test_metadata_xml_truncation;
       ]);
    ]
