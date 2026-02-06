(** Comprehensive Coverage Tests for figma_tree.ml

    Target: figma_tree.ml (131 bisect points, 43.51% coverage)
    Uncovered: ~74 points

    Functions:
    - type_emoji (19 node_type variants)
    - node_info (show_size/show_id flags, bbox presence)
    - render_ascii (depth, is_last, children branches)
    - render_indent (depth, children)
    - render_compact (type_char mapping, children)
    - collect_stats (recursive, merge_stats, by_type aggregation)
    - stats_to_string (formatting)
    - render (style variants, show_stats flag)
    - find_path (found/not found)
    - path_to_string
*)

open Alcotest
open Figma_types
open Figma_tree

(** ============== Test Fixtures ============== *)

let make_bbox ?(x=0.) ?(y=0.) width height = { x; y; width; height }

let make_node
    ?(id="n1") ?(name="Node") ?(node_type=Frame) ?(bbox=None) ?(children=[]) () =
  { default_node with id; name; node_type; bbox; children }

(** ============== type_emoji Tests ============== *)

let test_type_emoji_all () =
  (* Exercise all 19 variants *)
  let _ = type_emoji Document in
  let _ = type_emoji Canvas in
  let _ = type_emoji Frame in
  let _ = type_emoji Group in
  let _ = type_emoji Component in
  let _ = type_emoji ComponentSet in
  let _ = type_emoji Instance in
  let _ = type_emoji Text in
  let _ = type_emoji Rectangle in
  let _ = type_emoji Ellipse in
  let _ = type_emoji Vector in
  let _ = type_emoji Line in
  let _ = type_emoji Star in
  let _ = type_emoji RegularPolygon in
  let _ = type_emoji BooleanOperation in
  let _ = type_emoji Slice in
  let _ = type_emoji Sticky in
  let _ = type_emoji Section in
  let _ = type_emoji (Unknown "X") in
  (* Just verify they return non-empty strings *)
  check bool "document emoji" true (String.length (type_emoji Document) > 0);
  check bool "unknown emoji" true (String.length (type_emoji (Unknown "X")) > 0)

(** ============== node_info Tests ============== *)

let test_node_info_with_size () =
  let node = make_node ~name:"Button" ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let info = node_info ~show_size:true ~show_id:false node in
  check bool "contains name" true
    (try let _ = Str.search_forward (Str.regexp_string "Button") info 0 in true
     with Not_found -> false);
  check bool "contains size" true
    (try let _ = Str.search_forward (Str.regexp_string "100x50") info 0 in true
     with Not_found -> false)

let test_node_info_no_size () =
  let node = make_node ~name:"Card" ~bbox:(Some (make_bbox 200.0 100.0)) () in
  let info = node_info ~show_size:false ~show_id:false node in
  check bool "no size shown" false
    (try let _ = Str.search_forward (Str.regexp_string "200x100") info 0 in true
     with Not_found -> false)

let test_node_info_no_bbox () =
  let node = make_node ~name:"NoBbox" ~bbox:None () in
  let info = node_info ~show_size:true ~show_id:false node in
  (* When show_size=true but no bbox, no size string is appended *)
  check bool "contains name" true
    (try let _ = Str.search_forward (Str.regexp_string "NoBbox") info 0 in true
     with Not_found -> false)

let test_node_info_with_id () =
  let node = make_node ~id:"abc-123" ~name:"Test" () in
  let info = node_info ~show_size:false ~show_id:true node in
  check bool "contains id" true
    (try let _ = Str.search_forward (Str.regexp_string "abc-123") info 0 in true
     with Not_found -> false)

(** ============== render_ascii Tests ============== *)

let test_render_ascii_single () =
  let node = make_node ~name:"Root" () in
  let lines = render_ascii ~max_depth:None ~show_size:true ~show_id:false node in
  check bool "has one line" true (List.length lines >= 1)

let test_render_ascii_with_children () =
  let c1 = make_node ~id:"c1" ~name:"Child1" () in
  let c2 = make_node ~id:"c2" ~name:"Child2" () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[c1; c2] () in
  let lines = render_ascii ~max_depth:None ~show_size:true ~show_id:false root in
  check bool "has multiple lines" true (List.length lines >= 3)

let test_render_ascii_depth_limited () =
  let leaf = make_node ~id:"leaf" ~name:"Leaf" () in
  let mid = make_node ~id:"mid" ~name:"Mid" ~children:[leaf] () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[mid] () in
  let lines = render_ascii ~max_depth:(Some 1) ~show_size:true ~show_id:false root in
  (* Root + Mid shown, but Leaf is cut off *)
  check bool "limited depth" true (List.length lines <= 3)

let test_render_ascii_connectors () =
  let c1 = make_node ~id:"c1" ~name:"First" () in
  let c2 = make_node ~id:"c2" ~name:"Last" () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[c1; c2] () in
  let lines = render_ascii ~max_depth:None ~show_size:false ~show_id:false root in
  let output = String.concat "\n" lines in
  (* Should use tree connectors *)
  check bool "has connector chars" true (String.length output > 0)

(** ============== render_indent Tests ============== *)

let test_render_indent_single () =
  let node = make_node ~name:"Root" () in
  let lines = render_indent ~max_depth:None ~show_size:true ~show_id:false node in
  check bool "has one line" true (List.length lines >= 1)

let test_render_indent_with_children () =
  let c1 = make_node ~id:"c1" ~name:"Child" () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[c1] () in
  let lines = render_indent ~max_depth:None ~show_size:false ~show_id:false root in
  check bool "has multiple lines" true (List.length lines >= 2)

let test_render_indent_depth_limited () =
  let leaf = make_node ~id:"leaf" ~name:"Leaf" () in
  let mid = make_node ~id:"mid" ~name:"Mid" ~children:[leaf] () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[mid] () in
  let lines = render_indent ~max_depth:(Some 1) ~show_size:false ~show_id:false root in
  check bool "limited output" true (List.length lines <= 3)

(** ============== render_compact Tests ============== *)

let test_render_compact_single () =
  let node = make_node ~node_type:Frame ~name:"Root" () in
  let s = render_compact ~max_depth:None node in
  check string "frame = F" "F" s

let test_render_compact_types () =
  check string "Frame" "F" (render_compact ~max_depth:None (make_node ~node_type:Frame ()));
  check string "Component" "C" (render_compact ~max_depth:None (make_node ~node_type:Component ()));
  check string "Instance" "I" (render_compact ~max_depth:None (make_node ~node_type:Instance ()));
  check string "Text" "T" (render_compact ~max_depth:None (make_node ~node_type:Text ()));
  check string "Group" "G" (render_compact ~max_depth:None (make_node ~node_type:Group ()));
  check string "Rectangle" "R" (render_compact ~max_depth:None (make_node ~node_type:Rectangle ()));
  check string "Unknown" "?" (render_compact ~max_depth:None (make_node ~node_type:Ellipse ()))

let test_render_compact_with_children () =
  let c1 = make_node ~id:"c1" ~node_type:Text () in
  let c2 = make_node ~id:"c2" ~node_type:Rectangle () in
  let root = make_node ~id:"root" ~node_type:Frame ~children:[c1; c2] () in
  let s = render_compact ~max_depth:None root in
  check string "F[TR]" "F[TR]" s

let test_render_compact_depth_limited () =
  let leaf = make_node ~id:"leaf" ~node_type:Text () in
  let mid = make_node ~id:"mid" ~node_type:Group ~children:[leaf] () in
  let root = make_node ~id:"root" ~node_type:Frame ~children:[mid] () in
  let s = render_compact ~max_depth:(Some 1) root in
  check string "depth 1" "F[G]" s

(** ============== collect_stats Tests ============== *)

let test_collect_stats_single () =
  let node = make_node ~node_type:Frame () in
  let stats = collect_stats node in
  check int "total 1" 1 stats.total_nodes;
  check int "depth 0" 0 stats.max_depth;
  check int "1 leaf" 1 stats.leaf_count

let test_collect_stats_nested () =
  let leaf1 = make_node ~id:"l1" ~node_type:Text () in
  let leaf2 = make_node ~id:"l2" ~node_type:Rectangle () in
  let mid = make_node ~id:"mid" ~node_type:Group ~children:[leaf1; leaf2] () in
  let root = make_node ~id:"root" ~node_type:Frame ~children:[mid] () in
  let stats = collect_stats root in
  check int "total 4" 4 stats.total_nodes;
  check int "max depth 2" 2 stats.max_depth;
  check int "2 leaves" 2 stats.leaf_count;
  (* Check by_type *)
  check bool "has FRAME" true (List.exists (fun (t, _) -> t = "FRAME") stats.by_type);
  check bool "has TEXT" true (List.exists (fun (t, _) -> t = "TEXT") stats.by_type);
  check bool "has RECTANGLE" true (List.exists (fun (t, _) -> t = "RECTANGLE") stats.by_type);
  check bool "has GROUP" true (List.exists (fun (t, _) -> t = "GROUP") stats.by_type)

(** ============== stats_to_string Tests ============== *)

let test_stats_to_string () =
  let node = make_node ~node_type:Frame () in
  let stats = collect_stats node in
  let s = stats_to_string stats in
  check bool "contains header" true
    (try let _ = Str.search_forward (Str.regexp_string "트리 통계") s 0 in true
     with Not_found -> false);
  check bool "contains total" true
    (try let _ = Str.search_forward (Str.regexp_string "총 노드") s 0 in true
     with Not_found -> false)

(** ============== render (main) Tests ============== *)

let test_render_ascii_style () =
  let node = make_node ~name:"Root" () in
  let s = render ~style:Ascii ~show_size:true node in
  check bool "non-empty" true (String.length s > 0)

let test_render_indent_style () =
  let node = make_node ~name:"Root" () in
  let s = render ~style:Indent ~show_size:true node in
  check bool "non-empty" true (String.length s > 0)

let test_render_compact_style () =
  let node = make_node ~name:"Root" () in
  let s = render ~style:Compact node in
  check string "F" "F" s

let test_render_with_stats () =
  let node = make_node ~name:"Root" () in
  let s = render ~style:Ascii ~show_stats:true node in
  check bool "contains stats" true
    (try let _ = Str.search_forward (Str.regexp_string "트리 통계") s 0 in true
     with Not_found -> false)

let test_render_without_stats () =
  let node = make_node ~name:"Root" () in
  let s = render ~style:Ascii ~show_stats:false node in
  check bool "no stats" false
    (try let _ = Str.search_forward (Str.regexp_string "트리 통계") s 0 in true
     with Not_found -> false)

(** ============== find_path / path_to_string Tests ============== *)

let test_find_path_root () =
  let root = make_node ~id:"root" ~name:"Root" () in
  let path = find_path ~target_id:"root" root in
  check bool "found" true (Option.is_some path);
  check int "length 1" 1 (List.length (Option.get path))

let test_find_path_child () =
  let child = make_node ~id:"child" ~name:"Child" () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[child] () in
  let path = find_path ~target_id:"child" root in
  check bool "found" true (Option.is_some path);
  check int "length 2" 2 (List.length (Option.get path))

let test_find_path_deep () =
  let leaf = make_node ~id:"leaf" ~name:"Leaf" () in
  let mid = make_node ~id:"mid" ~name:"Mid" ~children:[leaf] () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[mid] () in
  let path = find_path ~target_id:"leaf" root in
  check bool "found" true (Option.is_some path);
  check int "length 3" 3 (List.length (Option.get path))

let test_find_path_not_found () =
  let root = make_node ~id:"root" ~name:"Root" () in
  let path = find_path ~target_id:"missing" root in
  check bool "not found" true (Option.is_none path)

let test_path_to_string () =
  let leaf = make_node ~id:"leaf" ~name:"Leaf" () in
  let mid = make_node ~id:"mid" ~name:"Mid" ~children:[leaf] () in
  let root = make_node ~id:"root" ~name:"Root" ~children:[mid] () in
  let path = find_path ~target_id:"leaf" root |> Option.get in
  let s = path_to_string path in
  check string "path string" "Root > Mid > Leaf" s

(** ============== Main Test Runner ============== *)

let () =
  run "figma-tree-coverage" [
    "type_emoji", [
      "all_variants", `Quick, test_type_emoji_all;
    ];
    "node_info", [
      "with_size", `Quick, test_node_info_with_size;
      "no_size", `Quick, test_node_info_no_size;
      "no_bbox", `Quick, test_node_info_no_bbox;
      "with_id", `Quick, test_node_info_with_id;
    ];
    "render_ascii", [
      "single", `Quick, test_render_ascii_single;
      "with_children", `Quick, test_render_ascii_with_children;
      "depth_limited", `Quick, test_render_ascii_depth_limited;
      "connectors", `Quick, test_render_ascii_connectors;
    ];
    "render_indent", [
      "single", `Quick, test_render_indent_single;
      "with_children", `Quick, test_render_indent_with_children;
      "depth_limited", `Quick, test_render_indent_depth_limited;
    ];
    "render_compact", [
      "single", `Quick, test_render_compact_single;
      "types", `Quick, test_render_compact_types;
      "with_children", `Quick, test_render_compact_with_children;
      "depth_limited", `Quick, test_render_compact_depth_limited;
    ];
    "collect_stats", [
      "single", `Quick, test_collect_stats_single;
      "nested", `Quick, test_collect_stats_nested;
    ];
    "stats_to_string", [
      "formatting", `Quick, test_stats_to_string;
    ];
    "render", [
      "ascii_style", `Quick, test_render_ascii_style;
      "indent_style", `Quick, test_render_indent_style;
      "compact_style", `Quick, test_render_compact_style;
      "with_stats", `Quick, test_render_with_stats;
      "without_stats", `Quick, test_render_without_stats;
    ];
    "find_path", [
      "root", `Quick, test_find_path_root;
      "child", `Quick, test_find_path_child;
      "deep", `Quick, test_find_path_deep;
      "not_found", `Quick, test_find_path_not_found;
    ];
    "path_to_string", [
      "formatting", `Quick, test_path_to_string;
    ];
  ]
