(** Coverage Wave 5: analyze_node_semantic + generate_code_template
    Pure JSON→string transformations in mcp_protocol_eio.ml (lines 1282-1559).
    No I/O, no Eio — all pure functions. *)

open Figma_protocol_eio

(** Helper: check if [haystack] contains [needle] *)
let str_contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found && String.sub haystack i nlen = needle then found := true
    done;
    !found

let check_contains ~msg haystack needle =
  if not (str_contains haystack needle) then
    Alcotest.fail
      (Printf.sprintf "%s: expected substring %S not found in:\n%s" msg needle haystack)

let check_not_contains ~msg haystack needle =
  if str_contains haystack needle then
    Alcotest.fail
      (Printf.sprintf "%s: unexpected substring %S found in:\n%s" msg needle haystack)

(* ---------- JSON node builders ---------- *)

let empty_node =
  Yojson.Safe.from_string {|{"name":"Empty","type":"FRAME"}|}

let simple_frame =
  Yojson.Safe.from_string {|{
    "name":"Header","type":"FRAME",
    "width":1200,"height":60,"x":10,"y":20,
    "fills":[{"color":"#FFFFFF"}],
    "children":[]
  }|}

(* Frame with diverse children *)
let frame_with_children =
  Yojson.Safe.from_string {|{
    "name":"Dashboard","type":"FRAME",
    "width":1440,"height":900,"x":0,"y":0,
    "fills":[{"color":"#F5F5F5"}],
    "children":[
      {"name":"Title","type":"TEXT","text":{"characters":"Dashboard","fontSize":24},"fills":[{"color":"#111111"}],"width":300,"height":32},
      {"name":"Logo","type":"ELLIPSE","width":48,"height":48,"fills":[{"color":"#0066FF"}]},
      {"name":"CardContainer","type":"FRAME","width":1200,"height":700,"children":[
        {"name":"StatItem","type":"FRAME","width":280,"height":120,"fills":[{"color":"#FFFFFF"}],"children":[]}
      ],"fills":[{"color":"#FAFAFA"}]},
      {"name":"SidePanel","type":"RECTANGLE","width":240,"height":900,"cornerRadius":0,"fills":[{"color":"#222222"}]}
    ]
  }|}

(* Deep nesting: 5 levels *)
let deep_node =
  Yojson.Safe.from_string {|{
    "name":"Root","type":"FRAME","width":1000,"height":800,"x":0,"y":0,
    "children":[
      {"name":"L1","type":"FRAME","width":900,"height":700,"children":[
        {"name":"L2","type":"FRAME","width":800,"height":600,"children":[
          {"name":"L3","type":"FRAME","width":700,"height":500,"children":[
            {"name":"L4deep","type":"FRAME","width":600,"height":400,"children":[
              {"name":"L5leaf","type":"TEXT","text":{"characters":"deep"},"width":100,"height":20}
            ]}
          ]}
        ]}
      ]}
    ]
  }|}

(* Node with >10 children *)
let many_children_node =
  let mk i = Printf.sprintf {|{"name":"Child%d","type":"FRAME","width":50,"height":50,"children":[]}|} i in
  let kids = List.init 15 mk |> String.concat "," in
  Yojson.Safe.from_string (Printf.sprintf {|{
    "name":"Grid","type":"FRAME","width":1000,"height":800,"x":0,"y":0,
    "fills":[{"color":"#AAAAAA"}],
    "children":[%s]
  }|} kids)

(* Node with special characters in name *)
let special_name_node =
  Yojson.Safe.from_string {|{
    "name":"My Frame - v2.0 (copy)","type":"FRAME",
    "width":500,"height":300,
    "children":[]
  }|}

(* Node name starting with digit *)
let digit_name_node =
  Yojson.Safe.from_string {|{
    "name":"3DObject","type":"FRAME",
    "width":200,"height":200,
    "children":[]
  }|}

(* Node with no fills *)
let no_fills_node =
  Yojson.Safe.from_string {|{
    "name":"Plain","type":"FRAME",
    "width":200,"height":100,"x":0,"y":0,
    "children":[
      {"name":"Inner","type":"FRAME","width":100,"height":50,"children":[]}
    ]
  }|}

(* Node with fills but no color key *)
let fills_no_color_node =
  Yojson.Safe.from_string {|{
    "name":"Gradient","type":"FRAME",
    "width":300,"height":200,"x":0,"y":0,
    "fills":[{"type":"GRADIENT_LINEAR"}],
    "children":[]
  }|}

(* For generate_code_template: TEXT child with characters at top level *)
let codegen_text_child =
  Yojson.Safe.from_string {|{
    "name":"Wrapper","type":"FRAME",
    "width":400,"height":200,
    "children":[
      {"name":"Label","type":"TEXT","characters":"Click me","width":80,"height":20}
    ]
  }|}

(* Node with autoLayout *)
let autolayout_h_node =
  Yojson.Safe.from_string {|{
    "name":"Row","type":"FRAME",
    "width":600,"height":40,
    "autoLayout":{"mode":"HORIZONTAL","spacing":12},
    "children":[
      {"name":"Item1","type":"FRAME","width":100,"height":40,"children":[]},
      {"name":"Item2","type":"FRAME","width":100,"height":40,"children":[]}
    ]
  }|}

let autolayout_v_node =
  Yojson.Safe.from_string {|{
    "name":"Column","type":"FRAME",
    "width":200,"height":500,
    "autoLayout":{"mode":"VERTICAL","spacing":8},
    "fills":[{"color":"#FFFFFF"}],
    "children":[
      {"name":"Top","type":"TEXT","characters":"Top","width":200,"height":20},
      {"name":"Bottom","type":"TEXT","characters":"Bottom","width":200,"height":20}
    ]
  }|}

(* autoLayout with spacing as Float *)
let autolayout_float_spacing =
  Yojson.Safe.from_string {|{
    "name":"FlexRow","type":"FRAME",
    "width":400,"height":60,
    "autoLayout":{"mode":"HORIZONTAL","spacing":16.5},
    "children":[
      {"name":"A","type":"FRAME","width":100,"height":60,"children":[]}
    ]
  }|}

(* Node for bg color extraction in generate_code_template *)
let bg_color_node =
  Yojson.Safe.from_string {|{
    "name":"Colored","type":"FRAME",
    "width":300,"height":200,
    "fills":[{"color":"#3A7BFF"}],
    "children":[]
  }|}

(* ===== analyze_node_semantic tests ===== *)

let test_semantic_root_info () =
  let result = analyze_node_semantic simple_frame in
  check_contains ~msg:"name" result "Header";
  check_contains ~msg:"dimensions" result "1200x60";
  check_contains ~msg:"background" result "#FFFFFF"

let test_semantic_root_transparent_bg () =
  let result = analyze_node_semantic no_fills_node in
  check_contains ~msg:"transparent bg" result "transparent"

let test_semantic_root_fills_no_color () =
  let result = analyze_node_semantic fills_no_color_node in
  check_contains ~msg:"transparent when no color" result "transparent"

let test_semantic_empty_node () =
  let result = analyze_node_semantic empty_node in
  check_contains ~msg:"name" result "Empty";
  check_contains ~msg:"has tokens section" result "Design Tokens"

let test_semantic_text_child () =
  let node = Yojson.Safe.from_string {|{
    "name":"Panel","type":"FRAME","width":400,"height":200,"x":0,"y":0,
    "children":[
      {"name":"Title","type":"TEXT","text":{"characters":"Hello","fontSize":16},"fills":[{"color":"#000000"}],"width":200,"height":24}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"TEXT keyword" result "TEXT";
  check_contains ~msg:"characters" result "Hello";
  check_contains ~msg:"font size" result "16px";
  check_contains ~msg:"color" result "#000000"

let test_semantic_text_no_chars () =
  let node = Yojson.Safe.from_string {|{
    "name":"Panel","type":"FRAME","width":400,"height":200,"x":0,"y":0,
    "children":[
      {"name":"EmptyText","type":"TEXT","text":{},"width":100,"height":20}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"TEXT keyword" result "TEXT";
  check_contains ~msg:"empty quotes for missing chars" result {|""|}

let test_semantic_frame_child () =
  let node = Yojson.Safe.from_string {|{
    "name":"Container","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[
      {"name":"Inner","type":"FRAME","width":400,"height":300,"fills":[{"color":"#AABBCC"}],"children":[]}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"FRAME" result "FRAME";
  check_contains ~msg:"child name" result "Inner";
  check_contains ~msg:"child dimensions" result "400x300";
  check_contains ~msg:"bg color" result "#AABBCC"

let test_semantic_rectangle_with_radius () =
  let node = Yojson.Safe.from_string {|{
    "name":"Page","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[
      {"name":"Card","type":"RECTANGLE","width":320,"height":200,"cornerRadius":12.5,"fills":[{"color":"#FFFFFF"}]}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"RECTANGLE" result "RECTANGLE";
  check_contains ~msg:"radius" result "radius";
  check_contains ~msg:"12" result "12"

let test_semantic_rectangle_no_radius () =
  let node = Yojson.Safe.from_string {|{
    "name":"Page","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[
      {"name":"Flat","type":"RECTANGLE","width":100,"height":50,"cornerRadius":0,"fills":[{"color":"#CCCCCC"}]}
    ]
  }|} in
  let result = analyze_node_semantic node in
  (* cornerRadius=0 should NOT add ", radius ..." *)
  check_not_contains ~msg:"no radius when 0" result "radius"

let test_semantic_corner_radius_int () =
  let node = Yojson.Safe.from_string {|{
    "name":"Page","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[
      {"name":"RoundedBox","type":"RECTANGLE","width":100,"height":100,"cornerRadius":8,"fills":[{"color":"#DDDDDD"}]}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"radius int" result "radius 8px"

let test_semantic_ellipse_child () =
  let node = Yojson.Safe.from_string {|{
    "name":"Container","type":"FRAME","width":200,"height":200,"x":0,"y":0,
    "children":[
      {"name":"Circle","type":"ELLIPSE","width":48,"height":48,"fills":[{"color":"#0066FF"}]}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"ELLIPSE" result "ELLIPSE";
  check_contains ~msg:"name" result "Circle";
  check_contains ~msg:"fill color" result "#0066FF"

let test_semantic_ellipse_no_fill () =
  let node = Yojson.Safe.from_string {|{
    "name":"Container","type":"FRAME","width":200,"height":200,"x":0,"y":0,
    "children":[
      {"name":"Ring","type":"ELLIPSE","width":48,"height":48}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"ELLIPSE without fill" result "ELLIPSE";
  check_not_contains ~msg:"no fill keyword" result "fill #"

let test_semantic_unknown_type () =
  let node = Yojson.Safe.from_string {|{
    "name":"Canvas","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[
      {"name":"Shape","type":"VECTOR","width":30,"height":30}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"VECTOR type" result "VECTOR";
  check_contains ~msg:"name" result "Shape"

let test_semantic_group_child () =
  let node = Yojson.Safe.from_string {|{
    "name":"Canvas","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[
      {"name":"NavGroup","type":"GROUP","width":400,"height":50,"children":[]}
    ]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"GROUP" result "GROUP";
  check_contains ~msg:"name" result "NavGroup"

(* Pattern detection tests *)
let test_semantic_pattern_sidebar () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"Sidebar","type":"FRAME","width":240,"height":600,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"Sidebar pattern" result "[Sidebar]"

let test_semantic_pattern_header () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"HeaderBar","type":"FRAME","width":800,"height":60,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"Header pattern" result "[Header]"

let test_semantic_pattern_card () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"CardItem","type":"FRAME","width":300,"height":200,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"Card pattern" result "[Card]"

let test_semantic_pattern_stat () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"StatWidget","type":"FRAME","width":200,"height":100,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"StatCard pattern" result "[StatCard]"

let test_semantic_pattern_nav () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"NavLink","type":"FRAME","width":120,"height":36,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"NavItem pattern" result "[NavItem]"

let test_semantic_pattern_button () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"ButtonPrimary","type":"FRAME","width":120,"height":40,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"Button pattern" result "[Button]"

let test_semantic_pattern_search () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"SearchBar","type":"FRAME","width":300,"height":36,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"SearchInput pattern" result "[SearchInput]"

let test_semantic_pattern_logo () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"LogoMark","type":"ELLIPSE","width":64,"height":64,"fills":[{"color":"#0000FF"}]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"Logo pattern" result "[Logo]"

let test_semantic_pattern_avatar () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"AvatarIcon","type":"ELLIPSE","width":32,"height":32,"fills":[{"color":"#CCCCCC"}]}]
  }|} in
  let result = analyze_node_semantic node in
  check_contains ~msg:"Avatar pattern" result "[Avatar]"

let test_semantic_pattern_no_match () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":800,"height":600,"x":0,"y":0,
    "children":[{"name":"RandomThing","type":"FRAME","width":100,"height":100,"children":[]}]
  }|} in
  let result = analyze_node_semantic node in
  (* No pattern bracket should appear *)
  check_not_contains ~msg:"no pattern for random name" result "[Sidebar]";
  check_not_contains ~msg:"no pattern" result "[Header]";
  check_not_contains ~msg:"no pattern" result "[Card]"

(* Depth limiting test *)
let test_semantic_depth_limit () =
  let result = analyze_node_semantic deep_node in
  check_contains ~msg:"Structure section" result "Structure";
  check_contains ~msg:"L1 present" result "L1";
  check_contains ~msg:"L2 present" result "L2";
  check_contains ~msg:"L3 present" result "L3";
  (* depth=3 child L3 has children → should show "... N more children" *)
  check_contains ~msg:"more children hint" result "more children"

(* Color collection *)
let test_semantic_colors_collected () =
  let result = analyze_node_semantic frame_with_children in
  check_contains ~msg:"Design Tokens" result "Design Tokens";
  check_contains ~msg:"root bg color" result "#F5F5F5";
  check_contains ~msg:"text color" result "#111111"

let test_semantic_no_children () =
  let result = analyze_node_semantic simple_frame in
  (* Empty children list → Structure section with no items *)
  check_not_contains ~msg:"no structure items" result "- FRAME";
  check_contains ~msg:"has tokens" result "Design Tokens"

let test_semantic_full_dashboard () =
  let result = analyze_node_semantic frame_with_children in
  check_contains ~msg:"title" result "Dashboard";
  check_contains ~msg:"1440x900" result "1440x900";
  check_contains ~msg:"Structure" result "Structure";
  check_contains ~msg:"SidePanel" result "SidePanel"

(* ===== generate_code_template tests ===== *)

let test_gen_react_empty () =
  let result = generate_code_template simple_frame "react" in
  check_contains ~msg:"import React" result "import React";
  check_contains ~msg:"export const" result "export const";
  check_contains ~msg:"component name" result "Header";
  check_contains ~msg:"width" result "1200";
  check_contains ~msg:"height" result "60";
  check_contains ~msg:"empty children" result "{/* Empty */}"

let test_gen_react_bg_color () =
  let result = generate_code_template bg_color_node "react" in
  check_contains ~msg:"backgroundColor" result "backgroundColor";
  check_contains ~msg:"color value" result "#3A7BFF"

let test_gen_react_with_children () =
  let result = generate_code_template codegen_text_child "react" in
  check_contains ~msg:"span" result "<span>";
  check_contains ~msg:"text content" result "Click me"

let test_gen_swiftui_empty () =
  let result = generate_code_template simple_frame "swiftui" in
  check_contains ~msg:"import SwiftUI" result "import SwiftUI";
  check_contains ~msg:"struct" result "struct Header";
  check_contains ~msg:"View" result "View";
  check_contains ~msg:"frame" result ".frame(width: 1200, height: 60)";
  check_contains ~msg:"EmptyView" result "EmptyView()"

let test_gen_swiftui_bg_color () =
  let result = generate_code_template bg_color_node "swiftui" in
  check_contains ~msg:".background" result ".background(Color(hex:";
  check_contains ~msg:"color value" result "#3A7BFF"

let test_gen_swiftui_text () =
  let result = generate_code_template codegen_text_child "swiftui" in
  check_contains ~msg:"Text" result {|Text("Click me")|}

let test_gen_compose_empty () =
  let result = generate_code_template simple_frame "compose" in
  check_contains ~msg:"@Composable" result "@Composable";
  check_contains ~msg:"fun" result "fun Header";
  check_contains ~msg:"Modifier" result "Modifier";
  check_contains ~msg:"empty" result "// Empty"

let test_gen_compose_bg_color () =
  let result = generate_code_template bg_color_node "compose" in
  check_contains ~msg:".background" result ".background(Color(0xFF";
  (* #3A7BFF → strip # → 3A7BFF *)
  check_contains ~msg:"color hex" result "3A7BFF"

let test_gen_compose_text () =
  let result = generate_code_template codegen_text_child "compose" in
  check_contains ~msg:"Text" result {|Text("Click me")|}

let test_gen_flutter_empty () =
  let result = generate_code_template simple_frame "flutter" in
  check_contains ~msg:"import flutter" result "import 'package:flutter/material.dart'";
  check_contains ~msg:"class" result "class Header extends StatelessWidget";
  check_contains ~msg:"Container" result "Container";
  check_contains ~msg:"width" result "width: 1200";
  check_contains ~msg:"empty" result "// Empty"

let test_gen_flutter_bg_color () =
  let result = generate_code_template bg_color_node "flutter" in
  check_contains ~msg:"Color" result "Color(0xFF";
  check_contains ~msg:"color hex" result "3A7BFF"

let test_gen_flutter_text () =
  let result = generate_code_template codegen_text_child "flutter" in
  check_contains ~msg:"Text" result "Text('Click me')"

let test_gen_html_default () =
  let result = generate_code_template simple_frame "html" in
  check_contains ~msg:"div class" result {|<div class="header"|};
  check_contains ~msg:"style block" result "<style>";
  check_contains ~msg:"width" result "1200px";
  check_contains ~msg:"height" result "60px";
  check_contains ~msg:"empty" result "<!-- Empty -->"

let test_gen_html_bg_color () =
  let result = generate_code_template bg_color_node "html" in
  check_contains ~msg:"background-color" result "background-color: #3A7BFF"

let test_gen_html_text () =
  let result = generate_code_template codegen_text_child "html" in
  check_contains ~msg:"span" result "<span>Click me</span>"

let test_gen_unknown_platform () =
  (* Non-matching platform falls through to default (HTML) *)
  let result = generate_code_template simple_frame "angular" in
  check_contains ~msg:"html output" result "<div class=";
  check_contains ~msg:"style" result "<style>"

(* Safe name tests *)
let test_gen_safe_name_special_chars () =
  let result = generate_code_template special_name_node "react" in
  (* "My Frame - v2.0 (copy)" → "My_Frame___v2_0__copy_" *)
  check_contains ~msg:"sanitized name" result "My_Frame___v2_0__copy_";
  check_not_contains ~msg:"no spaces" result "My Frame"

let test_gen_safe_name_digit_prefix () =
  let result = generate_code_template digit_name_node "react" in
  (* "3DObject" → "_3DObject" *)
  check_contains ~msg:"underscore prefix" result "_3DObject"

(* Depth limiting in generate_code_template *)
let test_gen_depth_limit_react () =
  (* Build a 4-level deep tree. At depth=3, children should be placeholders *)
  let deep = Yojson.Safe.from_string {|{
    "name":"Root","type":"FRAME","width":1000,"height":800,
    "children":[
      {"name":"L1","type":"FRAME","width":900,"height":700,"children":[
        {"name":"L2","type":"FRAME","width":800,"height":600,"children":[
          {"name":"L3","type":"FRAME","width":700,"height":500,"children":[
            {"name":"L4","type":"FRAME","width":600,"height":400,"children":[]}
          ]}
        ]}
      ]}
    ]
  }|} in
  let result = generate_code_template deep "react" in
  check_contains ~msg:"React output" result "import React";
  (* Sub-components should be generated for L1, L2, L3 *)
  check_contains ~msg:"L1" result "L1"

let test_gen_depth_limit_placeholder () =
  (* At max depth (3), a child node should render as placeholder *)
  let deep = Yojson.Safe.from_string {|{
    "name":"DeepChild","type":"FRAME","width":100,"height":50,
    "children":[{"name":"Leaf","type":"FRAME","width":50,"height":25,"children":[]}]
  }|} in
  let result = generate_code_template ~depth:3 deep "react" in
  (* At depth=3, children become placeholders: <div style={{width: ..., height: ...}}> *)
  check_contains ~msg:"placeholder div" result "{/*"

let test_gen_depth_limit_swiftui_placeholder () =
  let deep = Yojson.Safe.from_string {|{
    "name":"DeepChild","type":"FRAME","width":100,"height":50,
    "children":[{"name":"Leaf","type":"FRAME","width":50,"height":25,"children":[]}]
  }|} in
  let result = generate_code_template ~depth:3 deep "swiftui" in
  check_contains ~msg:"Rectangle placeholder" result "Rectangle()"

let test_gen_depth_limit_compose_placeholder () =
  let deep = Yojson.Safe.from_string {|{
    "name":"DeepChild","type":"FRAME","width":100,"height":50,
    "children":[{"name":"Leaf","type":"FRAME","width":50,"height":25,"children":[]}]
  }|} in
  let result = generate_code_template ~depth:3 deep "compose" in
  check_contains ~msg:"Box placeholder" result "Box(Modifier.size("

let test_gen_depth_limit_flutter_placeholder () =
  let deep = Yojson.Safe.from_string {|{
    "name":"DeepChild","type":"FRAME","width":100,"height":50,
    "children":[{"name":"Leaf","type":"FRAME","width":50,"height":25,"children":[]}]
  }|} in
  let result = generate_code_template ~depth:3 deep "flutter" in
  check_contains ~msg:"SizedBox placeholder" result "SizedBox(width:"

let test_gen_depth_limit_html_placeholder () =
  let deep = Yojson.Safe.from_string {|{
    "name":"DeepChild","type":"FRAME","width":100,"height":50,
    "children":[{"name":"Leaf","type":"FRAME","width":50,"height":25,"children":[]}]
  }|} in
  let result = generate_code_template ~depth:3 deep "html" in
  check_contains ~msg:"html placeholder" result "<!-- Leaf -->"

(* Many children: >10 should be limited *)
let test_gen_many_children_limited () =
  let result = generate_code_template many_children_node "react" in
  (* Should contain first 10 children and a "more children" comment *)
  check_contains ~msg:"Child0" result "Child0";
  check_contains ~msg:"Child9" result "Child9";
  check_contains ~msg:"and more" result "more children"

let test_gen_many_children_comment () =
  let result = generate_code_template many_children_node "react" in
  (* children_comment shows up to 8 children in comments *)
  check_contains ~msg:"Children count" result "Children (15)"

(* TEXT node with characters directly on child (generate_code_template style) *)
let test_gen_text_with_characters () =
  let node = Yojson.Safe.from_string {|{
    "name":"Card","type":"FRAME","width":300,"height":200,
    "children":[
      {"name":"Price","type":"TEXT","characters":"$99.99","width":80,"height":20}
    ]
  }|} in
  let result = generate_code_template node "react" in
  check_contains ~msg:"text content" result "$99.99"

let test_gen_text_no_characters () =
  (* TEXT node without characters → falls back to name *)
  let node = Yojson.Safe.from_string {|{
    "name":"Card","type":"FRAME","width":300,"height":200,
    "children":[
      {"name":"Placeholder","type":"TEXT","width":80,"height":20}
    ]
  }|} in
  let result = generate_code_template node "react" in
  check_contains ~msg:"fallback to name" result "Placeholder"

(* autoLayout tests for generate_code_template *)
let test_gen_react_autolayout_horizontal () =
  let result = generate_code_template autolayout_h_node "react" in
  check_contains ~msg:"flexDirection row" result "flexDirection: 'row'";
  check_contains ~msg:"gap" result "gap: 12"

let test_gen_react_autolayout_vertical () =
  let result = generate_code_template autolayout_v_node "react" in
  check_contains ~msg:"flexDirection column" result "flexDirection: 'column'";
  check_contains ~msg:"gap" result "gap: 8"

let test_gen_swiftui_autolayout_horizontal () =
  let result = generate_code_template autolayout_h_node "swiftui" in
  check_contains ~msg:"HStack" result "HStack(spacing: 12)"

let test_gen_swiftui_autolayout_vertical () =
  let result = generate_code_template autolayout_v_node "swiftui" in
  check_contains ~msg:"VStack" result "VStack(spacing: 8)"

let test_gen_compose_autolayout_horizontal () =
  let result = generate_code_template autolayout_h_node "compose" in
  check_contains ~msg:"Row" result "Row(horizontalArrangement"

let test_gen_compose_autolayout_vertical () =
  let result = generate_code_template autolayout_v_node "compose" in
  check_contains ~msg:"Column" result "Column(verticalArrangement"

let test_gen_flutter_autolayout_horizontal () =
  let result = generate_code_template autolayout_h_node "flutter" in
  check_contains ~msg:"Row" result "child: Row("

let test_gen_flutter_autolayout_vertical () =
  let result = generate_code_template autolayout_v_node "flutter" in
  check_contains ~msg:"Column" result "child: Column("

let test_gen_html_autolayout_horizontal () =
  let result = generate_code_template autolayout_h_node "html" in
  check_contains ~msg:"flex row" result "flex-direction: row";
  check_contains ~msg:"gap" result "gap: 12px"

let test_gen_html_autolayout_vertical () =
  let result = generate_code_template autolayout_v_node "html" in
  check_contains ~msg:"flex column" result "flex-direction: column";
  check_contains ~msg:"gap" result "gap: 8px"

let test_gen_autolayout_float_spacing () =
  let result = generate_code_template autolayout_float_spacing "react" in
  check_contains ~msg:"gap value (truncated)" result "gap: 16"

(* Default width/height when missing → 100 *)
let test_gen_default_dimensions () =
  let node = Yojson.Safe.from_string {|{"name":"NoSize","type":"FRAME","children":[]}|} in
  let result = generate_code_template node "react" in
  check_contains ~msg:"default width 100" result "width: 100";
  check_contains ~msg:"default height 100" result "height: 100"

(* Non-TEXT, non-FRAME child → component reference *)
let test_gen_component_ref_react () =
  let node = Yojson.Safe.from_string {|{
    "name":"Parent","type":"FRAME","width":400,"height":300,
    "children":[
      {"name":"Icon","type":"COMPONENT","width":24,"height":24,"children":[]}
    ]
  }|} in
  let result = generate_code_template node "react" in
  check_contains ~msg:"component ref" result "<Icon />"

let test_gen_component_ref_swiftui () =
  let node = Yojson.Safe.from_string {|{
    "name":"Parent","type":"FRAME","width":400,"height":300,
    "children":[
      {"name":"Icon","type":"COMPONENT","width":24,"height":24,"children":[]}
    ]
  }|} in
  let result = generate_code_template node "swiftui" in
  check_contains ~msg:"component call" result "Icon()"

let test_gen_component_ref_html () =
  let node = Yojson.Safe.from_string {|{
    "name":"Parent","type":"FRAME","width":400,"height":300,
    "children":[
      {"name":"Icon","type":"COMPONENT","width":24,"height":24,"children":[]}
    ]
  }|} in
  let result = generate_code_template node "html" in
  check_contains ~msg:"lowercase tags" result "<icon></icon>"

(* Sub-component generation: non-TEXT children get their own component definition *)
let test_gen_sub_components () =
  let node = Yojson.Safe.from_string {|{
    "name":"App","type":"FRAME","width":1200,"height":800,
    "children":[
      {"name":"Sidebar","type":"FRAME","width":240,"height":800,"children":[]},
      {"name":"Content","type":"FRAME","width":960,"height":800,"children":[]}
    ]
  }|} in
  let result = generate_code_template node "react" in
  (* Sub-components are generated for Sidebar and Content *)
  check_contains ~msg:"Sidebar sub-component" result "export const Sidebar";
  check_contains ~msg:"Content sub-component" result "export const Content"

(* TEXT children should NOT generate sub-components *)
let test_gen_text_no_sub_component () =
  let node = Yojson.Safe.from_string {|{
    "name":"Card","type":"FRAME","width":300,"height":200,
    "children":[
      {"name":"Title","type":"TEXT","characters":"Hello","width":200,"height":24}
    ]
  }|} in
  let result = generate_code_template node "react" in
  (* TEXT children are filtered out of sub-component generation *)
  check_not_contains ~msg:"no sub-component for TEXT" result "export const Title"

(* Children comment (depth=0 only, lists up to 8) *)
let test_gen_children_comment_limit () =
  let mk i = Printf.sprintf {|{"name":"Item%d","type":"FRAME","width":50,"height":50,"children":[]}|} i in
  let kids = List.init 12 mk |> String.concat "," in
  let node = Yojson.Safe.from_string (Printf.sprintf {|{
    "name":"Grid","type":"FRAME","width":600,"height":400,
    "children":[%s]
  }|} kids) in
  let result = generate_code_template node "react" in
  check_contains ~msg:"children count" result "Children (12)";
  check_contains ~msg:"Item0 in comment" result "Item0";
  check_contains ~msg:"Item7 in comment" result "Item7";
  check_contains ~msg:"and more" result "and 4 more"

(* No children comment when depth > 0 *)
let test_gen_no_children_comment_at_depth () =
  let node = Yojson.Safe.from_string {|{
    "name":"Inner","type":"FRAME","width":200,"height":100,
    "children":[
      {"name":"A","type":"FRAME","width":50,"height":50,"children":[]}
    ]
  }|} in
  let result = generate_code_template ~depth:1 node "react" in
  check_not_contains ~msg:"no Children comment at depth>0" result "Children ("

(* ===== Test runner ===== *)
let () =
  Alcotest.run "Protocol Codegen W5" [
    ("analyze_semantic_root", [
      Alcotest.test_case "root info (name, dims, bg)" `Quick test_semantic_root_info;
      Alcotest.test_case "transparent bg when no fills" `Quick test_semantic_root_transparent_bg;
      Alcotest.test_case "transparent bg when fills have no color" `Quick test_semantic_root_fills_no_color;
      Alcotest.test_case "empty node" `Quick test_semantic_empty_node;
      Alcotest.test_case "full dashboard" `Quick test_semantic_full_dashboard;
      Alcotest.test_case "no children → no structure items" `Quick test_semantic_no_children;
    ]);
    ("analyze_semantic_text", [
      Alcotest.test_case "TEXT child with characters" `Quick test_semantic_text_child;
      Alcotest.test_case "TEXT child without characters" `Quick test_semantic_text_no_chars;
    ]);
    ("analyze_semantic_frame", [
      Alcotest.test_case "FRAME child" `Quick test_semantic_frame_child;
      Alcotest.test_case "RECTANGLE with cornerRadius" `Quick test_semantic_rectangle_with_radius;
      Alcotest.test_case "RECTANGLE no radius" `Quick test_semantic_rectangle_no_radius;
      Alcotest.test_case "RECTANGLE cornerRadius int" `Quick test_semantic_corner_radius_int;
      Alcotest.test_case "GROUP child" `Quick test_semantic_group_child;
    ]);
    ("analyze_semantic_ellipse", [
      Alcotest.test_case "ELLIPSE with fill" `Quick test_semantic_ellipse_child;
      Alcotest.test_case "ELLIPSE without fill" `Quick test_semantic_ellipse_no_fill;
    ]);
    ("analyze_semantic_other", [
      Alcotest.test_case "unknown type (VECTOR)" `Quick test_semantic_unknown_type;
    ]);
    ("analyze_semantic_pattern", [
      Alcotest.test_case "Sidebar pattern" `Quick test_semantic_pattern_sidebar;
      Alcotest.test_case "Header pattern" `Quick test_semantic_pattern_header;
      Alcotest.test_case "Card pattern" `Quick test_semantic_pattern_card;
      Alcotest.test_case "StatCard pattern" `Quick test_semantic_pattern_stat;
      Alcotest.test_case "NavItem pattern" `Quick test_semantic_pattern_nav;
      Alcotest.test_case "Button pattern" `Quick test_semantic_pattern_button;
      Alcotest.test_case "SearchInput pattern" `Quick test_semantic_pattern_search;
      Alcotest.test_case "Logo pattern" `Quick test_semantic_pattern_logo;
      Alcotest.test_case "Avatar pattern" `Quick test_semantic_pattern_avatar;
      Alcotest.test_case "no match" `Quick test_semantic_pattern_no_match;
    ]);
    ("analyze_semantic_depth", [
      Alcotest.test_case "depth limit at 3" `Quick test_semantic_depth_limit;
    ]);
    ("analyze_semantic_colors", [
      Alcotest.test_case "colors collected recursively" `Quick test_semantic_colors_collected;
    ]);
    ("generate_react", [
      Alcotest.test_case "empty frame" `Quick test_gen_react_empty;
      Alcotest.test_case "background color" `Quick test_gen_react_bg_color;
      Alcotest.test_case "with text child" `Quick test_gen_react_with_children;
      Alcotest.test_case "autoLayout horizontal" `Quick test_gen_react_autolayout_horizontal;
      Alcotest.test_case "autoLayout vertical" `Quick test_gen_react_autolayout_vertical;
    ]);
    ("generate_swiftui", [
      Alcotest.test_case "empty frame" `Quick test_gen_swiftui_empty;
      Alcotest.test_case "background color" `Quick test_gen_swiftui_bg_color;
      Alcotest.test_case "text child" `Quick test_gen_swiftui_text;
      Alcotest.test_case "autoLayout horizontal" `Quick test_gen_swiftui_autolayout_horizontal;
      Alcotest.test_case "autoLayout vertical" `Quick test_gen_swiftui_autolayout_vertical;
    ]);
    ("generate_compose", [
      Alcotest.test_case "empty frame" `Quick test_gen_compose_empty;
      Alcotest.test_case "background color" `Quick test_gen_compose_bg_color;
      Alcotest.test_case "text child" `Quick test_gen_compose_text;
      Alcotest.test_case "autoLayout horizontal" `Quick test_gen_compose_autolayout_horizontal;
      Alcotest.test_case "autoLayout vertical" `Quick test_gen_compose_autolayout_vertical;
    ]);
    ("generate_flutter", [
      Alcotest.test_case "empty frame" `Quick test_gen_flutter_empty;
      Alcotest.test_case "background color" `Quick test_gen_flutter_bg_color;
      Alcotest.test_case "text child" `Quick test_gen_flutter_text;
      Alcotest.test_case "autoLayout horizontal" `Quick test_gen_flutter_autolayout_horizontal;
      Alcotest.test_case "autoLayout vertical" `Quick test_gen_flutter_autolayout_vertical;
    ]);
    ("generate_html_default", [
      Alcotest.test_case "empty frame" `Quick test_gen_html_default;
      Alcotest.test_case "background color" `Quick test_gen_html_bg_color;
      Alcotest.test_case "text child" `Quick test_gen_html_text;
      Alcotest.test_case "unknown platform → HTML" `Quick test_gen_unknown_platform;
      Alcotest.test_case "autoLayout horizontal" `Quick test_gen_html_autolayout_horizontal;
      Alcotest.test_case "autoLayout vertical" `Quick test_gen_html_autolayout_vertical;
    ]);
    ("generate_depth_limit", [
      Alcotest.test_case "deep tree react" `Quick test_gen_depth_limit_react;
      Alcotest.test_case "placeholder at depth=3 react" `Quick test_gen_depth_limit_placeholder;
      Alcotest.test_case "placeholder at depth=3 swiftui" `Quick test_gen_depth_limit_swiftui_placeholder;
      Alcotest.test_case "placeholder at depth=3 compose" `Quick test_gen_depth_limit_compose_placeholder;
      Alcotest.test_case "placeholder at depth=3 flutter" `Quick test_gen_depth_limit_flutter_placeholder;
      Alcotest.test_case "placeholder at depth=3 html" `Quick test_gen_depth_limit_html_placeholder;
    ]);
    ("generate_safe_name", [
      Alcotest.test_case "special chars replaced" `Quick test_gen_safe_name_special_chars;
      Alcotest.test_case "digit prefix" `Quick test_gen_safe_name_digit_prefix;
    ]);
    ("generate_empty_children", [
      Alcotest.test_case "default dimensions (100x100)" `Quick test_gen_default_dimensions;
    ]);
    ("generate_text_node", [
      Alcotest.test_case "TEXT with characters" `Quick test_gen_text_with_characters;
      Alcotest.test_case "TEXT without characters → name fallback" `Quick test_gen_text_no_characters;
      Alcotest.test_case "TEXT no sub-component" `Quick test_gen_text_no_sub_component;
    ]);
    ("generate_many_children", [
      Alcotest.test_case ">10 children limited" `Quick test_gen_many_children_limited;
      Alcotest.test_case "children comment count" `Quick test_gen_many_children_comment;
      Alcotest.test_case "children comment 8 limit" `Quick test_gen_children_comment_limit;
      Alcotest.test_case "no children comment at depth>0" `Quick test_gen_no_children_comment_at_depth;
    ]);
    ("generate_component_ref", [
      Alcotest.test_case "component ref react" `Quick test_gen_component_ref_react;
      Alcotest.test_case "component ref swiftui" `Quick test_gen_component_ref_swiftui;
      Alcotest.test_case "component ref html" `Quick test_gen_component_ref_html;
    ]);
    ("generate_sub_components", [
      Alcotest.test_case "sub-components generated" `Quick test_gen_sub_components;
    ]);
    ("generate_autolayout_float", [
      Alcotest.test_case "float spacing truncated" `Quick test_gen_autolayout_float_spacing;
    ]);
  ]
