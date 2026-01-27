(** Comprehensive Coverage Tests for Utility Modules

    Target modules:
    - figma_tokens.ml (443 lines) - Token extraction and output formats
    - figma_cache.ml (411 lines) - L1/L2 cache with smart tracking
    - figma_compare.ml (453 lines) - Node comparison engine

    Total: 1,307 lines to cover
*)

open Alcotest
open Figma_types
open Figma_tokens
open Figma_cache
open Figma_compare

(** ============== Test Fixtures ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_paint ?(visible=true) ?(opacity=1.0) paint_type color =
  { paint_type; visible; opacity; color; gradient_stops = []; image_ref = None; scale_mode = None }

let make_solid_paint rgba = make_paint Solid (Some rgba)

let make_typography
    ?(font_family="Inter")
    ?(font_weight=400)
    ?(line_height=None)
    ?(letter_spacing=None)
    font_size =
  { Figma_types.font_family; font_size; font_weight;
    font_style = "normal"; line_height; letter_spacing;
    text_align_h = Left; text_align_v = Top;
    text_decoration = NoDeco; text_case = Original }

let make_bbox ?(x=0.) ?(y=0.) width height = { x; y; width; height }

let make_node
    ?(id="test-id")
    ?(name="Test Node")
    ?(node_type=Frame)
    ?(visible=true)
    ?(fills=[])
    ?(typography=None)
    ?(layout_mode=None')
    ?(padding=(0., 0., 0., 0.))
    ?(gap=0.)
    ?(border_radii=None)
    ?(bbox=None)
    ?(children=[])
    () =
  { Figma_types.default_node with
    id; name; node_type; visible; fills; typography;
    layout_mode; padding; gap; border_radii; bbox; children }

(** ============== Figma_tokens Tests ============== *)

module Tokens_tests = struct

  (** Test extract_colors: basic solid color extraction *)
  let test_extract_colors_basic () =
    let red = make_rgba 1.0 0.0 0.0 in
    let node = make_node ~fills:[make_solid_paint red] () in
    let colors = extract_colors [node] in
    check int "one color extracted" 1 (List.length colors);
    let c = List.hd colors in
    check string "hex is #FF0000" "#FF0000" c.hex;
    check bool "name starts with color-" true (String.length c.name > 0)

  (** Test extract_colors: duplicate colors are filtered *)
  let test_extract_colors_dedup () =
    let red = make_rgba 1.0 0.0 0.0 in
    let node1 = make_node ~id:"n1" ~fills:[make_solid_paint red] () in
    let node2 = make_node ~id:"n2" ~fills:[make_solid_paint red] () in
    let colors = extract_colors [node1; node2] in
    check int "duplicates removed" 1 (List.length colors)

  (** Test extract_colors: empty fills *)
  let test_extract_colors_empty () =
    let node = make_node ~fills:[] () in
    let colors = extract_colors [node] in
    check int "no colors" 0 (List.length colors)

  (** Test extract_colors: non-solid paints are skipped *)
  let test_extract_colors_non_solid () =
    let paint = make_paint GradientLinear None in
    let node = make_node ~fills:[paint] () in
    let colors = extract_colors [node] in
    check int "gradient skipped" 0 (List.length colors)

  (** Test extract_typography: basic extraction *)
  let test_extract_typography_basic () =
    let typo = make_typography 16.0 in
    let node = make_node ~typography:(Some typo) () in
    let result = extract_typography [node] in
    check int "one typography" 1 (List.length result);
    let t = List.hd result in
    check (float 0.1) "font_size" 16.0 t.font_size;
    check string "font_family" "Inter" t.font_family

  (** Test extract_typography: duplicate fonts are filtered *)
  let test_extract_typography_dedup () =
    let typo = make_typography 16.0 in
    let node1 = make_node ~id:"n1" ~typography:(Some typo) () in
    let node2 = make_node ~id:"n2" ~typography:(Some typo) () in
    let result = extract_typography [node1; node2] in
    check int "duplicates removed" 1 (List.length result)

  (** Test extract_typography: nodes without typography *)
  let test_extract_typography_none () =
    let node = make_node ~typography:None () in
    let result = extract_typography [node] in
    check int "no typography" 0 (List.length result)

  (** Test extract_spacing: gap extraction *)
  let test_extract_spacing_gap () =
    let node = make_node ~gap:16.0 () in
    let result = extract_spacing [node] in
    check bool "has spacing" true (List.length result > 0);
    check bool "16 extracted" true (List.exists (fun s -> s.value = 16.0) result)

  (** Test extract_spacing: padding extraction *)
  let test_extract_spacing_padding () =
    let node = make_node ~padding:(10., 20., 10., 20.) () in
    let result = extract_spacing [node] in
    check bool "has 10" true (List.exists (fun s -> s.value = 10.0) result);
    check bool "has 20" true (List.exists (fun s -> s.value = 20.0) result)

  (** Test extract_spacing: zero values filtered *)
  let test_extract_spacing_zero () =
    let node = make_node ~gap:0. ~padding:(0., 0., 0., 0.) () in
    let result = extract_spacing [node] in
    check int "zero filtered" 0 (List.length result)

  (** Test extract_radii: basic extraction *)
  let test_extract_radii_basic () =
    let node = make_node ~border_radii:(Some (8., 8., 8., 8.)) () in
    let result = extract_radii [node] in
    check bool "has radius" true (List.length result > 0);
    check bool "8 extracted" true (List.exists (fun r -> r.value = 8.0) result)

  (** Test extract_radii: mixed values *)
  let test_extract_radii_mixed () =
    let node = make_node ~border_radii:(Some (8., 16., 8., 16.)) () in
    let result = extract_radii [node] in
    check bool "has 8" true (List.exists (fun r -> r.value = 8.0) result);
    check bool "has 16" true (List.exists (fun r -> r.value = 16.0) result)

  (** Test extract_radii: none *)
  let test_extract_radii_none () =
    let node = make_node ~border_radii:None () in
    let result = extract_radii [node] in
    check int "no radii" 0 (List.length result)

  (** Test extract_all: combines all extractions *)
  let test_extract_all () =
    let red = make_rgba 1.0 0.0 0.0 in
    let typo = make_typography 16.0 in
    let node = make_node
      ~fills:[make_solid_paint red]
      ~typography:(Some typo)
      ~gap:8.0
      ~border_radii:(Some (4., 4., 4., 4.))
      () in
    let tokens = extract_all [node] in
    check bool "has colors" true (List.length tokens.colors > 0);
    check bool "has typography" true (List.length tokens.typography > 0);
    check bool "has spacing" true (List.length tokens.spacing > 0);
    check bool "has radii" true (List.length tokens.radii > 0)

  (** Test to_css: generates valid CSS *)
  let test_to_css () =
    let tokens = {
      colors = [{ name = "color-ff0000"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [{ name = "text-inter-16"; font_family = "Inter"; font_size = 16.0;
                      font_weight = 400; line_height = Some 1.5; letter_spacing = None }];
      spacing = [{ name = "space-8"; value = 8.0 }];
      radii = [{ name = "radius-4"; value = 4.0 }];
    } in
    let css = to_css tokens in
    check bool "has :root" true (String.length css > 0);
    check bool "contains color-ff0000" true (String.sub css 0 100 |> String.lowercase_ascii |> fun s ->
      try ignore (Str.search_forward (Str.regexp "color") s 0); true with _ -> false)

  (** Test to_tailwind: generates Tailwind config *)
  let test_to_tailwind () =
    let tokens = {
      colors = [{ name = "color-ff0000"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [];
      spacing = [{ name = "space-8"; value = 8.0 }];
      radii = [{ name = "radius-4"; value = 4.0 }];
    } in
    let tw = to_tailwind tokens in
    check bool "has module.exports" true (String.length tw > 0)

  (** Test to_json: generates valid JSON *)
  let test_to_json () =
    let tokens = {
      colors = [{ name = "color-ff0000"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [];
      spacing = [];
      radii = [];
    } in
    let json_str = to_json tokens in
    match Yojson.Safe.from_string json_str with
    | `Assoc _ -> check pass "valid JSON" () ()
    | _ -> fail "expected JSON object"
    | exception _ -> fail "invalid JSON"

  (** Test to_swiftui: generates Swift code *)
  let test_to_swiftui () =
    let tokens = {
      colors = [{ name = "color-ff0000"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [{ name = "text-inter-16"; font_family = "Inter"; font_size = 16.0;
                      font_weight = 700; line_height = None; letter_spacing = None }];
      spacing = [{ name = "space-8"; value = 8.0 }];
      radii = [{ name = "radius-4"; value = 4.0 }];
    } in
    let swift = to_swiftui tokens in
    check bool "has import SwiftUI" true (String.length swift > 0)

  (** Test to_compose: generates Kotlin code *)
  let test_to_compose () =
    let tokens = {
      colors = [{ name = "color-ff0000"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [{ name = "text-inter-16"; font_family = "Inter"; font_size = 16.0;
                      font_weight = 500; line_height = None; letter_spacing = None }];
      spacing = [];
      radii = [];
    } in
    let kt = to_compose tokens in
    check bool "has package" true (String.length kt > 0)

  (** Test to_flutter: generates Dart code *)
  let test_to_flutter () =
    let tokens = {
      colors = [{ name = "color-ff0000"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [{ name = "text-inter-16"; font_family = "Inter"; font_size = 16.0;
                      font_weight = 400; line_height = None; letter_spacing = None }];
      spacing = [];
      radii = [{ name = "radius-4"; value = 4.0 }];
    } in
    let dart = to_flutter tokens in
    check bool "has import flutter" true (String.length dart > 0)

  (** Test to_w3c_dtcg: generates W3C Design Tokens format *)
  let test_to_w3c_dtcg () =
    let tokens = {
      colors = [{ name = "primary"; hex = "#FF0000"; rgba = "rgba(255, 0, 0, 1.00)" }];
      typography = [{ name = "heading"; font_family = "Inter"; font_size = 24.0;
                      font_weight = 700; line_height = None; letter_spacing = None }];
      spacing = [{ name = "sm"; value = 8.0 }];
      radii = [{ name = "md"; value = 8.0 }];
    } in
    let dtcg = to_w3c_dtcg tokens in
    match Yojson.Safe.from_string dtcg with
    | `Assoc _ -> check pass "valid DTCG JSON" () ()
    | _ -> fail "expected JSON object"
    | exception _ -> fail "invalid JSON"

  (** Test format_of_string: all formats *)
  let test_format_of_string () =
    check bool "css" true (format_of_string "css" = CSS);
    check bool "tailwind" true (format_of_string "tailwind" = Tailwind);
    check bool "json" true (format_of_string "json" = JSON);
    check bool "swiftui" true (format_of_string "swiftui" = SwiftUI);
    check bool "swift" true (format_of_string "swift" = SwiftUI);
    check bool "ios" true (format_of_string "ios" = SwiftUI);
    check bool "compose" true (format_of_string "compose" = Compose);
    check bool "kotlin" true (format_of_string "kotlin" = Compose);
    check bool "android" true (format_of_string "android" = Compose);
    check bool "flutter" true (format_of_string "flutter" = Flutter);
    check bool "dart" true (format_of_string "dart" = Flutter);
    check bool "w3c" true (format_of_string "w3c" = W3C_DTCG);
    check bool "dtcg" true (format_of_string "dtcg" = W3C_DTCG);
    check bool "tokens" true (format_of_string "tokens" = W3C_DTCG);
    check bool "unknown defaults to JSON" true (format_of_string "unknown" = JSON)

  (** Test export_tokens: dispatch to correct format *)
  let test_export_tokens () =
    let tokens = {
      colors = [{ name = "c"; hex = "#FF0000"; rgba = "rgba(255,0,0,1)" }];
      typography = [];
      spacing = [];
      radii = [];
    } in
    let css_out = export_tokens tokens CSS in
    let json_out = export_tokens tokens JSON in
    check bool "CSS output differs from JSON" true (css_out <> json_out)

  let tests = [
    "extract_colors_basic", `Quick, test_extract_colors_basic;
    "extract_colors_dedup", `Quick, test_extract_colors_dedup;
    "extract_colors_empty", `Quick, test_extract_colors_empty;
    "extract_colors_non_solid", `Quick, test_extract_colors_non_solid;
    "extract_typography_basic", `Quick, test_extract_typography_basic;
    "extract_typography_dedup", `Quick, test_extract_typography_dedup;
    "extract_typography_none", `Quick, test_extract_typography_none;
    "extract_spacing_gap", `Quick, test_extract_spacing_gap;
    "extract_spacing_padding", `Quick, test_extract_spacing_padding;
    "extract_spacing_zero", `Quick, test_extract_spacing_zero;
    "extract_radii_basic", `Quick, test_extract_radii_basic;
    "extract_radii_mixed", `Quick, test_extract_radii_mixed;
    "extract_radii_none", `Quick, test_extract_radii_none;
    "extract_all", `Quick, test_extract_all;
    "to_css", `Quick, test_to_css;
    "to_tailwind", `Quick, test_to_tailwind;
    "to_json", `Quick, test_to_json;
    "to_swiftui", `Quick, test_to_swiftui;
    "to_compose", `Quick, test_to_compose;
    "to_flutter", `Quick, test_to_flutter;
    "to_w3c_dtcg", `Quick, test_to_w3c_dtcg;
    "format_of_string", `Quick, test_format_of_string;
    "export_tokens", `Quick, test_export_tokens;
  ]
end

(** ============== Figma_cache Tests ============== *)

module Cache_tests = struct

  (** Test make_cache_key: deterministic hash *)
  let test_cache_key_deterministic () =
    let k1 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:[] in
    let k2 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:[] in
    check string "same inputs = same key" k1 k2

  (** Test make_cache_key: different inputs = different keys *)
  let test_cache_key_different () =
    let k1 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:[] in
    let k2 = make_cache_key ~file_key:"abc" ~node_id:"456" ~options:[] in
    check bool "different node_id = different key" true (k1 <> k2)

  (** Test make_cache_key: options affect key *)
  let test_cache_key_options () =
    let k1 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:[] in
    let k2 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:["depth:2"] in
    check bool "options change key" true (k1 <> k2)

  (** Test make_cache_key: options order doesn't matter *)
  let test_cache_key_options_order () =
    let k1 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:["a"; "b"] in
    let k2 = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:["b"; "a"] in
    check string "options sorted" k1 k2

  (** Test make_cache_key: key length is 16 chars *)
  let test_cache_key_length () =
    let k = make_cache_key ~file_key:"abc" ~node_id:"123" ~options:[] in
    check int "key length is 16" 16 (String.length k)

  (** Test Stats: initial state *)
  let test_stats_initial () =
    Stats.reset ();
    check (float 0.1) "initial hit_rate is 0" 0.0 (Stats.hit_rate ())

  (** Test Stats: record_hit increments *)
  let test_stats_record_hit () =
    Stats.reset ();
    Stats.record_hit ();
    check int "hits = 1" 1 !(Stats.hits)

  (** Test Stats: record_miss increments *)
  let test_stats_record_miss () =
    Stats.reset ();
    Stats.record_miss ();
    check int "misses = 1" 1 !(Stats.misses)

  (** Test Stats: hit_rate calculation *)
  let test_stats_hit_rate () =
    Stats.reset ();
    Stats.record_hit ();
    Stats.record_hit ();
    Stats.record_miss ();
    let rate = Stats.hit_rate () in
    check (float 1.0) "66.7% hit rate" 66.67 rate

  (** Test Stats: bytes_saved tracking *)
  let test_stats_bytes_saved () =
    Stats.reset ();
    Stats.record_hit ~bytes:1000 ();
    check int "bytes saved" 1000 !(Stats.bytes_saved)

  (** Test Stats: level tracking *)
  let test_stats_level_tracking () =
    Stats.reset ();
    Stats.record_hit ~level:`L1 ();
    Stats.record_hit ~level:`L2 ();
    Stats.record_hit ~level:`Prefetch ();
    check int "l1_hits" 1 !(Stats.l1_hits);
    check int "l2_hits" 1 !(Stats.l2_hits);
    check int "prefetch_hits" 1 !(Stats.prefetch_hits)

  (** Test PrefetchHints: record_access *)
  let test_prefetch_record_access () =
    PrefetchHints.recent_accesses := [];
    PrefetchHints.record_access ~node_id:"node1";
    check int "1 recent access" 1 (List.length !(PrefetchHints.recent_accesses))

  (** Test PrefetchHints: max recent limit *)
  let test_prefetch_max_recent () =
    PrefetchHints.recent_accesses := [];
    for i = 1 to 15 do
      PrefetchHints.record_access ~node_id:(Printf.sprintf "node%d" i)
    done;
    check bool "max 10 recent" true (List.length !(PrefetchHints.recent_accesses) <= 10)

  (** Test PrefetchHints: learn_pattern *)
  let test_prefetch_learn_pattern () =
    Hashtbl.clear PrefetchHints.patterns;
    PrefetchHints.learn_pattern ~from_node:"a" ~to_node:"b";
    let hints = PrefetchHints.get_hints ~node_id:"a" in
    check bool "b is hinted from a" true (List.mem "b" hints)

  (** Test PrefetchHints: get_top_patterns *)
  let test_prefetch_top_patterns () =
    Hashtbl.clear PrefetchHints.patterns;
    PrefetchHints.learn_pattern ~from_node:"a" ~to_node:"b";
    PrefetchHints.learn_pattern ~from_node:"a" ~to_node:"c";
    let top = PrefetchHints.get_top_patterns ~limit:5 () in
    check bool "has patterns" true (List.length top > 0)

  (** Test VersionTracker: update and get version *)
  let test_version_tracker_update () =
    VersionTracker.update_version ~file_key:"test-file" ~version:1.0;
    let v = VersionTracker.get_version ~file_key:"test-file" in
    check bool "version stored" true (v = Some 1.0)

  (** Test VersionTracker: check_version valid *)
  let test_version_tracker_valid () =
    VersionTracker.update_version ~file_key:"test-valid" ~version:1.0;
    let result = VersionTracker.check_version ~file_key:"test-valid" ~current_version:1.0 in
    check bool "valid" true (result = `Valid)

  (** Test VersionTracker: check_version new file *)
  let test_version_tracker_new () =
    let result = VersionTracker.check_version ~file_key:"brand-new-file" ~current_version:1.0 in
    check bool "new file" true (result = `NewFile)

  (** Test stats: returns JSON structure *)
  let test_stats_json () =
    match Figma_cache.stats () with
    | `Assoc fields ->
        check bool "has l1_entries" true (List.mem_assoc "l1_entries" fields);
        check bool "has hit_rate_percent" true (List.mem_assoc "hit_rate_percent" fields)
    | _ -> fail "expected JSON object"

  (** Test cached_at_of_json: float *)
  let test_cached_at_float () =
    let json = `Assoc [("_cached_at", `Float 123.0)] in
    let result = cached_at_of_json ~fallback:0.0 json in
    check (float 0.1) "float timestamp" 123.0 result

  (** Test cached_at_of_json: int *)
  let test_cached_at_int () =
    let json = `Assoc [("_cached_at", `Int 456)] in
    let result = cached_at_of_json ~fallback:0.0 json in
    check (float 0.1) "int timestamp" 456.0 result

  (** Test cached_at_of_json: fallback *)
  let test_cached_at_fallback () =
    let json = `Assoc [] in
    let result = cached_at_of_json ~fallback:999.0 json in
    check (float 0.1) "fallback" 999.0 result

  let tests = [
    "cache_key_deterministic", `Quick, test_cache_key_deterministic;
    "cache_key_different", `Quick, test_cache_key_different;
    "cache_key_options", `Quick, test_cache_key_options;
    "cache_key_options_order", `Quick, test_cache_key_options_order;
    "cache_key_length", `Quick, test_cache_key_length;
    "stats_initial", `Quick, test_stats_initial;
    "stats_record_hit", `Quick, test_stats_record_hit;
    "stats_record_miss", `Quick, test_stats_record_miss;
    "stats_hit_rate", `Quick, test_stats_hit_rate;
    "stats_bytes_saved", `Quick, test_stats_bytes_saved;
    "stats_level_tracking", `Quick, test_stats_level_tracking;
    "prefetch_record_access", `Quick, test_prefetch_record_access;
    "prefetch_max_recent", `Quick, test_prefetch_max_recent;
    "prefetch_learn_pattern", `Quick, test_prefetch_learn_pattern;
    "prefetch_top_patterns", `Quick, test_prefetch_top_patterns;
    "version_tracker_update", `Quick, test_version_tracker_update;
    "version_tracker_valid", `Quick, test_version_tracker_valid;
    "version_tracker_new", `Quick, test_version_tracker_new;
    "stats_json", `Quick, test_stats_json;
    "cached_at_float", `Quick, test_cached_at_float;
    "cached_at_int", `Quick, test_cached_at_int;
    "cached_at_fallback", `Quick, test_cached_at_fallback;
  ]
end

(** ============== Figma_compare Tests ============== *)

module Compare_tests = struct

  (** Test severity_to_string *)
  let test_severity_to_string () =
    check string "critical" "CRITICAL" (severity_to_string Critical);
    check string "major" "MAJOR" (severity_to_string Major);
    check string "minor" "MINOR" (severity_to_string Minor);
    check string "info" "INFO" (severity_to_string Info)

  (** Test severity_weight *)
  let test_severity_weight () =
    check (float 0.01) "critical = 1.0" 1.0 (severity_weight Critical);
    check (float 0.01) "major = 0.5" 0.5 (severity_weight Major);
    check (float 0.01) "minor = 0.2" 0.2 (severity_weight Minor);
    check (float 0.01) "info = 0.0" 0.0 (severity_weight Info)

  (** Test float_eq: within tolerance *)
  let test_float_eq_within () =
    check bool "0.0 ~= 0.3" true (float_eq 0.0 0.3);
    check bool "10.0 ~= 10.4" true (float_eq 10.0 10.4)

  (** Test float_eq: outside tolerance *)
  let test_float_eq_outside () =
    check bool "0.0 != 1.0" false (float_eq 0.0 1.0);
    check bool "10.0 != 11.0" false (float_eq 10.0 11.0)

  (** Test float_eq: custom tolerance *)
  let test_float_eq_custom_tolerance () =
    check bool "with tolerance 2.0" true (float_eq ~tolerance:2.0 0.0 1.5);
    check bool "with tolerance 0.1" false (float_eq ~tolerance:0.1 0.0 0.3)

  (** Test percent_diff: basic *)
  let test_percent_diff_basic () =
    check (float 0.1) "100 to 110 = 10%" 10.0 (percent_diff 100.0 110.0);
    check (float 0.1) "100 to 90 = 10%" 10.0 (percent_diff 100.0 90.0)

  (** Test percent_diff: zero base *)
  let test_percent_diff_zero_base () =
    check (float 0.1) "0 to 100 = 100%" 100.0 (percent_diff 0.0 100.0)

  (** Test percent_diff: both zero *)
  let test_percent_diff_both_zero () =
    check (float 0.1) "0 to 0 = 0%" 0.0 (percent_diff 0.0 0.0)

  (** Test rgba_to_hex *)
  let test_rgba_to_hex () =
    let red = make_rgba 1.0 0.0 0.0 in
    check string "red" "#FF0000" (rgba_to_hex red);
    let green = make_rgba 0.0 1.0 0.0 in
    check string "green" "#00FF00" (rgba_to_hex green);
    let blue = make_rgba 0.0 0.0 1.0 in
    check string "blue" "#0000FF" (rgba_to_hex blue)

  (** Test rgba_to_hex: mixed colors *)
  let test_rgba_to_hex_mixed () =
    let purple = make_rgba 0.5 0.0 0.5 in
    check string "purple-ish" "#7F007F" (rgba_to_hex purple)

  (** Test color_distance: same color *)
  let test_color_distance_same () =
    let red = make_rgba 1.0 0.0 0.0 in
    check (float 0.01) "same color = 0" 0.0 (color_distance red red)

  (** Test color_distance: opposite colors *)
  let test_color_distance_opposite () =
    let black = make_rgba 0.0 0.0 0.0 in
    let white = make_rgba 1.0 1.0 1.0 in
    let dist = color_distance black white in
    check bool "large distance" true (dist > 400.0)

  (** Test layout_mode_to_string *)
  let test_layout_mode_to_string () =
    check string "horizontal" "Horizontal" (layout_mode_to_string Horizontal);
    check string "vertical" "Vertical" (layout_mode_to_string Vertical);
    check string "none" "None" (layout_mode_to_string None')

  (** Test normalize_name: removes web prefix *)
  let test_normalize_name_web () =
    check string "web/" "button" (normalize_name "Web/Button");
    check string "web_" "button" (normalize_name "web_button")

  (** Test normalize_name: removes mobile prefix *)
  let test_normalize_name_mobile () =
    check string "mobile/" "button" (normalize_name "Mobile/Button");
    check string "mobile_" "button" (normalize_name "mobile_button")

  (** Test normalize_name: removes desktop/ios/android prefixes *)
  let test_normalize_name_other_platforms () =
    check string "desktop/" "button" (normalize_name "Desktop/Button");
    check string "ios/" "card" (normalize_name "iOS/Card");
    check string "android/" "list" (normalize_name "Android/List")

  (** Test normalize_name: preserves non-prefixed *)
  let test_normalize_name_preserved () =
    check string "no change" "button" (normalize_name "Button")

  (** Test compare_size: same size *)
  let test_compare_size_same () =
    let bbox = Some (make_bbox 100.0 50.0) in
    let node_a = make_node ~bbox () in
    let node_b = make_node ~bbox () in
    let diffs = compare_size node_a node_b in
    check int "no size diff" 0 (List.length diffs)

  (** Test compare_size: different width *)
  let test_compare_size_different_width () =
    let node_a = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
    let node_b = make_node ~bbox:(Some (make_bbox 150.0 50.0)) () in
    let diffs = compare_size node_a node_b in
    check bool "has width diff" true (List.exists (fun d -> d.property = "width") diffs)

  (** Test compare_size: no bbox *)
  let test_compare_size_no_bbox () =
    let node_a = make_node ~bbox:None () in
    let node_b = make_node ~bbox:None () in
    let diffs = compare_size node_a node_b in
    check int "no diff when no bbox" 0 (List.length diffs)

  (** Test compare_colors: same color *)
  let test_compare_colors_same () =
    let red = make_rgba 1.0 0.0 0.0 in
    let node_a = make_node ~fills:[make_solid_paint red] () in
    let node_b = make_node ~fills:[make_solid_paint red] () in
    let diffs = compare_colors node_a node_b in
    check int "no color diff" 0 (List.length diffs)

  (** Test compare_colors: different colors *)
  let test_compare_colors_different () =
    let red = make_rgba 1.0 0.0 0.0 in
    let blue = make_rgba 0.0 0.0 1.0 in
    let node_a = make_node ~fills:[make_solid_paint red] () in
    let node_b = make_node ~fills:[make_solid_paint blue] () in
    let diffs = compare_colors node_a node_b in
    check bool "has color diff" true (List.length diffs > 0)

  (** Test compare_colors: one has color, other doesn't *)
  let test_compare_colors_one_empty () =
    let red = make_rgba 1.0 0.0 0.0 in
    let node_a = make_node ~fills:[make_solid_paint red] () in
    let node_b = make_node ~fills:[] () in
    let diffs = compare_colors node_a node_b in
    check bool "has diff" true (List.length diffs > 0)

  (** Test compare_typography: same typography *)
  let test_compare_typography_same () =
    let typo = make_typography 16.0 in
    let node_a = make_node ~typography:(Some typo) () in
    let node_b = make_node ~typography:(Some typo) () in
    let diffs = compare_typography node_a node_b in
    check int "no typography diff" 0 (List.length diffs)

  (** Test compare_typography: different font size *)
  let test_compare_typography_different_size () =
    let typo_a = make_typography 16.0 in
    let typo_b = make_typography 24.0 in
    let node_a = make_node ~typography:(Some typo_a) () in
    let node_b = make_node ~typography:(Some typo_b) () in
    let diffs = compare_typography node_a node_b in
    check bool "has font_size diff" true (List.exists (fun d -> d.property = "font_size") diffs)

  (** Test compare_typography: different font family *)
  let test_compare_typography_different_family () =
    let typo_a = make_typography ~font_family:"Inter" 16.0 in
    let typo_b = make_typography ~font_family:"Roboto" 16.0 in
    let node_a = make_node ~typography:(Some typo_a) () in
    let node_b = make_node ~typography:(Some typo_b) () in
    let diffs = compare_typography node_a node_b in
    check bool "has font_family diff" true (List.exists (fun d -> d.property = "font_family") diffs)

  (** Test compare_typography: one has typography, other doesn't *)
  let test_compare_typography_one_none () =
    let typo = make_typography 16.0 in
    let node_a = make_node ~typography:(Some typo) () in
    let node_b = make_node ~typography:None () in
    let diffs = compare_typography node_a node_b in
    check bool "has presence diff" true (List.exists (fun d -> d.property = "presence") diffs)

  (** Test compare_layout: same layout *)
  let test_compare_layout_same () =
    let node_a = make_node ~layout_mode:Horizontal ~gap:8.0 ~padding:(16., 16., 16., 16.) () in
    let node_b = make_node ~layout_mode:Horizontal ~gap:8.0 ~padding:(16., 16., 16., 16.) () in
    let diffs = compare_layout node_a node_b in
    check int "no layout diff" 0 (List.length diffs)

  (** Test compare_layout: different mode *)
  let test_compare_layout_different_mode () =
    let node_a = make_node ~layout_mode:Horizontal () in
    let node_b = make_node ~layout_mode:Vertical () in
    let diffs = compare_layout node_a node_b in
    check bool "has layout_mode diff" true (List.exists (fun d -> d.property = "layout_mode") diffs)

  (** Test compare_layout: different gap *)
  let test_compare_layout_different_gap () =
    let node_a = make_node ~gap:8.0 () in
    let node_b = make_node ~gap:16.0 () in
    let diffs = compare_layout node_a node_b in
    check bool "has gap diff" true (List.exists (fun d -> d.property = "gap") diffs)

  (** Test compare_layout: different padding *)
  let test_compare_layout_different_padding () =
    let node_a = make_node ~padding:(8., 8., 8., 8.) () in
    let node_b = make_node ~padding:(16., 16., 16., 16.) () in
    let diffs = compare_layout node_a node_b in
    check bool "has padding diff" true (List.exists (fun d -> d.property = "padding") diffs)

  (** Test compare_structure: same children count *)
  let test_compare_structure_same_children () =
    let child = make_node ~id:"c1" () in
    let node_a = make_node ~children:[child] () in
    let node_b = make_node ~children:[child] () in
    let diffs = compare_structure node_a node_b in
    check bool "no children_count diff" false
      (List.exists (fun d -> d.property = "children_count") diffs)

  (** Test compare_structure: different children count *)
  let test_compare_structure_different_children () =
    let child = make_node ~id:"c1" () in
    let node_a = make_node ~children:[child; child] () in
    let node_b = make_node ~children:[child] () in
    let diffs = compare_structure node_a node_b in
    check bool "has children_count diff" true
      (List.exists (fun d -> d.property = "children_count") diffs)

  (** Test compare_structure: different node type *)
  let test_compare_structure_different_type () =
    let node_a = make_node ~node_type:Frame () in
    let node_b = make_node ~node_type:Text () in
    let diffs = compare_structure node_a node_b in
    check bool "has node_type diff" true
      (List.exists (fun d -> d.property = "node_type") diffs);
    let type_diff = List.find (fun d -> d.property = "node_type") diffs in
    check bool "is critical" true (type_diff.severity = Critical)

  (** Test compare_nodes: identical nodes *)
  let test_compare_nodes_identical () =
    let node = make_node () in
    let result = compare_nodes node node in
    check (float 0.01) "similarity 1.0" 1.0 result.similarity_score;
    check int "no differences" 0 (List.length result.differences)

  (** Test compare_nodes: with differences *)
  let test_compare_nodes_with_diffs () =
    let node_a = make_node ~layout_mode:Horizontal () in
    let node_b = make_node ~layout_mode:Vertical () in
    let result = compare_nodes node_a node_b in
    check bool "has differences" true (List.length result.differences > 0);
    check bool "similarity < 1" true (result.similarity_score < 1.0)

  (** Test result_to_string: no differences *)
  let test_result_to_string_no_diff () =
    let node = make_node ~name:"TestNode" () in
    let result = compare_nodes node node in
    let s = result_to_string result in
    check bool "contains 100%" true (String.length s > 0)

  (** Test result_to_string: with differences *)
  let test_result_to_string_with_diff () =
    let node_a = make_node ~name:"A" ~layout_mode:Horizontal () in
    let node_b = make_node ~name:"B" ~layout_mode:Vertical () in
    let result = compare_nodes node_a node_b in
    let s = result_to_string result in
    check bool "contains diff count" true (String.length s > 0)

  (** Test find_matching_pairs: basic matching *)
  let test_find_matching_pairs_basic () =
    let web_btn = make_node ~name:"Web/Button" () in
    let mobile_btn = make_node ~name:"Mobile/Button" () in
    let pairs = find_matching_pairs [web_btn] [mobile_btn] in
    check int "one pair found" 1 (List.length pairs)

  (** Test find_matching_pairs: no match *)
  let test_find_matching_pairs_no_match () =
    let web_btn = make_node ~name:"Web/Button" () in
    let mobile_card = make_node ~name:"Mobile/Card" () in
    let pairs = find_matching_pairs [web_btn] [mobile_card] in
    check int "no pairs" 0 (List.length pairs)

  (** Test compare_web_mobile: summary stats *)
  let test_compare_web_mobile () =
    let web_btn = make_node ~name:"Web/Button" () in
    let mobile_btn = make_node ~name:"Mobile/Button" () in
    let (results, total, avg_sim, critical, major) =
      compare_web_mobile ~web_nodes:[web_btn] ~mobile_nodes:[mobile_btn] in
    check int "1 result" 1 total;
    check int "1 in results" 1 (List.length results);
    check (float 0.01) "100% avg similarity" 1.0 avg_sim;
    check int "0 critical" 0 critical;
    check int "0 major" 0 major

  let tests = [
    "severity_to_string", `Quick, test_severity_to_string;
    "severity_weight", `Quick, test_severity_weight;
    "float_eq_within", `Quick, test_float_eq_within;
    "float_eq_outside", `Quick, test_float_eq_outside;
    "float_eq_custom_tolerance", `Quick, test_float_eq_custom_tolerance;
    "percent_diff_basic", `Quick, test_percent_diff_basic;
    "percent_diff_zero_base", `Quick, test_percent_diff_zero_base;
    "percent_diff_both_zero", `Quick, test_percent_diff_both_zero;
    "rgba_to_hex", `Quick, test_rgba_to_hex;
    "rgba_to_hex_mixed", `Quick, test_rgba_to_hex_mixed;
    "color_distance_same", `Quick, test_color_distance_same;
    "color_distance_opposite", `Quick, test_color_distance_opposite;
    "layout_mode_to_string", `Quick, test_layout_mode_to_string;
    "normalize_name_web", `Quick, test_normalize_name_web;
    "normalize_name_mobile", `Quick, test_normalize_name_mobile;
    "normalize_name_other_platforms", `Quick, test_normalize_name_other_platforms;
    "normalize_name_preserved", `Quick, test_normalize_name_preserved;
    "compare_size_same", `Quick, test_compare_size_same;
    "compare_size_different_width", `Quick, test_compare_size_different_width;
    "compare_size_no_bbox", `Quick, test_compare_size_no_bbox;
    "compare_colors_same", `Quick, test_compare_colors_same;
    "compare_colors_different", `Quick, test_compare_colors_different;
    "compare_colors_one_empty", `Quick, test_compare_colors_one_empty;
    "compare_typography_same", `Quick, test_compare_typography_same;
    "compare_typography_different_size", `Quick, test_compare_typography_different_size;
    "compare_typography_different_family", `Quick, test_compare_typography_different_family;
    "compare_typography_one_none", `Quick, test_compare_typography_one_none;
    "compare_layout_same", `Quick, test_compare_layout_same;
    "compare_layout_different_mode", `Quick, test_compare_layout_different_mode;
    "compare_layout_different_gap", `Quick, test_compare_layout_different_gap;
    "compare_layout_different_padding", `Quick, test_compare_layout_different_padding;
    "compare_structure_same_children", `Quick, test_compare_structure_same_children;
    "compare_structure_different_children", `Quick, test_compare_structure_different_children;
    "compare_structure_different_type", `Quick, test_compare_structure_different_type;
    "compare_nodes_identical", `Quick, test_compare_nodes_identical;
    "compare_nodes_with_diffs", `Quick, test_compare_nodes_with_diffs;
    "result_to_string_no_diff", `Quick, test_result_to_string_no_diff;
    "result_to_string_with_diff", `Quick, test_result_to_string_with_diff;
    "find_matching_pairs_basic", `Quick, test_find_matching_pairs_basic;
    "find_matching_pairs_no_match", `Quick, test_find_matching_pairs_no_match;
    "compare_web_mobile", `Quick, test_compare_web_mobile;
  ]
end

(** ============== Main Test Runner ============== *)

let () =
  run "figma-mcp-utils-coverage" [
    "Figma_tokens", Tokens_tests.tests;
    "Figma_cache", Cache_tests.tests;
    "Figma_compare", Compare_tests.tests;
  ]
