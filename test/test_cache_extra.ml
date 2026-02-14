(** Extra Coverage Tests for figma_cache.ml
    Targets: make_cache_key, cached_at_of_json, Stats, PrefetchHints, enforce_l1_limit *)

let () =
  let open Alcotest in

  (* ============== make_cache_key ============== *)
  let test_cache_key_basic () =
    let k = Figma_cache.make_cache_key ~file_key:"abc" ~node_id:"1:2" ~options:[] in
    check int "key length" 16 (String.length k);
    (* MD5 hex is 0-9a-f *)
    String.iter (fun c ->
      let ok = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
      check bool (Printf.sprintf "hex char '%c'" c) true ok
    ) k
  in
  let test_cache_key_deterministic () =
    let k1 = Figma_cache.make_cache_key ~file_key:"f" ~node_id:"1:1" ~options:["a";"b"] in
    let k2 = Figma_cache.make_cache_key ~file_key:"f" ~node_id:"1:1" ~options:["a";"b"] in
    check string "same input → same key" k1 k2
  in
  let test_cache_key_options_sorted () =
    (* Options are sorted, so order shouldn't matter *)
    let k1 = Figma_cache.make_cache_key ~file_key:"f" ~node_id:"1:1" ~options:["a";"b"] in
    let k2 = Figma_cache.make_cache_key ~file_key:"f" ~node_id:"1:1" ~options:["b";"a"] in
    check string "option order irrelevant" k1 k2
  in
  let test_cache_key_different_inputs () =
    let k1 = Figma_cache.make_cache_key ~file_key:"f1" ~node_id:"1:1" ~options:[] in
    let k2 = Figma_cache.make_cache_key ~file_key:"f2" ~node_id:"1:1" ~options:[] in
    check bool "different file_key → different key" true (k1 <> k2)
  in
  let test_cache_key_empty_options () =
    let k1 = Figma_cache.make_cache_key ~file_key:"f" ~node_id:"1:1" ~options:[] in
    let k2 = Figma_cache.make_cache_key ~file_key:"f" ~node_id:"1:1" ~options:["x"] in
    check bool "empty vs non-empty options differ" true (k1 <> k2)
  in

  (* ============== cached_at_of_json ============== *)
  let test_cached_at_float () =
    let json = `Assoc [("_cached_at", `Float 12345.0)] in
    let ts = Figma_cache.cached_at_of_json ~fallback:0.0 json in
    check (float 0.001) "float ts" 12345.0 ts
  in
  let test_cached_at_int () =
    let json = `Assoc [("_cached_at", `Int 99999)] in
    let ts = Figma_cache.cached_at_of_json ~fallback:0.0 json in
    check (float 0.001) "int ts" 99999.0 ts
  in
  let test_cached_at_missing () =
    let json = `Assoc [("other", `String "x")] in
    let ts = Figma_cache.cached_at_of_json ~fallback:42.0 json in
    check (float 0.001) "fallback" 42.0 ts
  in
  let test_cached_at_wrong_type () =
    let json = `Assoc [("_cached_at", `String "not a number")] in
    let ts = Figma_cache.cached_at_of_json ~fallback:7.0 json in
    check (float 0.001) "fallback on wrong type" 7.0 ts
  in

  (* ============== Stats module ============== *)
  let test_stats_initial () =
    Figma_cache.Stats.reset ();
    check (float 0.001) "initial hit rate" 0.0 (Figma_cache.Stats.hit_rate ())
  in
  let test_stats_hits () =
    Figma_cache.Stats.reset ();
    Figma_cache.Stats.record_hit ();
    Figma_cache.Stats.record_hit ();
    Figma_cache.Stats.record_miss ();
    let rate = Figma_cache.Stats.hit_rate () in
    (* 2 hits / 3 total = 66.67% *)
    check bool "rate ~66.7%" true (rate > 66.0 && rate < 67.0)
  in
  let test_stats_l2_hits () =
    Figma_cache.Stats.reset ();
    Figma_cache.Stats.record_hit ~level:`L2 ~bytes:1024 ();
    check (float 0.001) "100% after l2 hit" 100.0 (Figma_cache.Stats.hit_rate ())
  in
  let test_stats_prefetch_hits () =
    Figma_cache.Stats.reset ();
    Figma_cache.Stats.record_hit ~level:`Prefetch ();
    check (float 0.001) "100% after prefetch hit" 100.0 (Figma_cache.Stats.hit_rate ())
  in
  let test_stats_all_misses () =
    Figma_cache.Stats.reset ();
    Figma_cache.Stats.record_miss ();
    Figma_cache.Stats.record_miss ();
    check (float 0.001) "0% all misses" 0.0 (Figma_cache.Stats.hit_rate ())
  in
  let test_stats_reset () =
    Figma_cache.Stats.record_hit ();
    Figma_cache.Stats.record_hit ();
    Figma_cache.Stats.reset ();
    check (float 0.001) "0% after reset" 0.0 (Figma_cache.Stats.hit_rate ())
  in

  (* ============== PrefetchHints module ============== *)
  let test_prefetch_record_access () =
    (* Just verify it doesn't crash *)
    Figma_cache.PrefetchHints.record_access ~node_id:"1:1";
    Figma_cache.PrefetchHints.record_access ~node_id:"1:2";
    ()
  in
  let test_prefetch_learn_pattern () =
    Figma_cache.PrefetchHints.learn_pattern ~from_node:"a" ~to_node:"b";
    let hints = Figma_cache.PrefetchHints.get_hints ~node_id:"a" in
    check bool "has hint b" true (List.mem "b" hints)
  in
  let test_prefetch_get_hints_empty () =
    let hints = Figma_cache.PrefetchHints.get_hints ~node_id:"nonexistent-node-xyz" in
    check int "no hints" 0 (List.length hints)
  in
  let test_prefetch_top_patterns () =
    Figma_cache.PrefetchHints.learn_pattern ~from_node:"x" ~to_node:"y1";
    Figma_cache.PrefetchHints.learn_pattern ~from_node:"x" ~to_node:"y2";
    let top = Figma_cache.PrefetchHints.get_top_patterns ~limit:5 () in
    check bool "has patterns" true (List.length top > 0)
  in
  let test_prefetch_analyze_chain () =
    (* Record a chain of accesses, then analyze *)
    Figma_cache.PrefetchHints.record_access ~node_id:"c1";
    Figma_cache.PrefetchHints.record_access ~node_id:"c2";
    Figma_cache.PrefetchHints.record_access ~node_id:"c3";
    Figma_cache.PrefetchHints.analyze_chain ();
    ()
  in

  (* ============== enforce_l1_limit ============== *)
  let test_enforce_l1_limit_no_eviction () =
    (* Clear and add a few entries — should be well under limit *)
    Hashtbl.reset Figma_cache.memory_cache;
    let now = Unix.gettimeofday () in
    Hashtbl.replace Figma_cache.memory_cache "test-key-1"
      { Figma_cache.payload = `Null; cached_at = now; last_access = now;
        file_key = "f"; node_id = "1:1" };
    Figma_cache.enforce_l1_limit ();
    check int "still has entry" 1 (Hashtbl.length Figma_cache.memory_cache);
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* ============== should_invalidate_entry ============== *)
  let test_invalidate_entry_match_all () =
    let entry : Figma_cache.cache_entry = {
      payload = `Null; cached_at = 0.0; last_access = 0.0;
      file_key = "fk1"; node_id = "n1"
    } in
    (* file_key matches, node_id=None => invalidate all nodes for that file *)
    check bool "match all" true
      (Figma_cache.should_invalidate_entry ~file_key:"fk1" ~node_id:None entry)
  in
  let test_invalidate_entry_match_specific () =
    let entry : Figma_cache.cache_entry = {
      payload = `Null; cached_at = 0.0; last_access = 0.0;
      file_key = "fk1"; node_id = "n1"
    } in
    check bool "match specific" true
      (Figma_cache.should_invalidate_entry ~file_key:"fk1" ~node_id:(Some "n1") entry)
  in
  let test_invalidate_entry_no_match_file () =
    let entry : Figma_cache.cache_entry = {
      payload = `Null; cached_at = 0.0; last_access = 0.0;
      file_key = "fk1"; node_id = "n1"
    } in
    check bool "no match file" false
      (Figma_cache.should_invalidate_entry ~file_key:"other" ~node_id:None entry)
  in
  let test_invalidate_entry_no_match_node () =
    let entry : Figma_cache.cache_entry = {
      payload = `Null; cached_at = 0.0; last_access = 0.0;
      file_key = "fk1"; node_id = "n1"
    } in
    check bool "no match node" false
      (Figma_cache.should_invalidate_entry ~file_key:"fk1" ~node_id:(Some "n2") entry)
  in

  (* ============== should_invalidate_json ============== *)
  let test_invalidate_json_match_all () =
    let json = `Assoc [("_file_key", `String "fk1"); ("_node_id", `String "n1")] in
    check bool "json match all" true
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:None json)
  in
  let test_invalidate_json_match_specific () =
    let json = `Assoc [("_file_key", `String "fk1"); ("_node_id", `String "n1")] in
    check bool "json match specific" true
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:(Some "n1") json)
  in
  let test_invalidate_json_no_match_node () =
    let json = `Assoc [("_file_key", `String "fk1"); ("_node_id", `String "n1")] in
    check bool "json no match node" false
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:(Some "n2") json)
  in
  let test_invalidate_json_no_file_key () =
    let json = `Assoc [("other", `String "x")] in
    check bool "json no file_key" false
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:None json)
  in
  let test_invalidate_json_wrong_file () =
    let json = `Assoc [("_file_key", `String "other"); ("_node_id", `String "n1")] in
    check bool "json wrong file" false
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:None json)
  in
  let test_invalidate_json_node_not_string () =
    let json = `Assoc [("_file_key", `String "fk1"); ("_node_id", `Int 42)] in
    check bool "json node not string" false
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:(Some "42") json)
  in

  (* ============== VersionTracker ============== *)
  let test_version_update_and_get () =
    Figma_cache.VersionTracker.update_version ~file_key:"vt-test-1" ~version:1.0;
    let v = Figma_cache.VersionTracker.get_version ~file_key:"vt-test-1" in
    check bool "has version" true (v = Some 1.0)
  in
  let test_version_get_missing () =
    let v = Figma_cache.VersionTracker.get_version ~file_key:"nonexistent-vt-key" in
    check bool "no version" true (v = None)
  in
  let test_version_check_valid () =
    Figma_cache.VersionTracker.update_version ~file_key:"vt-valid" ~version:5.0;
    let result = Figma_cache.VersionTracker.check_version ~file_key:"vt-valid" ~current_version:5.0 in
    check bool "valid" true (result = `Valid)
  in
  let test_version_check_invalidated () =
    Figma_cache.VersionTracker.update_version ~file_key:"vt-inv" ~version:3.0;
    let result = Figma_cache.VersionTracker.check_version ~file_key:"vt-inv" ~current_version:5.0 in
    check bool "invalidated" true (result = `Invalidated)
  in
  let test_version_check_new_file () =
    let result = Figma_cache.VersionTracker.check_version ~file_key:"vt-new-unique" ~current_version:1.0 in
    check bool "new file" true (result = `NewFile)
  in

  (* ============== stats (JSON output) ============== *)
  let test_stats_json () =
    Figma_cache.Stats.reset ();
    Hashtbl.reset Figma_cache.memory_cache;
    let json = Figma_cache.stats () in
    match json with
    | `Assoc fields ->
        check bool "has l1_entries" true (List.assoc_opt "l1_entries" fields <> None);
        check bool "has hit_rate_percent" true (List.assoc_opt "hit_rate_percent" fields <> None);
        check bool "has total_hits" true (List.assoc_opt "total_hits" fields <> None);
        check bool "has learned_patterns" true (List.assoc_opt "learned_patterns" fields <> None);
        check bool "has top_patterns" true (List.assoc_opt "top_patterns" fields <> None);
        check bool "has tracked_versions" true (List.assoc_opt "tracked_versions" fields <> None);
        check bool "has cache_dir" true (List.assoc_opt "cache_dir" fields <> None);
        check bool "has bytes_saved" true (List.assoc_opt "bytes_saved" fields <> None)
    | _ -> fail "expected assoc from stats"
  in

  (* ============== invalidate (full clear) ============== *)
  let test_invalidate_all () =
    Hashtbl.reset Figma_cache.memory_cache;
    let now = Unix.gettimeofday () in
    Hashtbl.replace Figma_cache.memory_cache "inv-key-1"
      { Figma_cache.payload = `Null; cached_at = now; last_access = now;
        file_key = "fk"; node_id = "n" };
    Figma_cache.invalidate ();
    check int "cleared" 0 (Hashtbl.length Figma_cache.memory_cache)
  in

  (* ============== invalidate (specific file_key) ============== *)
  let test_invalidate_specific_file () =
    Hashtbl.reset Figma_cache.memory_cache;
    let now = Unix.gettimeofday () in
    Hashtbl.replace Figma_cache.memory_cache "key-a"
      { Figma_cache.payload = `Null; cached_at = now; last_access = now;
        file_key = "target"; node_id = "n1" };
    Hashtbl.replace Figma_cache.memory_cache "key-b"
      { Figma_cache.payload = `Null; cached_at = now; last_access = now;
        file_key = "keep"; node_id = "n2" };
    Figma_cache.invalidate ~file_key:"target" ();
    check int "one remains" 1 (Hashtbl.length Figma_cache.memory_cache);
    check bool "kept entry" true (Hashtbl.mem Figma_cache.memory_cache "key-b")
  in

  (* ============== invalidate (specific file_key + node_id) ============== *)
  let test_invalidate_specific_node () =
    Hashtbl.reset Figma_cache.memory_cache;
    let now = Unix.gettimeofday () in
    Hashtbl.replace Figma_cache.memory_cache "key-c"
      { Figma_cache.payload = `Null; cached_at = now; last_access = now;
        file_key = "fk1"; node_id = "n1" };
    Hashtbl.replace Figma_cache.memory_cache "key-d"
      { Figma_cache.payload = `Null; cached_at = now; last_access = now;
        file_key = "fk1"; node_id = "n2" };
    Figma_cache.invalidate ~file_key:"fk1" ~node_id:"n1" ();
    check int "one remains" 1 (Hashtbl.length Figma_cache.memory_cache);
    check bool "kept n2" true (Hashtbl.mem Figma_cache.memory_cache "key-d")
  in

  (* ============== record_access ============== *)
  let test_record_access_wrapper () =
    (* Just exercise the wrapper function that delegates to PrefetchHints *)
    Figma_cache.record_access ~node_id:"rec-1";
    Figma_cache.record_access ~node_id:"rec-2";
    ()
  in

  (* ============== with_cache ============== *)
  let test_with_cache_miss_then_hit () =
    Hashtbl.reset Figma_cache.memory_cache;
    let call_count = ref 0 in
    let fetch () =
      incr call_count;
      `String "fetched"
    in
    (* First call: cache miss, fetches *)
    let r1 = Figma_cache.with_cache ~file_key:"wc-file" ~node_id:"wc-node" fetch in
    check bool "first call result" true (r1 = `String "fetched");
    check int "called once" 1 !call_count;
    (* Second call: cache hit from L1 *)
    let r2 = Figma_cache.with_cache ~file_key:"wc-file" ~node_id:"wc-node" fetch in
    check bool "second call result" true (r2 = `String "fetched");
    check int "still called once" 1 !call_count;
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* ============== set + get round-trip ============== *)
  let test_set_get_roundtrip () =
    Hashtbl.reset Figma_cache.memory_cache;
    Figma_cache.Stats.reset ();
    let payload = `Assoc [("key", `String "value")] in
    Figma_cache.set ~file_key:"rt-file" ~node_id:"rt-node" payload;
    let result = Figma_cache.get ~file_key:"rt-file" ~node_id:"rt-node" () in
    check bool "got value back" true (result = Some payload);
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* ============== get: L1 expired ============== *)
  let test_get_l1_expired () =
    Hashtbl.reset Figma_cache.memory_cache;
    Figma_cache.Stats.reset ();
    let key = Figma_cache.make_cache_key ~file_key:"exp-file" ~node_id:"exp-node" ~options:[] in
    let old_time = Unix.gettimeofday () -. (100.0 *. 3600.0) in
    Hashtbl.replace Figma_cache.memory_cache key
      { Figma_cache.payload = `String "old"; cached_at = old_time;
        last_access = old_time; file_key = "exp-file"; node_id = "exp-node" };
    let result = Figma_cache.get ~file_key:"exp-file" ~node_id:"exp-node" () in
    check bool "expired returns None" true (result = None);
    check bool "entry removed" false (Hashtbl.mem Figma_cache.memory_cache key);
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* ============== get with options ============== *)
  let test_get_with_options () =
    Hashtbl.reset Figma_cache.memory_cache;
    Figma_cache.Stats.reset ();
    let payload = `String "opt-value" in
    Figma_cache.set ~file_key:"opt-f" ~node_id:"opt-n" ~options:["depth:2";"geometry:paths"] payload;
    let result = Figma_cache.get ~file_key:"opt-f" ~node_id:"opt-n" ~options:["depth:2";"geometry:paths"] () in
    check bool "got with options" true (result = Some payload);
    (* Different options = different key = miss *)
    let result2 = Figma_cache.get ~file_key:"opt-f" ~node_id:"opt-n" ~options:["other"] () in
    check bool "different options miss" true (result2 = None);
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* ============== PrefetchHints: overflow recent_accesses to >max_recent ============== *)
  let test_prefetch_overflow_recent () =
    (* Record more than max_recent (10) accesses to trigger truncation *)
    for i = 1 to 15 do
      Figma_cache.PrefetchHints.record_access ~node_id:(Printf.sprintf "overflow-%d" i)
    done;
    (* Should not crash and recent should be truncated *)
    ()
  in

  (* ============== PrefetchHints: learn_pattern duplicate ============== *)
  let test_prefetch_learn_dup () =
    Figma_cache.PrefetchHints.learn_pattern ~from_node:"dup-src" ~to_node:"dup-dst";
    Figma_cache.PrefetchHints.learn_pattern ~from_node:"dup-src" ~to_node:"dup-dst";
    let hints = Figma_cache.PrefetchHints.get_hints ~node_id:"dup-src" in
    (* Should not duplicate *)
    let count = List.length (List.filter (fun h -> h = "dup-dst") hints) in
    check int "no dup" 1 count
  in

  (* ============== enforce_l1_limit with eviction ============== *)
  let test_enforce_l1_limit_eviction () =
    Hashtbl.reset Figma_cache.memory_cache;
    (* Fill way beyond max_l1_entries (default 256) *)
    let now = Unix.gettimeofday () in
    for i = 0 to 300 do
      let key = Printf.sprintf "evict-key-%d" i in
      Hashtbl.replace Figma_cache.memory_cache key
        { Figma_cache.payload = `Null;
          cached_at = now;
          last_access = now -. (float_of_int (300 - i));  (* older entries first *)
          file_key = "f"; node_id = Printf.sprintf "%d" i }
    done;
    let before = Hashtbl.length Figma_cache.memory_cache in
    Figma_cache.enforce_l1_limit ();
    let after = Hashtbl.length Figma_cache.memory_cache in
    check bool "evicted some" true (after < before);
    check bool "at or below limit" true (after <= Figma_cache.Config.max_l1_entries);
    Hashtbl.reset Figma_cache.memory_cache
  in

  run "Figma_cache Extra" [
    ("make_cache_key", [
      test_case "basic" `Quick test_cache_key_basic;
      test_case "deterministic" `Quick test_cache_key_deterministic;
      test_case "options sorted" `Quick test_cache_key_options_sorted;
      test_case "different inputs" `Quick test_cache_key_different_inputs;
      test_case "empty options" `Quick test_cache_key_empty_options;
    ]);
    ("cached_at_of_json", [
      test_case "float" `Quick test_cached_at_float;
      test_case "int" `Quick test_cached_at_int;
      test_case "missing" `Quick test_cached_at_missing;
      test_case "wrong type" `Quick test_cached_at_wrong_type;
    ]);
    ("Stats", [
      test_case "initial" `Quick test_stats_initial;
      test_case "hits and misses" `Quick test_stats_hits;
      test_case "l2 hits" `Quick test_stats_l2_hits;
      test_case "prefetch hits" `Quick test_stats_prefetch_hits;
      test_case "all misses" `Quick test_stats_all_misses;
      test_case "reset" `Quick test_stats_reset;
    ]);
    ("PrefetchHints", [
      test_case "record_access" `Quick test_prefetch_record_access;
      test_case "learn_pattern" `Quick test_prefetch_learn_pattern;
      test_case "get_hints empty" `Quick test_prefetch_get_hints_empty;
      test_case "top_patterns" `Quick test_prefetch_top_patterns;
      test_case "analyze_chain" `Quick test_prefetch_analyze_chain;
      test_case "overflow recent" `Quick test_prefetch_overflow_recent;
      test_case "learn duplicate" `Quick test_prefetch_learn_dup;
    ]);
    ("enforce_l1_limit", [
      test_case "no eviction" `Quick test_enforce_l1_limit_no_eviction;
      test_case "eviction" `Quick test_enforce_l1_limit_eviction;
    ]);
    ("should_invalidate_entry", [
      test_case "match all" `Quick test_invalidate_entry_match_all;
      test_case "match specific" `Quick test_invalidate_entry_match_specific;
      test_case "no match file" `Quick test_invalidate_entry_no_match_file;
      test_case "no match node" `Quick test_invalidate_entry_no_match_node;
    ]);
    ("should_invalidate_json", [
      test_case "match all" `Quick test_invalidate_json_match_all;
      test_case "match specific" `Quick test_invalidate_json_match_specific;
      test_case "no match node" `Quick test_invalidate_json_no_match_node;
      test_case "no file_key" `Quick test_invalidate_json_no_file_key;
      test_case "wrong file" `Quick test_invalidate_json_wrong_file;
      test_case "node not string" `Quick test_invalidate_json_node_not_string;
    ]);
    ("VersionTracker", [
      test_case "update and get" `Quick test_version_update_and_get;
      test_case "get missing" `Quick test_version_get_missing;
      test_case "check valid" `Quick test_version_check_valid;
      test_case "check invalidated" `Quick test_version_check_invalidated;
      test_case "check new file" `Quick test_version_check_new_file;
    ]);
    ("stats", [
      test_case "json output" `Quick test_stats_json;
    ]);
    ("invalidate", [
      test_case "all" `Quick test_invalidate_all;
      test_case "specific file" `Quick test_invalidate_specific_file;
      test_case "specific node" `Quick test_invalidate_specific_node;
    ]);
    ("record_access", [
      test_case "wrapper" `Quick test_record_access_wrapper;
    ]);
    ("with_cache", [
      test_case "miss then hit" `Quick test_with_cache_miss_then_hit;
    ]);
    ("set_get", [
      test_case "roundtrip" `Quick test_set_get_roundtrip;
      test_case "L1 expired" `Quick test_get_l1_expired;
      test_case "with options" `Quick test_get_with_options;
    ]);
  ]
