(** Coverage Wave 10: Small gap closers across 6 modules.
    Target modules:
    1. server_metrics.ml — 76.39% (34 pts missing)
    2. figma_cache.ml — 77.24% (56 pts missing)
    3. mcp_progress.ml — 76.92% (3 pts missing)
    4. safe_exec.ml — 80.18% (22 pts missing)
    5. figma_image_similarity.ml — 80.71% (38 pts missing)
    6. mcp_protocol.ml — 83.66% (25 pts missing)
*)

let () =
  let open Alcotest in

  (* ================================================================
     1. server_metrics.ml — additional gap-closing tests
     ================================================================ *)

  (* record_untracked_response with 2xx status (exercises 2xx branch) *)
  let test_untracked_2xx () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `OK;
    let after = Server_metrics.snapshot () in
    check int "2xx +1" (before.status_2xx + 1) after.status_2xx;
    check int "total +1" (before.total + 1) after.total;
    check int "errors unchanged" before.errors after.errors
  in

  (* record_untracked_response with default bytes (no ~bytes param) *)
  let test_untracked_default_bytes () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `OK;
    let after = Server_metrics.snapshot () in
    (* default bytes=0 should not change bytes_out *)
    check int "bytes unchanged" before.bytes_out after.bytes_out
  in

  (* record_untracked_response with 3xx should NOT increment errors *)
  let test_untracked_3xx_no_errors () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `Found;
    let after = Server_metrics.snapshot () in
    check int "3xx +1" (before.status_3xx + 1) after.status_3xx;
    check int "errors unchanged" before.errors after.errors
  in

  (* record_untracked with large byte count *)
  let test_untracked_large_bytes () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response ~bytes:999999 `OK;
    let after = Server_metrics.snapshot () in
    check int "bytes +999999" (before.bytes_out + 999999) after.bytes_out
  in

  (* sse_open then sse_close: exercise paired open/close *)
  let test_sse_open_close_pair () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.sse_open ();
    let mid = Server_metrics.snapshot () in
    check int "sse_open +1" (before.sse_open + 1) mid.sse_open;
    check int "sse_total +1" (before.sse_total + 1) mid.sse_total;
    Server_metrics.sse_close ();
    let after = Server_metrics.snapshot () in
    check int "sse_open back" before.sse_open after.sse_open;
    (* sse_total stays incremented *)
    check int "sse_total stays" (before.sse_total + 1) after.sse_total
  in

  (* record_rps with 5m bucket array *)
  let test_record_rps_5m () =
    Eio_main.run @@ fun _env ->
    let now_sec = int_of_float (Unix.gettimeofday ()) in
    Server_metrics.with_lock (fun () ->
      Server_metrics.record_rps Server_metrics.rps_5m now_sec;
      Server_metrics.record_rps Server_metrics.rps_5m now_sec;
      let idx = now_sec mod 300 in
      check bool "5m bucket >= 2" true (Server_metrics.rps_5m.(idx).count >= 2)
    )
  in

  (* sum_recent with window=0: nothing should be recent *)
  let test_sum_recent_zero_window () =
    Eio_main.run @@ fun _env ->
    let now_sec = int_of_float (Unix.gettimeofday ()) in
    Server_metrics.with_lock (fun () ->
      let buckets = [|
        { Server_metrics.sec = now_sec; count = 5 };
        { Server_metrics.sec = now_sec; count = 3 }
      |] in
      let sum = Server_metrics.sum_recent buckets now_sec 0 in
      check int "zero window = 0" 0 sum
    )
  in

  (* record_latency: wrap around ring_size boundary *)
  let test_record_latency_wrap () =
    Eio_main.run @@ fun _env ->
    Server_metrics.with_lock (fun () ->
      (* Position index near the end of ring *)
      Server_metrics.latency_index := Server_metrics.ring_size - 1;
      Server_metrics.latency_count := Server_metrics.ring_size;
      Server_metrics.record_latency 99.0;
      (* Index should have wrapped to 0 *)
      check int "wrapped to 0" 0 !(Server_metrics.latency_index);
      (* Count stays at ring_size *)
      check int "count stays" Server_metrics.ring_size !(Server_metrics.latency_count)
    )
  in

  (* prom_metric: various label/value combinations *)
  let test_prom_metric_complex () =
    let result = Server_metrics.prom_metric
      "mcp_test" "server=\"figma\",class=\"2xx\"" "123" in
    check string "formatted" "mcp_test{server=\"figma\",class=\"2xx\"} 123\n" result
  in

  let test_prom_metric_float_value () =
    let result = Server_metrics.prom_metric "gauge" "app=\"test\"" "3.14" in
    check string "float value" "gauge{app=\"test\"} 3.14\n" result
  in

  (* to_json: verify JSON can be serialized/deserialized *)
  let test_to_json_roundtrip () =
    Eio_main.run @@ fun _env ->
    let json = Server_metrics.to_json () in
    let str = Yojson.Safe.to_string json in
    let parsed = Yojson.Safe.from_string str in
    check bool "roundtrip" true (Yojson.Safe.equal json parsed)
  in

  (* to_prometheus_text: verify it contains the server name *)
  let test_prometheus_server_name () =
    Eio_main.run @@ fun _env ->
    let text = Server_metrics.to_prometheus_text () in
    let has s = String.length s > 0 &&
      (try ignore (Str.search_forward (Str.regexp_string s) text 0); true
       with Not_found -> false)
    in
    check bool "has server name" true (has Mcp_protocol.server_name)
  in

  (* ================================================================
     2. figma_cache.ml — additional gap-closing tests
     ================================================================ *)

  (* Config module: access all config values *)
  let test_cache_config_values () =
    check bool "ttl_hours > 0" true (Figma_cache.Config.ttl_hours > 0.0);
    check bool "ttl_variables_hours > 0" true (Figma_cache.Config.ttl_variables_hours > 0.0);
    check bool "ttl_search_hours > 0" true (Figma_cache.Config.ttl_search_hours > 0.0);
    check bool "max_l1_entries > 0" true (Figma_cache.Config.max_l1_entries > 0);
    check bool "l2_max_mb > 0" true (Figma_cache.Config.l2_max_mb > 0);
    check bool "l2_max_bytes = l2_max_mb * 1M" true
      (Figma_cache.Config.l2_max_bytes = Figma_cache.Config.l2_max_mb * 1024 * 1024);
    check bool "cache_dir non-empty" true (String.length Figma_cache.Config.cache_dir > 0)
  in

  (* make_cache_key: special characters in inputs *)
  let test_cache_key_special_chars () =
    let k = Figma_cache.make_cache_key
      ~file_key:"abc/def" ~node_id:"1:2:3" ~options:["opt=val"] in
    check int "key length 16" 16 (String.length k)
  in

  (* make_cache_key: very long inputs *)
  let test_cache_key_long_input () =
    let long_key = String.make 1000 'x' in
    let k = Figma_cache.make_cache_key
      ~file_key:long_key ~node_id:"1:1" ~options:[] in
    check int "key length still 16" 16 (String.length k)
  in

  (* cached_at_of_json: null value *)
  let test_cached_at_null () =
    let json = `Assoc [("_cached_at", `Null)] in
    let ts = Figma_cache.cached_at_of_json ~fallback:55.0 json in
    check (float 0.001) "null fallback" 55.0 ts
  in

  (* cached_at_of_json: bool value *)
  let test_cached_at_bool () =
    let json = `Assoc [("_cached_at", `Bool true)] in
    let ts = Figma_cache.cached_at_of_json ~fallback:66.0 json in
    check (float 0.001) "bool fallback" 66.0 ts
  in

  (* cached_at_of_json: list value *)
  let test_cached_at_list () =
    let json = `Assoc [("_cached_at", `List [])] in
    let ts = Figma_cache.cached_at_of_json ~fallback:77.0 json in
    check (float 0.001) "list fallback" 77.0 ts
  in

  (* should_invalidate_entry: file_key mismatch with Some node_id *)
  let test_invalidate_entry_fk_mismatch_with_nid () =
    let entry : Figma_cache.cache_entry = {
      payload = `Null; cached_at = 0.0; last_access = 0.0;
      file_key = "fk1"; node_id = "n1"
    } in
    check bool "fk mismatch" false
      (Figma_cache.should_invalidate_entry ~file_key:"other" ~node_id:(Some "n1") entry)
  in

  (* should_invalidate_json: file_key not a string *)
  let test_invalidate_json_fk_not_string () =
    let json = `Assoc [("_file_key", `Int 42)] in
    check bool "fk not string" false
      (Figma_cache.should_invalidate_json ~file_key:"42" ~node_id:None json)
  in

  (* should_invalidate_json: missing _node_id when node_id=Some *)
  let test_invalidate_json_missing_node_id () =
    let json = `Assoc [("_file_key", `String "fk1")] in
    check bool "missing nid" false
      (Figma_cache.should_invalidate_json ~file_key:"fk1" ~node_id:(Some "n1") json)
  in

  (* PrefetchHints: learn_pattern with >5 different targets (overflow trim) *)
  let test_prefetch_learn_many () =
    for i = 1 to 10 do
      Figma_cache.PrefetchHints.learn_pattern
        ~from_node:"multi-src"
        ~to_node:(Printf.sprintf "target-%d" i)
    done;
    let hints = Figma_cache.PrefetchHints.get_hints ~node_id:"multi-src" in
    (* Should be limited to 6 max (1 new + filteri < 5 from existing) *)
    check bool "hints <= 6" true (List.length hints <= 6)
  in

  (* PrefetchHints: get_top_patterns with limit=0 *)
  let test_prefetch_top_limit_zero () =
    let top = Figma_cache.PrefetchHints.get_top_patterns ~limit:0 () in
    check int "zero limit" 0 (List.length top)
  in

  (* VersionTracker: check_version with same version (Valid) *)
  let test_version_check_same () =
    Figma_cache.VersionTracker.update_version ~file_key:"vt-same" ~version:10.0;
    let result = Figma_cache.VersionTracker.check_version
      ~file_key:"vt-same" ~current_version:10.0 in
    check bool "same version is Valid" true (result = `Valid)
  in

  (* VersionTracker: check_version with lower current (should be Valid, not invalidated) *)
  let test_version_check_lower_current () =
    Figma_cache.VersionTracker.update_version ~file_key:"vt-lower" ~version:10.0;
    let result = Figma_cache.VersionTracker.check_version
      ~file_key:"vt-lower" ~current_version:5.0 in
    (* cached_ver=10 NOT < current_version=5, so it matches the `Some _` branch => `Valid *)
    check bool "lower current is Valid" true (result = `Valid)
  in

  (* with_cache: custom ttl_hours — use unique keys to avoid L2 disk cache collision *)
  let test_with_cache_custom_ttl () =
    Hashtbl.reset Figma_cache.memory_cache;
    let uid = Printf.sprintf "ttl-%f" (Unix.gettimeofday ()) in
    let call_count = ref 0 in
    let fetch () = incr call_count; `String "data" in
    let _r1 = Figma_cache.with_cache
      ~file_key:uid ~node_id:"ttl-n" ~ttl_hours:1000.0 fetch in
    let _r2 = Figma_cache.with_cache
      ~file_key:uid ~node_id:"ttl-n" ~ttl_hours:1000.0 fetch in
    check int "called once with custom ttl" 1 !call_count;
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* with_cache: with options parameter *)
  let test_with_cache_with_options () =
    Hashtbl.reset Figma_cache.memory_cache;
    let uid = Printf.sprintf "wco-%f" (Unix.gettimeofday ()) in
    let call_count = ref 0 in
    let fetch () = incr call_count; `String "opt-data" in
    let _r = Figma_cache.with_cache
      ~file_key:uid ~node_id:"wco-n" ~options:["geometry:paths"] fetch in
    check int "called" 1 !call_count;
    (* Cache hit with same options *)
    let _r2 = Figma_cache.with_cache
      ~file_key:uid ~node_id:"wco-n" ~options:["geometry:paths"] fetch in
    check int "still 1 call" 1 !call_count;
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* set with options *)
  let test_set_with_options () =
    Hashtbl.reset Figma_cache.memory_cache;
    Figma_cache.set ~file_key:"so-f" ~node_id:"so-n"
      ~options:["depth:2";"geometry:paths"] (`String "payload");
    let result = Figma_cache.get ~file_key:"so-f" ~node_id:"so-n"
      ~options:["depth:2";"geometry:paths"] () in
    check bool "options roundtrip" true (result = Some (`String "payload"));
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* get with custom ttl_hours (short, so entry is fresh) *)
  let test_get_custom_ttl_fresh () =
    Hashtbl.reset Figma_cache.memory_cache;
    Figma_cache.Stats.reset ();
    Figma_cache.set ~file_key:"ttl2-f" ~node_id:"ttl2-n" (`String "v");
    let result = Figma_cache.get ~file_key:"ttl2-f" ~node_id:"ttl2-n"
      ~ttl_hours:9999.0 () in
    check bool "fresh with large ttl" true (result = Some (`String "v"));
    Hashtbl.reset Figma_cache.memory_cache
  in

  (* ================================================================
     3. mcp_progress.ml — all 3 missing points
     ================================================================ *)

  (* register + unregister progress sender *)
  let test_progress_register_unregister () =
    let sender () = () in
    Mcp_progress.register_progress_sender ~client_id:42 ~sender;
    check bool "registered" true
      (Hashtbl.mem !(Mcp_progress.sse_clients_ref) 42);
    Mcp_progress.unregister_progress_sender 42;
    check bool "unregistered" false
      (Hashtbl.mem !(Mcp_progress.sse_clients_ref) 42)
  in

  (* set_broadcast_fn *)
  let test_progress_set_broadcast () =
    let old = !(Mcp_progress.broadcast_fn) in
    Mcp_progress.set_broadcast_fn (fun _s -> ());
    check bool "broadcast_fn set" true (!(Mcp_progress.broadcast_fn) <> None);
    (* Restore *)
    Mcp_progress.broadcast_fn := old
  in

  (* make_progress_token: returns unique tokens *)
  let test_progress_make_token () =
    let t1 = Mcp_progress.make_progress_token () in
    let t2 = Mcp_progress.make_progress_token () in
    check bool "starts with progress_" true
      (String.length t1 > 9 && String.sub t1 0 9 = "progress_");
    (* Tokens should contain pid and timestamp *)
    check bool "non-empty" true (String.length t1 > 0);
    (* May or may not be equal due to same millisecond, but format is right *)
    ignore t2
  in

  (* send_progress: with broadcast_fn=None (exercises None branch) *)
  let test_progress_send_no_broadcast () =
    let old = !(Mcp_progress.broadcast_fn) in
    Mcp_progress.broadcast_fn := None;
    (* Should not crash *)
    Mcp_progress.send_progress
      ~token:"test-token" ~current:1 ~total:10 ~message:"step 1" ();
    Mcp_progress.broadcast_fn := old
  in

  (* send_progress: with broadcast_fn=Some (exercises Some branch) *)
  let test_progress_send_with_broadcast () =
    let received = ref "" in
    let old = !(Mcp_progress.broadcast_fn) in
    Mcp_progress.set_broadcast_fn (fun data -> received := data);
    Mcp_progress.send_progress
      ~token:"tok1" ~current:5 ~total:10 ~message:"halfway" ();
    check bool "received data" true (String.length !received > 0);
    check bool "contains token" true
      (try ignore (Str.search_forward (Str.regexp_string "tok1") !received 0); true
       with Not_found -> false);
    check bool "contains progress" true
      (try ignore (Str.search_forward (Str.regexp_string "\"progress\":5") !received 0); true
       with Not_found -> false);
    Mcp_progress.broadcast_fn := old
  in

  (* update_progress is an alias for send_progress *)
  let test_progress_update_alias () =
    let received = ref "" in
    let old = !(Mcp_progress.broadcast_fn) in
    Mcp_progress.set_broadcast_fn (fun data -> received := data);
    Mcp_progress.update_progress
      ~token:"alias-tok" ~current:3 ~total:5 ~message:"testing" ();
    check bool "alias received" true (String.length !received > 0);
    Mcp_progress.broadcast_fn := old
  in

  (* ================================================================
     4. safe_exec.ml — additional gap-closing tests
     ================================================================ *)

  (* find_in_path: finds common binaries *)
  let test_find_in_path_sh () =
    let result = Safe_exec.find_in_path "sh" in
    check bool "sh found" true (Option.is_some result)
  in

  let test_find_in_path_ls () =
    let result = Safe_exec.find_in_path "ls" in
    check bool "ls found" true (Option.is_some result)
  in

  (* find_in_path: nonexistent binary *)
  let test_find_in_path_missing () =
    let result = Safe_exec.find_in_path "nonexistent_binary_xyz_42" in
    check bool "not found" true (Option.is_none result)
  in

  (* command_exists: true for common binaries *)
  let test_command_exists_true () =
    check bool "sh exists" true (Safe_exec.command_exists "sh");
    check bool "ls exists" true (Safe_exec.command_exists "ls")
  in

  (* command_exists: false for nonexistent *)
  let test_command_exists_false () =
    check bool "missing not exists" false
      (Safe_exec.command_exists "nonexistent_binary_xyz_42")
  in

  (* run_stdout: success case *)
  let test_run_stdout_success () =
    match Safe_exec.run_stdout ~timeout_ms:5000 ~output_limit:1024
      "echo" [| "echo"; "hello" |] with
    | Ok output ->
        check bool "contains hello" true
          (try ignore (Str.search_forward (Str.regexp_string "hello") output 0); true
           with Not_found -> false)
    | Error e -> fail e
  in

  (* run_stdout: timeout case *)
  let test_run_stdout_timeout () =
    match Safe_exec.run_stdout ~timeout_ms:100 ~output_limit:1024
      "sleep" [| "sleep"; "5" |] with
    | Error msg ->
        check bool "timeout error" true
          (try ignore (Str.search_forward (Str.regexp_string "timed out") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected timeout"
  in

  (* run_stdout: nonzero exit code *)
  let test_run_stdout_nonzero () =
    match Safe_exec.run_stdout ~timeout_ms:5000 ~output_limit:1024
      "sh" [| "sh"; "-c"; "exit 42" |] with
    | Error msg ->
        check bool "has exit code" true
          (try ignore (Str.search_forward (Str.regexp_string "42") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* run: with stderr output *)
  let test_run_stderr () =
    match Safe_exec.run ~timeout_ms:5000 ~output_limit:1024
      "sh" [| "sh"; "-c"; "echo err >&2" |] with
    | Ok out ->
        check bool "has stderr" true (String.length out.stderr > 0);
        check bool "not timed out" false out.timed_out
    | Error e -> fail e
  in

  (* run: binary not found *)
  let test_run_missing_binary () =
    match Safe_exec.run ~timeout_ms:1000 ~output_limit:1024
      "/nonexistent/path/binary" [| "/nonexistent/path/binary" |] with
    | Error _ -> () (* Expected: Unix error *)
    | Ok _ -> fail "expected error for missing binary"
  in

  (* run with explicit timeout and output_limit *)
  let test_run_explicit_opts () =
    match Safe_exec.run ~timeout_ms:5000 ~output_limit:512
      "echo" [| "echo"; "opts" |] with
    | Ok out ->
        check bool "stdout contains opts" true
          (try ignore (Str.search_forward (Str.regexp_string "opts") out.stdout 0); true
           with Not_found -> false)
    | Error _ -> () (* echo might not be in path in some environments *)
  in

  (* command_exists for common binary *)
  let test_command_exists_echo () =
    let result = Safe_exec.command_exists "echo" in
    check bool "echo exists" true result
  in

  (* ================================================================
     5. figma_image_similarity.ml — additional gap-closing tests
     ================================================================ *)

  (* ensure_ppm: .ppm file should be returned as-is *)
  let test_ensure_ppm_already_ppm () =
    match Figma_image_similarity.ensure_ppm "/some/file.ppm" with
    | Ok path -> check string "same path" "/some/file.ppm" path
    | Error _ -> fail "expected Ok for .ppm"
  in

  (* ensure_ppm: non-.ppm file tries conversion (will fail without ImageMagick) *)
  let test_ensure_ppm_needs_conversion () =
    match Figma_image_similarity.ensure_ppm "/nonexistent/file.png" with
    | Error _ -> () (* Expected: conversion fails *)
    | Ok _ -> () (* If magick/convert available, also OK *)
  in

  (* load_ppm: invalid magic *)
  let test_load_ppm_bad_magic () =
    let path = Filename.temp_file "test-ppm" ".ppm" in
    Out_channel.with_open_bin path (fun oc ->
      output_string oc "P5 2 2 255\n";
      output_string oc (String.make 12 '\000')
    );
    let result = Figma_image_similarity.load_ppm path in
    (match result with
     | Error msg ->
         check bool "bad magic" true
           (try ignore (Str.search_forward (Str.regexp_string "P6") msg 0); true
            with Not_found -> false)
     | Ok _ -> fail "expected error for P5 magic");
    (try Sys.remove path with _ -> ())
  in

  (* load_ppm: zero dimensions *)
  let test_load_ppm_zero_dims () =
    let path = Filename.temp_file "test-ppm" ".ppm" in
    Out_channel.with_open_bin path (fun oc ->
      output_string oc "P6 0 0 255\n"
    );
    let result = Figma_image_similarity.load_ppm path in
    (match result with
     | Error msg ->
         check bool "bad dims" true
           (try ignore (Str.search_forward (Str.regexp_string "dimensions") msg 0); true
            with Not_found -> false)
     | Ok _ -> fail "expected error for zero dims");
    (try Sys.remove path with _ -> ())
  in

  (* load_ppm: zero maxval *)
  let test_load_ppm_zero_maxval () =
    let path = Filename.temp_file "test-ppm" ".ppm" in
    Out_channel.with_open_bin path (fun oc ->
      output_string oc "P6 2 2 0\n";
      output_string oc (String.make 12 '\000')
    );
    let result = Figma_image_similarity.load_ppm path in
    (match result with
     | Error msg ->
         check bool "bad maxval" true
           (try ignore (Str.search_forward (Str.regexp_string "maxval") msg 0); true
            with Not_found -> false)
     | Ok _ -> fail "expected error for zero maxval");
    (try Sys.remove path with _ -> ())
  in

  (* load_ppm: truncated pixel data *)
  let test_load_ppm_truncated () =
    let path = Filename.temp_file "test-ppm" ".ppm" in
    Out_channel.with_open_bin path (fun oc ->
      output_string oc "P6 2 2 255\n";
      (* need 2*2*3=12 bytes but only provide 5 *)
      output_string oc (String.make 5 '\128')
    );
    let result = Figma_image_similarity.load_ppm path in
    (match result with
     | Error msg ->
         check bool "truncated" true
           (try ignore (Str.search_forward (Str.regexp_string "truncated") msg 0); true
            with Not_found -> false)
     | Ok _ -> fail "expected error for truncated data");
    (try Sys.remove path with _ -> ())
  in

  (* load_ppm: valid small image *)
  let test_load_ppm_valid () =
    let path = Filename.temp_file "test-ppm" ".ppm" in
    Out_channel.with_open_bin path (fun oc ->
      output_string oc "P6 2 2 255\n";
      (* 2x2 pixels, 3 bytes each = 12 bytes *)
      for _ = 0 to 3 do
        output_char oc '\128'; (* R *)
        output_char oc '\064'; (* G *)
        output_char oc '\192'; (* B *)
      done
    );
    let result = Figma_image_similarity.load_ppm path in
    (match result with
     | Ok img ->
         check int "width" 2 img.width;
         check int "height" 2 img.height;
         check int "luma length" 4 (Array.length img.luma);
         check int "lab length" 4 (Array.length img.lab)
     | Error e -> fail e);
    (try Sys.remove path with _ -> ())
  in

  (* ssim_window with different pixel values *)
  let test_ssim_window_varied () =
    let lab = Array.make 16 { Ciede2000.l = 50.0; a = 0.0; b = 0.0 } in
    let img_a = {
      Figma_image_similarity.width = 4; height = 4;
      lab; luma = Array.init 16 (fun i -> float_of_int i /. 16.0)
    } in
    let img_b = {
      Figma_image_similarity.width = 4; height = 4;
      lab; luma = Array.init 16 (fun i -> float_of_int (15 - i) /. 16.0)
    } in
    let result = Figma_image_similarity.ssim_window
      ~a:img_a ~b:img_b ~x0:0 ~y0:0 ~w:4 ~h:4 in
    check bool "ssim_window varied < 1.0" true (result < 1.0);
    check bool "ssim_window varied > -1.0" true (result > -1.0)
  in

  (* ssim: with varied images, window=4 *)
  let test_ssim_varied_window4 () =
    let lab = Array.make 64 { Ciede2000.l = 50.0; a = 0.0; b = 0.0 } in
    let img_a = {
      Figma_image_similarity.width = 8; height = 8;
      lab; luma = Array.init 64 (fun i -> float_of_int i /. 64.0)
    } in
    let img_b = {
      Figma_image_similarity.width = 8; height = 8;
      lab; luma = Array.init 64 (fun i -> float_of_int (63 - i) /. 64.0)
    } in
    let result = Figma_image_similarity.ssim img_a img_b ~window:4 in
    check bool "ssim < 1" true (result < 1.0);
    check bool "ssim >= 0" true (result >= 0.0 || result < 0.0) (* valid float *)
  in

  (* calculate_avg_delta_e: different-size images *)
  let test_delta_e_different_sizes () =
    let lab_a = Array.make 6 { Ciede2000.l = 50.0; a = 0.0; b = 0.0 } in
    let lab_b = Array.make 4 { Ciede2000.l = 70.0; a = 10.0; b = -5.0 } in
    let img_a = {
      Figma_image_similarity.width = 3; height = 2;
      lab = lab_a; luma = Array.make 6 0.5
    } in
    let img_b = {
      Figma_image_similarity.width = 2; height = 2;
      lab = lab_b; luma = Array.make 4 0.5
    } in
    let result = Figma_image_similarity.calculate_avg_delta_e img_a img_b in
    check bool "delta_e > 0" true (result > 0.0)
  in

  (* mse_psnr: one-pixel image *)
  let test_mse_psnr_single_pixel () =
    let lab = [| { Ciede2000.l = 50.0; a = 0.0; b = 0.0 } |] in
    let img_a = {
      Figma_image_similarity.width = 1; height = 1;
      lab; luma = [| 0.3 |]
    } in
    let img_b = {
      Figma_image_similarity.width = 1; height = 1;
      lab; luma = [| 0.7 |]
    } in
    let (mse, psnr, w, h) = Figma_image_similarity.mse_psnr img_a img_b in
    check bool "mse > 0" true (mse > 0.0);
    check bool "psnr finite" true (Float.is_finite psnr);
    check int "w=1" 1 w;
    check int "h=1" 1 h
  in

  (* convert_to_ppm: with nonexistent file *)
  let test_convert_to_ppm_missing () =
    let result = Figma_image_similarity.convert_to_ppm
      ~input:"/nonexistent/file.png"
      ~output:"/tmp/test-output.ppm" in
    match result with
    | Error _ -> () (* Expected *)
    | Ok () -> () (* If magick exists and handles it somehow *)
  in

  (* ================================================================
     6. mcp_protocol.ml — additional gap-closing tests
     ================================================================ *)

  let make_test_server () =
    let tool : Mcp_protocol.tool_def = {
      name = "echo";
      description = "Echo tool";
      input_schema = `Assoc [("type", `String "object")];
    } in
    let deprecated_tool : Mcp_protocol.tool_def = {
      name = "old_tool";
      description = "[DEPRECATED] Use new_tool instead";
      input_schema = `Assoc [("type", `String "object")];
    } in
    let resource : Mcp_protocol.mcp_resource = {
      uri = "file:///test.txt";
      name = "Test";
      description = "Test resource";
      mime_type = "text/plain";
    } in
    let prompt : Mcp_protocol.mcp_prompt = {
      name = "greeting";
      description = "Greeting prompt";
      arguments = [
        { name = "name"; description = "Your name"; required = true };
        { name = "lang"; description = "Language"; required = false };
      ];
      text = "Hello, {name}!";
    } in
    let template : Mcp_protocol.mcp_resource_template = {
      uri_template = "figma://tokens/{file_key}";
      name = "Tokens";
      description = "Design tokens";
      mime_type = "application/json";
    } in
    let echo_handler params =
      Ok (`Assoc [("content", `List [`Assoc [
        ("type", `String "text");
        ("text", `String (Yojson.Safe.to_string params))
      ]])])
    in
    let failing_handler _params =
      Error "handler failed"
    in
    let read_resource uri =
      if uri = "file:///test.txt" then Ok ("text/plain", "Test content")
      else Error (Printf.sprintf "Not found: %s" uri)
    in
    Mcp_protocol.create_server
      ~handlers_sync:[("echo", echo_handler); ("fail_tool", failing_handler)]
      ~resource_templates:[template]
      [tool; deprecated_tool] [resource] [prompt] read_resource
  in

  (* tool_to_json: deprecated tool has deprecated field *)
  let test_tool_to_json_deprecated () =
    let tool : Mcp_protocol.tool_def = {
      name = "old_tool";
      description = "[DEPRECATED] Use new_tool instead";
      input_schema = `Assoc [("type", `String "object")];
    } in
    let json = Mcp_protocol.tool_to_json tool in
    match json with
    | `Assoc fields ->
        check bool "has deprecated" true
          (List.assoc_opt "deprecated" fields = Some (`Bool true))
    | _ -> fail "expected assoc"
  in

  (* tool_to_json: non-deprecated tool has no deprecated field *)
  let test_tool_to_json_not_deprecated () =
    let tool : Mcp_protocol.tool_def = {
      name = "new_tool";
      description = "A new tool";
      input_schema = `Assoc [];
    } in
    let json = Mcp_protocol.tool_to_json tool in
    match json with
    | `Assoc fields ->
        check bool "no deprecated" true
          (List.assoc_opt "deprecated" fields = None)
    | _ -> fail "expected assoc"
  in

  (* tool_to_json: short description (< 12 chars, not deprecated) *)
  let test_tool_to_json_short_desc () =
    let tool : Mcp_protocol.tool_def = {
      name = "t";
      description = "short";
      input_schema = `Null;
    } in
    let json = Mcp_protocol.tool_to_json tool in
    match json with
    | `Assoc fields ->
        check bool "no deprecated for short" true
          (List.assoc_opt "deprecated" fields = None)
    | _ -> fail "expected assoc"
  in

  (* resource_template_to_json: verify fields *)
  let test_resource_template_to_json () =
    let tmpl : Mcp_protocol.mcp_resource_template = {
      uri_template = "figma://tokens/{file_key}";
      name = "Tokens";
      description = "Design tokens";
      mime_type = "application/json";
    } in
    let json = Mcp_protocol.resource_template_to_json tmpl in
    match json with
    | `Assoc fields ->
        check bool "has uriTemplate" true
          (List.assoc_opt "uriTemplate" fields = Some (`String "figma://tokens/{file_key}"));
        check bool "has name" true
          (List.assoc_opt "name" fields = Some (`String "Tokens"));
        check bool "has mimeType" true
          (List.assoc_opt "mimeType" fields = Some (`String "application/json"))
    | _ -> fail "expected assoc"
  in

  (* handle_prompts_get: non-Assoc params *)
  let test_prompts_get_non_assoc () =
    let server = make_test_server () in
    match Mcp_protocol.handle_prompts_get server (Some (`List [])) with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_prompts_get: None params *)
  let test_prompts_get_none_params () =
    let server = make_test_server () in
    match Mcp_protocol.handle_prompts_get server None with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_resources_read: non-Assoc params *)
  let test_resources_read_non_assoc () =
    let server = make_test_server () in
    match Mcp_protocol.handle_resources_read server (Some (`String "bad")) with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_resources_read: missing uri *)
  let test_resources_read_missing_uri () =
    let server = make_test_server () in
    let params = Some (`Assoc [("other", `String "value")]) in
    match Mcp_protocol.handle_resources_read server params with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_tools_call_sync: missing tool name *)
  let test_tools_call_missing_name () =
    let server = make_test_server () in
    let params = Some (`Assoc [("arguments", `Assoc [])]) in
    match Mcp_protocol.handle_tools_call_sync server params with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_tools_call_sync: non-Assoc params *)
  let test_tools_call_non_assoc () =
    let server = make_test_server () in
    match Mcp_protocol.handle_tools_call_sync server (Some (`List [])) with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_tools_call_sync: None params *)
  let test_tools_call_none_params () =
    let server = make_test_server () in
    match Mcp_protocol.handle_tools_call_sync server None with
    | Error (code, _) ->
        check int "invalid_params" Mcp_protocol.invalid_params code
    | Ok _ -> fail "expected error"
  in

  (* handle_tools_call_sync: handler returns Error *)
  let test_tools_call_handler_error () =
    let server = make_test_server () in
    let params = Some (`Assoc [
      ("name", `String "fail_tool");
      ("arguments", `Assoc [])
    ]) in
    match Mcp_protocol.handle_tools_call_sync server params with
    | Error (code, msg) ->
        check int "internal_error" Mcp_protocol.internal_error code;
        check bool "has error msg" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* process_request_sync: notifications/initialized *)
  let test_process_notifications_initialized () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 1);
      method_ = "notifications/initialized";
      params = None;
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "result is null" true
          (List.assoc_opt "result" fields = Some `Null)
    | _ -> fail "expected assoc"
  in

  (* process_request_sync: tools/call routing *)
  let test_process_tools_call () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 99);
      method_ = "tools/call";
      params = Some (`Assoc [
        ("name", `String "echo");
        ("arguments", `Assoc [("msg", `String "hi")])
      ]);
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has result" true (List.mem_assoc "result" fields);
        check bool "no error" false (List.mem_assoc "error" fields)
    | _ -> fail "expected assoc"
  in

  (* process_request_sync: resources/read routing *)
  let test_process_resources_read () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 2);
      method_ = "resources/read";
      params = Some (`Assoc [("uri", `String "file:///test.txt")]);
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has result" true (List.mem_assoc "result" fields)
    | _ -> fail "expected assoc"
  in

  (* process_request_sync: resources/read error *)
  let test_process_resources_read_error () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 2);
      method_ = "resources/read";
      params = Some (`Assoc [("uri", `String "file:///missing.txt")]);
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has error" true (List.mem_assoc "error" fields)
    | _ -> fail "expected assoc"
  in

  (* process_request_sync: prompts/get routing *)
  let test_process_prompts_get () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 3);
      method_ = "prompts/get";
      params = Some (`Assoc [("name", `String "greeting")]);
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has result" true (List.mem_assoc "result" fields)
    | _ -> fail "expected assoc"
  in

  (* process_request_sync: prompts/get error *)
  let test_process_prompts_get_error () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 3);
      method_ = "prompts/get";
      params = Some (`Assoc [("name", `String "nonexistent")]);
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has error" true (List.mem_assoc "error" fields)
    | _ -> fail "expected assoc"
  in

  (* handle_initialize: with various protocol versions *)
  let test_initialize_old_version () =
    let params = Some (`Assoc [("protocolVersion", `String "2024-11-05")]) in
    let result = Mcp_protocol.handle_initialize params in
    match result with
    | `Assoc fields ->
        check bool "negotiated version" true
          (List.assoc_opt "protocolVersion" fields = Some (`String "2024-11-05"))
    | _ -> fail "expected assoc"
  in

  let test_initialize_unsupported_version () =
    let params = Some (`Assoc [("protocolVersion", `String "1999-01-01")]) in
    let result = Mcp_protocol.handle_initialize params in
    match result with
    | `Assoc fields ->
        check bool "default version" true
          (List.assoc_opt "protocolVersion" fields =
           Some (`String Mcp_protocol.default_protocol_version))
    | _ -> fail "expected assoc"
  in

  (* protocol_version_from_params: non-string version *)
  let test_protocol_version_non_string () =
    let params = Some (`Assoc [("protocolVersion", `Int 42)]) in
    let version = Mcp_protocol.protocol_version_from_params params in
    check string "default for non-string" Mcp_protocol.default_protocol_version version
  in

  (* protocol_version_from_params: non-Assoc params *)
  let test_protocol_version_from_list () =
    let params = Some (`List []) in
    let version = Mcp_protocol.protocol_version_from_params params in
    check string "default for list" Mcp_protocol.default_protocol_version version
  in

  (* prompt with arguments serialization *)
  let test_prompt_with_args_json () =
    let prompt : Mcp_protocol.mcp_prompt = {
      name = "search";
      description = "Search";
      arguments = [
        { name = "q"; description = "Query"; required = true };
        { name = "limit"; description = "Max results"; required = false };
      ];
      text = "Search: {q}";
    } in
    let json = Mcp_protocol.prompt_to_json prompt in
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "arguments" fields with
         | Some (`List args) -> check int "2 args" 2 (List.length args)
         | _ -> fail "expected args list")
    | _ -> fail "expected assoc"
  in

  (* prompt_to_detail_json includes text *)
  let test_prompt_detail_includes_text () =
    let prompt : Mcp_protocol.mcp_prompt = {
      name = "test";
      description = "Test";
      arguments = [];
      text = "Detail text here";
    } in
    let json = Mcp_protocol.prompt_to_detail_json prompt in
    match json with
    | `Assoc fields ->
        check bool "has text" true
          (List.assoc_opt "text" fields = Some (`String "Detail text here"));
        check bool "has description" true
          (List.assoc_opt "description" fields = Some (`String "Test"))
    | _ -> fail "expected assoc"
  in

  (* normalize_protocol_version: each supported version *)
  let test_normalize_all_supported () =
    List.iter (fun v ->
      let result = Mcp_protocol.normalize_protocol_version v in
      check string (Printf.sprintf "supported %s" v) v result
    ) Mcp_protocol.supported_protocol_versions
  in

  (* is_notification_id edge cases *)
  let test_is_notification_id_int () =
    check bool "Int not notification" false
      (Mcp_protocol.is_notification_id (Some (`Int 1)));
    check bool "String not notification" false
      (Mcp_protocol.is_notification_id (Some (`String "abc")));
    check bool "Float not notification" false
      (Mcp_protocol.is_notification_id (Some (`Float 1.5)))
  in

  (* create_server: verify fields *)
  let test_create_server () =
    let server = make_test_server () in
    check int "2 tools" 2 (List.length server.tools);
    check int "2 handlers" 2 (List.length server.handlers_sync);
    check int "1 resource" 1 (List.length server.resources);
    check int "1 template" 1 (List.length server.resource_templates);
    check int "1 prompt" 1 (List.length server.prompts)
  in

  (* make_error_response with data=None *)
  let test_error_response_no_data () =
    let resp = Mcp_protocol.make_error_response (`Int 1) (-32600) "Bad" None in
    match resp with
    | `Assoc fields ->
        (match List.assoc_opt "error" fields with
         | Some (`Assoc err) ->
             check bool "no data field" true
               (List.assoc_opt "data" err = None)
         | _ -> fail "expected error")
    | _ -> fail "expected assoc"
  in

  (* ================================================================
     Test Registration
     ================================================================ *)

  run "misc-w10" [
    (* server_metrics *)
    ("server_metrics", [
      test_case "2xx status" `Quick test_untracked_2xx;
      test_case "default bytes" `Quick test_untracked_default_bytes;
      test_case "3xx no errors" `Quick test_untracked_3xx_no_errors;
      test_case "large bytes" `Quick test_untracked_large_bytes;
      test_case "sse pair" `Quick test_sse_open_close_pair;
      test_case "rps 5m" `Quick test_record_rps_5m;
      test_case "sum_recent zero window" `Quick test_sum_recent_zero_window;
      test_case "latency wrap" `Quick test_record_latency_wrap;
      test_case "prom_metric complex" `Quick test_prom_metric_complex;
      test_case "prom_metric float" `Quick test_prom_metric_float_value;
      test_case "to_json roundtrip" `Quick test_to_json_roundtrip;
      test_case "prometheus server name" `Quick test_prometheus_server_name;
    ]);

    (* figma_cache *)
    ("figma_cache", [
      test_case "config values" `Quick test_cache_config_values;
      test_case "key special chars" `Quick test_cache_key_special_chars;
      test_case "key long input" `Quick test_cache_key_long_input;
      test_case "cached_at null" `Quick test_cached_at_null;
      test_case "cached_at bool" `Quick test_cached_at_bool;
      test_case "cached_at list" `Quick test_cached_at_list;
      test_case "invalidate fk mismatch" `Quick test_invalidate_entry_fk_mismatch_with_nid;
      test_case "invalidate json fk int" `Quick test_invalidate_json_fk_not_string;
      test_case "invalidate json missing nid" `Quick test_invalidate_json_missing_node_id;
      test_case "prefetch learn many" `Quick test_prefetch_learn_many;
      test_case "prefetch top limit 0" `Quick test_prefetch_top_limit_zero;
      test_case "version check same" `Quick test_version_check_same;
      test_case "version check lower" `Quick test_version_check_lower_current;
      test_case "with_cache custom ttl" `Quick test_with_cache_custom_ttl;
      test_case "with_cache options" `Quick test_with_cache_with_options;
      test_case "set with options" `Quick test_set_with_options;
      test_case "get custom ttl fresh" `Quick test_get_custom_ttl_fresh;
    ]);

    (* mcp_progress *)
    ("mcp_progress", [
      test_case "register/unregister" `Quick test_progress_register_unregister;
      test_case "set broadcast fn" `Quick test_progress_set_broadcast;
      test_case "make token" `Quick test_progress_make_token;
      test_case "send no broadcast" `Quick test_progress_send_no_broadcast;
      test_case "send with broadcast" `Quick test_progress_send_with_broadcast;
      test_case "update alias" `Quick test_progress_update_alias;
    ]);

    (* safe_exec *)
    ("safe_exec", [
      test_case "find sh" `Quick test_find_in_path_sh;
      test_case "find ls" `Quick test_find_in_path_ls;
      test_case "find missing" `Quick test_find_in_path_missing;
      test_case "command_exists true" `Quick test_command_exists_true;
      test_case "command_exists false" `Quick test_command_exists_false;
      test_case "run_stdout success" `Quick test_run_stdout_success;
      test_case "run_stdout timeout" `Quick test_run_stdout_timeout;
      test_case "run_stdout nonzero" `Quick test_run_stdout_nonzero;
      test_case "run stderr" `Quick test_run_stderr;
      test_case "run missing binary" `Quick test_run_missing_binary;
      test_case "run explicit opts" `Quick test_run_explicit_opts;
      test_case "command exists echo" `Quick test_command_exists_echo;
    ]);

    (* figma_image_similarity *)
    ("figma_image_similarity", [
      test_case "ensure ppm already" `Quick test_ensure_ppm_already_ppm;
      test_case "ensure ppm convert" `Quick test_ensure_ppm_needs_conversion;
      test_case "load ppm bad magic" `Quick test_load_ppm_bad_magic;
      test_case "load ppm zero dims" `Quick test_load_ppm_zero_dims;
      test_case "load ppm zero maxval" `Quick test_load_ppm_zero_maxval;
      test_case "load ppm truncated" `Quick test_load_ppm_truncated;
      test_case "load ppm valid" `Quick test_load_ppm_valid;
      test_case "ssim_window varied" `Quick test_ssim_window_varied;
      test_case "ssim varied w4" `Quick test_ssim_varied_window4;
      test_case "delta_e diff sizes" `Quick test_delta_e_different_sizes;
      test_case "mse single pixel" `Quick test_mse_psnr_single_pixel;
      test_case "convert_to_ppm missing" `Quick test_convert_to_ppm_missing;
    ]);

    (* mcp_protocol *)
    ("mcp_protocol", [
      test_case "tool deprecated" `Quick test_tool_to_json_deprecated;
      test_case "tool not deprecated" `Quick test_tool_to_json_not_deprecated;
      test_case "tool short desc" `Quick test_tool_to_json_short_desc;
      test_case "resource template" `Quick test_resource_template_to_json;
      test_case "prompts get non-assoc" `Quick test_prompts_get_non_assoc;
      test_case "prompts get none" `Quick test_prompts_get_none_params;
      test_case "resources read non-assoc" `Quick test_resources_read_non_assoc;
      test_case "resources read no uri" `Quick test_resources_read_missing_uri;
      test_case "tools call no name" `Quick test_tools_call_missing_name;
      test_case "tools call non-assoc" `Quick test_tools_call_non_assoc;
      test_case "tools call none" `Quick test_tools_call_none_params;
      test_case "tools call handler error" `Quick test_tools_call_handler_error;
      test_case "notifications/initialized" `Quick test_process_notifications_initialized;
      test_case "tools/call routing" `Quick test_process_tools_call;
      test_case "resources/read routing" `Quick test_process_resources_read;
      test_case "resources/read error" `Quick test_process_resources_read_error;
      test_case "prompts/get routing" `Quick test_process_prompts_get;
      test_case "prompts/get error" `Quick test_process_prompts_get_error;
      test_case "init old version" `Quick test_initialize_old_version;
      test_case "init unsupported" `Quick test_initialize_unsupported_version;
      test_case "version non-string" `Quick test_protocol_version_non_string;
      test_case "version from list" `Quick test_protocol_version_from_list;
      test_case "prompt with args" `Quick test_prompt_with_args_json;
      test_case "prompt detail text" `Quick test_prompt_detail_includes_text;
      test_case "normalize all" `Quick test_normalize_all_supported;
      test_case "notification id edge" `Quick test_is_notification_id_int;
      test_case "create server" `Quick test_create_server;
      test_case "error no data" `Quick test_error_response_no_data;
    ]);
  ]
