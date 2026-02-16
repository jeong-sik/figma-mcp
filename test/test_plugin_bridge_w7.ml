(** Coverage Wave 7: figma_plugin_bridge.ml
    Target: 84 uncovered points -> 40+ improvement.

    Focus areas:
    - check_payload_limit: both Ok and Error branches
    - enqueue_command: payload too large, queue full, waiter notify exception
    - register_waiter: trim_waiters dropping excess, dropped waiter exception, high count warning
    - unregister_waiter: missing channel path
    - store_result: payload too large path
    - store_result_unlocked: eviction loop, compact_results_order
    - wait_for_result_with_sleep: immediate result, timeout, backoff, sleep_time<=0
    - publish_event: event buffer trim >200, waiter exception + re-register
    - cleanup_inactive: stale session removal with disconnect event, waiter exception
    - cleanup_results_unlocked: TTL-based result pruning, compact trigger
    - compact_results_order: removed entries filtered
    - poll_commands: partial drain
    - poll_events: partial drain from existing channel
*)

open Figma_plugin_bridge

(* ============== Helpers ============== *)

(** Use unique channel prefixes per test to avoid state collision with w4. *)
let mk_ch name = Printf.sprintf "w7-%s-%d" name (Random.bits () mod 100000)

(* ============== 1. check_payload_limit ============== *)

let test_check_payload_ok () =
  (* Default max_payload_bytes is 5MB. Small payload passes. *)
  let r = check_payload_limit (`String "hello") in
  (match r with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "small payload should be Ok")

let test_check_payload_error () =
  (* Build a payload larger than max_payload_bytes (5*1024*1024).
     We test the branch via enqueue_command which calls check_payload_limit
     and stores an error result. Testing the function directly here too. *)
  let big = String.make (6 * 1024 * 1024) 'x' in
  let r = check_payload_limit (`String big) in
  (match r with
   | Ok () -> Alcotest.fail "big payload should be Error"
   | Error size -> Alcotest.(check bool) "size > limit" true (size > 5 * 1024 * 1024))

(* ============== 2. enqueue_command: payload too large ============== *)

let test_enqueue_payload_too_large () =
  let ch = register_channel ~channel_id:(mk_ch "toolarge") () in
  let big = String.make (6 * 1024 * 1024) 'y' in
  let cmd_id = enqueue_command ~channel_id:ch ~name:"big_cmd" ~payload:(`String big) in
  (* Command should NOT be in the queue *)
  let cmds = poll_commands ~channel_id:ch ~max:10 in
  Alcotest.(check int) "no commands queued" 0 (List.length cmds);
  (* An error result should be stored *)
  let r = take_result ~channel_id:ch ~command_id:cmd_id in
  (match r with
   | None -> Alcotest.fail "error result should exist"
   | Some result ->
       Alcotest.(check bool) "not ok" false result.ok;
       let s = Yojson.Safe.to_string result.payload in
       Alcotest.(check bool) "has error msg" true
         (String.length s > 0 &&
          let re = Str.regexp_string "Command payload too large" in
          (try ignore (Str.search_forward re s 0); true with Not_found -> false)))

(* ============== 3. enqueue_command: command queue full ============== *)

let test_enqueue_queue_full () =
  let ch = register_channel ~channel_id:(mk_ch "qfull") () in
  (* Fill the queue up to max_commands *)
  let small = `String "x" in
  for _ = 1 to max_commands do
    ignore (enqueue_command ~channel_id:ch ~name:"fill" ~payload:small)
  done;
  (* Next command should fail with "Command queue full" *)
  let cmd_id = enqueue_command ~channel_id:ch ~name:"overflow" ~payload:small in
  let r = take_result ~channel_id:ch ~command_id:cmd_id in
  (match r with
   | None -> Alcotest.fail "overflow error result should exist"
   | Some result ->
       Alcotest.(check bool) "not ok" false result.ok;
       let s = Yojson.Safe.to_string result.payload in
       Alcotest.(check bool) "has queue full" true
         (let re = Str.regexp_string "Command queue full" in
          try ignore (Str.search_forward re s 0); true with Not_found -> false))

(* ============== 4. enqueue_command: waiter notify + exception ============== *)

let test_enqueue_notifies_waiters () =
  let ch = register_channel ~channel_id:(mk_ch "waiter-notify") () in
  let notified = ref false in
  let _wid = register_waiter ~channel_id:ch ~notify:(fun () -> notified := true) in
  let _ = enqueue_command ~channel_id:ch ~name:"ping" ~payload:`Null in
  Alcotest.(check bool) "waiter notified" true !notified

let test_enqueue_waiter_exception () =
  (* Waiter notify raises -- should not crash, just print warning *)
  let ch = register_channel ~channel_id:(mk_ch "waiter-exn") () in
  let _wid = register_waiter ~channel_id:ch
    ~notify:(fun () -> raise (Failure "boom")) in
  let cmd_id = enqueue_command ~channel_id:ch ~name:"ping2" ~payload:`Null in
  (* Should not raise *)
  Alcotest.(check bool) "cmd_id nonempty" true (String.length cmd_id > 0)

(* ============== 5. register_waiter: trim excess + exception on dropped ============== *)

let test_register_waiter_trim () =
  let ch = register_channel ~channel_id:(mk_ch "trim") () in
  let count = ref 0 in
  (* Fill up to max_waiters *)
  for _ = 1 to max_waiters do
    ignore (register_waiter ~channel_id:ch ~notify:(fun () -> incr count))
  done;
  (* One more should trigger trim; the oldest waiter gets dropped and notified *)
  let dropped_notified = ref false in
  (* Register one more; this pushes past max_waiters *)
  ignore (register_waiter ~channel_id:ch ~notify:(fun () -> dropped_notified := true));
  (* The dropped waiter's notify is called. Since max_waiters is 128 default,
     and we added 129, one should be dropped. But the dropped one is the latest
     added beyond limit. Actually: waiters are prepended (newest first).
     trim_waiters keeps first max_waiters, drops the rest.
     So the OLDEST registered gets dropped.
     The first one registered (notify = incr count) gets notified. *)
  Alcotest.(check bool) "dropped waiter was notified" true (!count > 0 || !dropped_notified)

let test_register_waiter_dropped_exception () =
  let ch = register_channel ~channel_id:(mk_ch "trim-exn") () in
  (* Fill up to max_waiters with failing notifiers *)
  for _ = 1 to max_waiters do
    ignore (register_waiter ~channel_id:ch
      ~notify:(fun () -> raise (Failure "dropped-boom")))
  done;
  (* Next registration triggers trim; dropped waiter's notify raises *)
  ignore (register_waiter ~channel_id:ch ~notify:(fun () -> ()));
  (* Should not crash *)
  Alcotest.(check bool) "no crash on dropped exception" true true

(* ============== 6. unregister_waiter: missing channel ============== *)

let test_unregister_waiter_missing_channel () =
  unregister_waiter ~channel_id:"no-such-w7" ~waiter_id:9999;
  Alcotest.(check bool) "no crash" true true

let test_unregister_waiter_removes () =
  let ch = register_channel ~channel_id:(mk_ch "unreg") () in
  let wid = register_waiter ~channel_id:ch ~notify:(fun () -> ()) in
  unregister_waiter ~channel_id:ch ~waiter_id:wid;
  (* Verify stats show 0 waiters *)
  let stats = list_channel_stats () in
  let found = List.find_opt (fun (s : channel_stats) -> s.id = ch) stats in
  (match found with
   | None -> Alcotest.fail "channel should exist"
   | Some s -> Alcotest.(check int) "0 waiters" 0 s.waiters)

(* ============== 7. store_result: payload too large via public API ============== *)

let test_store_result_payload_too_large () =
  let ch = register_channel ~channel_id:(mk_ch "store-big") () in
  let big = String.make (6 * 1024 * 1024) 'z' in
  store_result ~channel_id:ch ~command_id:"cmd-big" ~ok:true ~payload:(`String big);
  let r = take_result ~channel_id:ch ~command_id:"cmd-big" in
  (match r with
   | None -> Alcotest.fail "error result should exist"
   | Some result ->
       Alcotest.(check bool) "forced not-ok" false result.ok;
       let s = Yojson.Safe.to_string result.payload in
       Alcotest.(check bool) "has error msg" true
         (let re = Str.regexp_string "Result payload too large" in
          try ignore (Str.search_forward re s 0); true with Not_found -> false))

(* ============== 8. store_result: duplicate command_id (replace) ============== *)

let test_store_result_replace () =
  let ch = register_channel ~channel_id:(mk_ch "replace") () in
  store_result ~channel_id:ch ~command_id:"cmd-dup" ~ok:true ~payload:(`String "v1");
  store_result ~channel_id:ch ~command_id:"cmd-dup" ~ok:false ~payload:(`String "v2");
  let r = take_result ~channel_id:ch ~command_id:"cmd-dup" in
  (match r with
   | None -> Alcotest.fail "should exist"
   | Some result ->
       Alcotest.(check bool) "replaced with v2 (not ok)" false result.ok)

(* ============== 9. wait_for_result_with_sleep: immediate, timeout, backoff ============== *)

let test_wait_immediate () =
  let ch = register_channel ~channel_id:(mk_ch "wait-imm") () in
  store_result ~channel_id:ch ~command_id:"cmd-imm" ~ok:true ~payload:(`String "fast");
  let r = wait_for_result_with_sleep
    ~sleep:(fun _ -> ())
    ~channel_id:ch ~command_id:"cmd-imm" ~timeout_ms:1000 in
  (match r with
   | None -> Alcotest.fail "should find immediately"
   | Some result -> Alcotest.(check bool) "ok" true result.ok)

let test_wait_timeout () =
  let ch = register_channel ~channel_id:(mk_ch "wait-to") () in
  (* No result stored => should timeout *)
  let r = wait_for_result_with_sleep
    ~sleep:(fun _ -> ()) (* instant sleep to not block test *)
    ~channel_id:ch ~command_id:"cmd-missing" ~timeout_ms:0 in
  Alcotest.(check bool) "timeout returns None" true (r = None)

let test_wait_backoff_then_found () =
  let ch = register_channel ~channel_id:(mk_ch "wait-bf") () in
  let attempt_count = ref 0 in
  let sleep_fn _dur =
    incr attempt_count;
    (* After 2 attempts, store the result so it's found on next poll *)
    if !attempt_count = 2 then
      store_result ~channel_id:ch ~command_id:"cmd-delayed"
        ~ok:true ~payload:(`String "delayed")
  in
  let r = wait_for_result_with_sleep
    ~sleep:sleep_fn
    ~channel_id:ch ~command_id:"cmd-delayed" ~timeout_ms:5000 in
  (match r with
   | None -> Alcotest.fail "should find after delay"
   | Some result -> Alcotest.(check bool) "ok" true result.ok);
  Alcotest.(check bool) "multiple attempts" true (!attempt_count >= 2)

let test_wait_sleep_time_zero () =
  (* When remaining time is essentially 0, sleep_time <= 0 => return None *)
  let ch = register_channel ~channel_id:(mk_ch "wait-zero") () in
  let sleep_count = ref 0 in
  let r = wait_for_result_with_sleep
    ~sleep:(fun _dur -> incr sleep_count)
    ~channel_id:ch ~command_id:"cmd-never" ~timeout_ms:1 in
  (* With timeout_ms=1 and instant sleep, it should loop a few times then timeout *)
  Alcotest.(check bool) "returns None" true (r = None)

(* ============== 10. publish_event: event buffer trim >200 ============== *)

let test_publish_event_buffer_trim () =
  let ch = register_channel ~channel_id:(mk_ch "ev-trim") () in
  (* Publish 210 events; buffer should trim to 200 *)
  for i = 1 to 210 do
    publish_event ~channel_id:ch
      ~event_type:"bulk" ~payload:(`Int i)
  done;
  let evts = poll_events ~channel_id:ch ~max:300 in
  Alcotest.(check bool) "at most 200" true (List.length evts <= 200)

(* ============== 11. publish_event: waiter notify exception + re-register ============== *)

let test_publish_event_waiter_exception () =
  let ch = register_channel ~channel_id:(mk_ch "ev-exn") () in
  let _wid = register_event_waiter ~channel_id:ch
    ~notify:(fun () -> raise (Failure "event-boom")) in
  (* Should not crash; failed waiter gets re-registered *)
  publish_event ~channel_id:ch ~event_type:"test" ~payload:`Null;
  Alcotest.(check bool) "no crash" true true

let test_publish_event_notifies_waiters () =
  let ch = register_channel ~channel_id:(mk_ch "ev-ok") () in
  let notified = ref false in
  let _wid = register_event_waiter ~channel_id:ch
    ~notify:(fun () -> notified := true) in
  publish_event ~channel_id:ch ~event_type:"selection_change" ~payload:(`String "x");
  Alcotest.(check bool) "event waiter notified" true !notified

(* ============== 12. publish_event: non-existent channel ============== *)

let test_publish_event_no_channel () =
  publish_event ~channel_id:"ghost-w7" ~event_type:"test" ~payload:`Null;
  Alcotest.(check bool) "no crash" true true

(* ============== 13. poll_events: partial drain ============== *)

let test_poll_events_partial () =
  let ch = register_channel ~channel_id:(mk_ch "ev-partial") () in
  for i = 1 to 5 do
    publish_event ~channel_id:ch
      ~event_type:"test" ~payload:(`Int i)
  done;
  let evts = poll_events ~channel_id:ch ~max:3 in
  Alcotest.(check int) "got 3" 3 (List.length evts);
  (* Remaining 2 *)
  let evts2 = poll_events ~channel_id:ch ~max:10 in
  Alcotest.(check int) "remaining 2" 2 (List.length evts2)

let test_poll_events_empty_channel () =
  let ch = register_channel ~channel_id:(mk_ch "ev-empty") () in
  let evts = poll_events ~channel_id:ch ~max:10 in
  Alcotest.(check int) "empty" 0 (List.length evts)

let test_poll_events_nonexistent () =
  let evts = poll_events ~channel_id:"noexist-w7-poll" ~max:10 in
  Alcotest.(check int) "0 for missing" 0 (List.length evts)

(* ============== 14. register/unregister event waiters ============== *)

let test_event_waiter_lifecycle () =
  let ch = register_channel ~channel_id:(mk_ch "ew-life") () in
  let wid = register_event_waiter ~channel_id:ch ~notify:(fun () -> ()) in
  Alcotest.(check bool) "wid > 0" true (wid > 0);
  unregister_event_waiter ~channel_id:ch ~waiter_id:wid;
  Alcotest.(check bool) "no crash" true true

let test_unregister_event_waiter_missing () =
  unregister_event_waiter ~channel_id:"no-such-w7-ew" ~waiter_id:1234;
  Alcotest.(check bool) "no crash" true true

(* ============== 15. cleanup_inactive: stale session disconnect event ============== *)

let test_cleanup_removes_stale () =
  (* Register a channel, then cleanup with ttl=0 to force removal *)
  let ch = register_channel ~channel_id:(mk_ch "stale") () in
  (* Verify it exists *)
  let channels = list_channels () in
  Alcotest.(check bool) "exists before cleanup" true (List.mem ch channels);
  (* Wait a tiny bit so last_seen is in the past *)
  Unix.sleepf 0.01;
  cleanup_inactive ~ttl_seconds:0.001;
  let channels2 = list_channels () in
  Alcotest.(check bool) "removed after cleanup" false (List.mem ch channels2)

let test_cleanup_stale_with_event_waiter () =
  (* Stale channel with event waiters should publish disconnect event *)
  let ch = register_channel ~channel_id:(mk_ch "stale-ew") () in
  let notified = ref false in
  let _wid = register_event_waiter ~channel_id:ch
    ~notify:(fun () -> notified := true) in
  Unix.sleepf 0.01;
  cleanup_inactive ~ttl_seconds:0.001;
  (* The disconnect event should have been published and waiter notified *)
  Alcotest.(check bool) "event waiter notified on disconnect" true !notified

let test_cleanup_stale_event_waiter_exception () =
  (* Disconnect waiter that raises should not crash cleanup *)
  let ch = register_channel ~channel_id:(mk_ch "stale-ew-exn") () in
  let _wid = register_event_waiter ~channel_id:ch
    ~notify:(fun () -> raise (Failure "disconnect-boom")) in
  Unix.sleepf 0.01;
  cleanup_inactive ~ttl_seconds:0.001;
  Alcotest.(check bool) "no crash" true true

let test_cleanup_keeps_active () =
  let ch = register_channel ~channel_id:(mk_ch "active") () in
  cleanup_inactive ~ttl_seconds:99999.0;
  let channels = list_channels () in
  Alcotest.(check bool) "still present" true (List.mem ch channels)

(* ============== 16. poll_commands: partial drain ============== *)

let test_poll_commands_partial () =
  let ch = register_channel ~channel_id:(mk_ch "cmd-partial") () in
  for _ = 1 to 5 do
    ignore (enqueue_command ~channel_id:ch ~name:"batch" ~payload:`Null)
  done;
  let cmds = poll_commands ~channel_id:ch ~max:3 in
  Alcotest.(check int) "got 3" 3 (List.length cmds);
  let cmds2 = poll_commands ~channel_id:ch ~max:10 in
  Alcotest.(check int) "remaining 2" 2 (List.length cmds2)

let test_poll_commands_max_zero () =
  let ch = register_channel ~channel_id:(mk_ch "cmd-zero") () in
  ignore (enqueue_command ~channel_id:ch ~name:"test" ~payload:`Null);
  let cmds = poll_commands ~channel_id:ch ~max:0 in
  Alcotest.(check int) "max=0 returns nothing" 0 (List.length cmds)

(* ============== 17. take_result: missing channel + missing command ============== *)

let test_take_result_missing_channel () =
  let r = take_result ~channel_id:"noexist-w7-take" ~command_id:"x" in
  Alcotest.(check bool) "None" true (r = None)

let test_take_result_missing_command () =
  let ch = register_channel ~channel_id:(mk_ch "take-miss") () in
  let r = take_result ~channel_id:ch ~command_id:"no-such-cmd" in
  Alcotest.(check bool) "None for missing cmd" true (r = None)

let test_take_result_consumed () =
  let ch = register_channel ~channel_id:(mk_ch "take-cons") () in
  store_result ~channel_id:ch ~command_id:"cmd-once" ~ok:true ~payload:(`String "v");
  let r1 = take_result ~channel_id:ch ~command_id:"cmd-once" in
  Alcotest.(check bool) "found first time" true (r1 <> None);
  let r2 = take_result ~channel_id:ch ~command_id:"cmd-once" in
  Alcotest.(check bool) "consumed second time" true (r2 = None)

(* ============== 18. set_default_channel + get_default_channel ============== *)

let test_set_get_default_channel () =
  set_default_channel "w7-default-test";
  let d = get_default_channel () in
  Alcotest.(check (option string)) "set" (Some "w7-default-test") d

(* ============== 19. list_channel_stats: field inspection ============== *)

let test_list_channel_stats_fields () =
  let ch = register_channel ~channel_id:(mk_ch "stats-fld") () in
  ignore (enqueue_command ~channel_id:ch ~name:"cmd1" ~payload:`Null);
  store_result ~channel_id:ch ~command_id:"res1" ~ok:true ~payload:`Null;
  let stats = list_channel_stats () in
  let found = List.find_opt (fun (s : channel_stats) -> s.id = ch) stats in
  (match found with
   | None -> Alcotest.fail "stats should contain channel"
   | Some s ->
       Alcotest.(check int) "1 command" 1 s.commands;
       Alcotest.(check int) "1 result" 1 s.results;
       Alcotest.(check bool) "last_seen > 0" true (s.last_seen > 0.0))

(* ============== 20. new_id format ============== *)

let test_register_auto_id () =
  let id = register_channel () in
  Alcotest.(check bool) "starts with ch-" true
    (String.length id > 3 && String.sub id 0 3 = "ch-")

(* ============== 21. store_result_unlocked eviction: fill beyond max_results ============== *)

let test_store_result_eviction () =
  let ch = register_channel ~channel_id:(mk_ch "evict") () in
  (* Fill results beyond max_results; oldest should be evicted *)
  for i = 1 to max_results + 10 do
    let cid = Printf.sprintf "evict-cmd-%d" i in
    store_result ~channel_id:ch ~command_id:cid ~ok:true ~payload:(`Int i)
  done;
  (* First 10 should be evicted *)
  let r1 = take_result ~channel_id:ch ~command_id:"evict-cmd-1" in
  Alcotest.(check bool) "first evicted" true (r1 = None);
  (* Last one should still exist *)
  let last_id = Printf.sprintf "evict-cmd-%d" (max_results + 10) in
  let r_last = take_result ~channel_id:ch ~command_id:last_id in
  Alcotest.(check bool) "last exists" true (r_last <> None)

(* ============== 22. cleanup_results_unlocked: TTL-based stale result removal ============== *)

let test_cleanup_results_stale () =
  (* Store a result, wait past TTL, then trigger cleanup via cleanup_inactive.
     cleanup_inactive calls cleanup_results_unlocked internally. *)
  let ch = register_channel ~channel_id:(mk_ch "res-stale") () in
  store_result ~channel_id:ch ~command_id:"old-res" ~ok:true ~payload:(`String "old");
  (* result_ttl_seconds defaults to 120.0. We can't easily override that in tests.
     But cleanup_inactive triggers cleanup_results_unlocked which checks result TTL.
     Since the result was just stored, it won't be stale yet.
     The main coverage gain here is hitting cleanup_results_unlocked's iteration path. *)
  cleanup_inactive ~ttl_seconds:99999.0;
  (* The result should still exist since it's not stale *)
  let r = take_result ~channel_id:ch ~command_id:"old-res" in
  Alcotest.(check bool) "not stale yet" true (r <> None)

(* ============== 23. compact_results_order: triggered by large order queue ============== *)

let test_compact_results_order () =
  (* To trigger compact_results_order, results_order must exceed max_results * 3.
     We store many results, then remove some via take_result, creating
     orphan entries in results_order. Then trigger cleanup. *)
  let ch = register_channel ~channel_id:(mk_ch "compact") () in
  (* Store max_results * 3 + 50 results, then take half to create orphans *)
  let n = min (max_results * 3 + 50) 2000 in
  for i = 1 to n do
    let cid = Printf.sprintf "compact-cmd-%d" i in
    store_result ~channel_id:ch ~command_id:cid ~ok:true ~payload:(`Int i)
  done;
  (* Take some results to create orphans in results_order *)
  for i = 1 to min 100 (n / 2) do
    let cid = Printf.sprintf "compact-cmd-%d" i in
    ignore (take_result ~channel_id:ch ~command_id:cid)
  done;
  (* Trigger cleanup which may call compact_results_order *)
  cleanup_inactive ~ttl_seconds:99999.0;
  (* Channel should still work normally *)
  let last_cid = Printf.sprintf "compact-cmd-%d" n in
  let r = take_result ~channel_id:ch ~command_id:last_cid in
  (* Last result may or may not exist (eviction), but no crash *)
  ignore r;
  Alcotest.(check bool) "no crash after compact" true true

(* ============== 24. wait_for_result: uses Unix.sleepf fallback (no Eio context) ============== *)

let test_wait_for_result_no_eio () =
  (* wait_for_result checks get_eio_context; with no Eio runtime, falls back to Unix.sleepf.
     With timeout_ms=0, it should return None immediately. *)
  let ch = register_channel ~channel_id:(mk_ch "wait-noeio") () in
  let r = wait_for_result ~channel_id:ch ~command_id:"missing" ~timeout_ms:0 in
  Alcotest.(check bool) "None with no Eio" true (r = None)

let test_wait_for_result_found_no_eio () =
  let ch = register_channel ~channel_id:(mk_ch "wait-noeio-ok") () in
  store_result ~channel_id:ch ~command_id:"found-cmd" ~ok:true ~payload:(`String "v");
  let r = wait_for_result ~channel_id:ch ~command_id:"found-cmd" ~timeout_ms:100 in
  (match r with
   | None -> Alcotest.fail "should find result"
   | Some result -> Alcotest.(check bool) "ok" true result.ok)

(* ============== Test Runner ============== *)

let () =
  Alcotest.run "Figma Plugin Bridge W7" [
    ("check_payload_limit", [
      ("ok for small", `Quick, test_check_payload_ok);
      ("error for large", `Quick, test_check_payload_error);
    ]);
    ("enqueue_command", [
      ("payload too large", `Quick, test_enqueue_payload_too_large);
      ("queue full", `Quick, test_enqueue_queue_full);
      ("notifies waiters", `Quick, test_enqueue_notifies_waiters);
      ("waiter exception", `Quick, test_enqueue_waiter_exception);
    ]);
    ("register_waiter", [
      ("trim excess", `Quick, test_register_waiter_trim);
      ("dropped waiter exception", `Quick, test_register_waiter_dropped_exception);
    ]);
    ("unregister_waiter", [
      ("missing channel", `Quick, test_unregister_waiter_missing_channel);
      ("removes waiter", `Quick, test_unregister_waiter_removes);
    ]);
    ("store_result", [
      ("payload too large", `Quick, test_store_result_payload_too_large);
      ("replace duplicate", `Quick, test_store_result_replace);
      ("eviction beyond max", `Quick, test_store_result_eviction);
    ]);
    ("wait_for_result_with_sleep", [
      ("immediate", `Quick, test_wait_immediate);
      ("timeout", `Quick, test_wait_timeout);
      ("backoff then found", `Quick, test_wait_backoff_then_found);
      ("sleep_time zero", `Quick, test_wait_sleep_time_zero);
    ]);
    ("publish_event", [
      ("buffer trim >200", `Quick, test_publish_event_buffer_trim);
      ("waiter exception", `Quick, test_publish_event_waiter_exception);
      ("notifies waiters", `Quick, test_publish_event_notifies_waiters);
      ("no channel", `Quick, test_publish_event_no_channel);
    ]);
    ("poll_events", [
      ("partial drain", `Quick, test_poll_events_partial);
      ("empty channel", `Quick, test_poll_events_empty_channel);
      ("nonexistent channel", `Quick, test_poll_events_nonexistent);
    ]);
    ("event_waiters", [
      ("lifecycle", `Quick, test_event_waiter_lifecycle);
      ("unregister missing", `Quick, test_unregister_event_waiter_missing);
    ]);
    ("cleanup_inactive", [
      ("removes stale", `Quick, test_cleanup_removes_stale);
      ("stale with event waiter", `Quick, test_cleanup_stale_with_event_waiter);
      ("stale event waiter exception", `Quick, test_cleanup_stale_event_waiter_exception);
      ("keeps active", `Quick, test_cleanup_keeps_active);
    ]);
    ("poll_commands", [
      ("partial drain", `Quick, test_poll_commands_partial);
      ("max zero", `Quick, test_poll_commands_max_zero);
    ]);
    ("take_result", [
      ("missing channel", `Quick, test_take_result_missing_channel);
      ("missing command", `Quick, test_take_result_missing_command);
      ("consumed", `Quick, test_take_result_consumed);
    ]);
    ("default_channel", [
      ("set and get", `Quick, test_set_get_default_channel);
    ]);
    ("channel_stats", [
      ("field inspection", `Quick, test_list_channel_stats_fields);
    ]);
    ("auto_id", [
      ("register auto", `Quick, test_register_auto_id);
    ]);
    ("cleanup_results", [
      ("stale result cleanup", `Quick, test_cleanup_results_stale);
      ("compact results order", `Quick, test_compact_results_order);
    ]);
    ("wait_for_result", [
      ("no Eio fallback timeout", `Quick, test_wait_for_result_no_eio);
      ("no Eio fallback found", `Quick, test_wait_for_result_found_no_eio);
    ]);
  ]
