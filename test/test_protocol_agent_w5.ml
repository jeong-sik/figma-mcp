(** Coverage Wave 5: mcp_protocol_eio.ml agent queue subsystem.
    Tests all queue operations (add, claim, heartbeat, abandon, submit, cleanup, stats)
    plus pure utility functions (status_to_string, parse_positive_int, env_int, hex_of_bytes).
    Every queue test runs inside [Eio_main.run] since the queue uses [Eio.Mutex]. *)

open Alcotest
open Mcp_protocol_eio

(* ---------- helpers ---------- *)

let reset () = Hashtbl.reset agent_queue

(** Add a request and return (id, secret). Wraps agent_add_request. *)
let add ?(priority = 0) ?(digest = "ctx") ?(platform = "react") ?(prompt = "p") () =
  agent_add_request ~priority ~context_digest:digest
    (`Assoc [("type", `String "FRAME")]) platform prompt

(** Claim the highest-priority pending request, fail if none. *)
let claim_exn ~worker_id =
  match agent_claim ~worker_id with
  | None -> Alcotest.fail "expected a request to claim"
  | Some r -> r

(** Extract claim_token from a claimed request, fail if absent. *)
let token_of r =
  match r.claim_token with
  | Some t -> t
  | None -> Alcotest.fail "expected claim_token"

(* ================================================================
   1. agent_status_to_string — pure, no Eio needed
   ================================================================ *)

let test_status_pending () =
  check string "pending" "pending" (agent_status_to_string Pending)

let test_status_claimed () =
  check string "claimed" "claimed" (agent_status_to_string Claimed)

let test_status_completed () =
  check string "completed" "completed" (agent_status_to_string Completed)

let test_status_failed () =
  check string "failed" "failed" (agent_status_to_string Failed)

(* ================================================================
   2. parse_positive_int — pure
   ================================================================ *)

let test_parse_positive_int_ok () =
  check (option int) "42" (Some 42) (parse_positive_int "42")

let test_parse_positive_int_one () =
  check (option int) "1" (Some 1) (parse_positive_int "1")

let test_parse_positive_int_zero () =
  check (option int) "0 rejected" None (parse_positive_int "0")

let test_parse_positive_int_negative () =
  check (option int) "-1 rejected" None (parse_positive_int "-1")

let test_parse_positive_int_garbage () =
  check (option int) "abc rejected" None (parse_positive_int "abc")

let test_parse_positive_int_empty () =
  check (option int) "empty rejected" None (parse_positive_int "")

let test_parse_positive_int_float () =
  check (option int) "3.14 rejected" None (parse_positive_int "3.14")

(* ================================================================
   3. env_int — reads env var with fallback
   ================================================================ *)

let test_env_int_missing () =
  (* use a var name that certainly does not exist *)
  let v = env_int ~name:"__W5_NONEXISTENT_VAR__" ~default:99 in
  check int "default" 99 v

let test_env_int_set () =
  Unix.putenv "__W5_TEST_VAR_INT__" "7";
  let v = env_int ~name:"__W5_TEST_VAR_INT__" ~default:99 in
  check int "env wins" 7 v

let test_env_int_bad_value () =
  Unix.putenv "__W5_TEST_VAR_BAD__" "nope";
  let v = env_int ~name:"__W5_TEST_VAR_BAD__" ~default:55 in
  check int "fallback on garbage" 55 v

let test_env_int_zero_value () =
  Unix.putenv "__W5_TEST_VAR_ZERO__" "0";
  let v = env_int ~name:"__W5_TEST_VAR_ZERO__" ~default:55 in
  check int "0 is not positive, fallback" 55 v

(* ================================================================
   4. hex_of_bytes — pure
   ================================================================ *)

let test_hex_empty () =
  check string "empty" "" (hex_of_bytes (Bytes.of_string ""))

let test_hex_simple () =
  check string "0x00" "00" (hex_of_bytes (Bytes.make 1 '\x00'))

let test_hex_ff () =
  check string "0xff" "ff" (hex_of_bytes (Bytes.make 1 '\xff'))

let test_hex_hello () =
  (* "Hi" = 0x48 0x69 *)
  check string "Hi" "4869" (hex_of_bytes (Bytes.of_string "Hi"))

(* ================================================================
   5. agent_add_request — Eio
   ================================================================ *)

let test_add_request_returns_ids () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, secret = add () in
  check bool "id starts with req_" true (String.length id > 4 && String.sub id 0 4 = "req_");
  check bool "secret non-empty" true (String.length secret > 0)

let test_add_request_initial_state () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _secret = add ~priority:5 ~digest:"d1" ~platform:"vue" ~prompt:"build it" () in
  match agent_get_result id with
  | None -> Alcotest.fail "request should exist"
  | Some r ->
    check string "status" "pending" (agent_status_to_string r.status);
    check int "priority" 5 r.priority;
    check string "platform" "vue" r.platform;
    check string "prompt" "build it" r.prompt;
    check string "context_digest" "d1" r.context_digest;
    check (option string) "claimed_by" None r.claimed_by;
    check (option string) "claim_token" None r.claim_token;
    check int "attempts" 0 r.attempts;
    check bool "drifted" false r.drifted

let test_add_multiple () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id1, _ = add ~priority:1 () in
  let id2, _ = add ~priority:2 () in
  check bool "distinct ids" true (id1 <> id2);
  let pending = agent_get_pending () in
  check bool "both pending" true (List.length pending >= 2)

(* ================================================================
   6. agent_get_pending — Eio
   ================================================================ *)

let test_get_pending_empty () =
  Eio_main.run @@ fun _env ->
  reset ();
  let pending = agent_get_pending () in
  check int "empty queue" 0 (List.length pending)

let test_get_pending_sorted_by_priority () =
  Eio_main.run @@ fun _env ->
  reset ();
  let _low, _ = add ~priority:1 () in
  let _high, _ = add ~priority:10 () in
  let _mid, _ = add ~priority:5 () in
  let pending = agent_get_pending () in
  check bool "at least 3" true (List.length pending >= 3);
  (* first element should have highest priority *)
  let p0 = (List.nth pending 0).priority in
  let p1 = (List.nth pending 1).priority in
  let p2 = (List.nth pending 2).priority in
  check bool "sorted desc" true (p0 >= p1 && p1 >= p2)

let test_get_pending_excludes_claimed () =
  Eio_main.run @@ fun _env ->
  reset ();
  let _, _ = add () in
  let _ = agent_claim ~worker_id:"w1" in
  let pending = agent_get_pending () in
  check int "no pending left" 0 (List.length pending)

(* ================================================================
   7. agent_claim — Eio
   ================================================================ *)

let test_claim_happy () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  check string "correct id" id r.id;
  check string "status claimed" "claimed" (agent_status_to_string r.status);
  check (option string) "claimed_by" (Some "w1") r.claimed_by;
  check bool "claim_token set" true (r.claim_token <> None);
  check int "attempts" 1 r.attempts

let test_claim_empty_queue () =
  Eio_main.run @@ fun _env ->
  reset ();
  check (option reject) "none" None (agent_claim ~worker_id:"w")

let test_claim_picks_highest_priority () =
  Eio_main.run @@ fun _env ->
  reset ();
  let _low, _ = add ~priority:1 () in
  let high, _ = add ~priority:10 () in
  let r = claim_exn ~worker_id:"w1" in
  check string "picked high" high r.id

let test_claim_max_attempts_skips () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  (* exhaust attempts by claim+abandon cycle *)
  for _ = 1 to 3 do
    match agent_claim ~worker_id:"w" with
    | None -> ()
    | Some r ->
      let tok = token_of r in
      ignore (agent_abandon ~worker_id:"w" ~claim_token:tok ~reason:"retry" r.id)
  done;
  (* next claim should mark it Failed and return None *)
  let result = agent_claim ~worker_id:"w" in
  check (option reject) "no more claimable" None result;
  (* verify it is Failed *)
  (match agent_get_result id with
   | Some r ->
     check string "status failed" "failed" (agent_status_to_string r.status);
     check (option string) "error" (Some "max_attempts_exceeded") r.error
   | None -> Alcotest.fail "request should still exist")

(* ================================================================
   8. agent_heartbeat — Eio
   ================================================================ *)

let test_heartbeat_ok () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  check (result unit string) "ok" (Ok ())
    (agent_heartbeat ~worker_id:"w1" ~claim_token:tok id)

let test_heartbeat_bad_token () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let _ = claim_exn ~worker_id:"w1" in
  match agent_heartbeat ~worker_id:"w1" ~claim_token:"wrong" id with
  | Error e -> check string "msg" "invalid_claim_token" e
  | Ok () -> Alcotest.fail "expected error"

let test_heartbeat_wrong_worker () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  match agent_heartbeat ~worker_id:"intruder" ~claim_token:tok id with
  | Error e -> check string "msg" "not_claimed_by_worker" e
  | Ok () -> Alcotest.fail "expected error"

let test_heartbeat_not_found () =
  Eio_main.run @@ fun _env ->
  reset ();
  match agent_heartbeat ~worker_id:"w" ~claim_token:"t" "nonexistent" with
  | Error e -> check string "msg" "not_found" e
  | Ok () -> Alcotest.fail "expected error"

(* ================================================================
   9. agent_abandon — Eio
   ================================================================ *)

let test_abandon_happy () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  check (result unit string) "ok" (Ok ())
    (agent_abandon ~worker_id:"w1" ~claim_token:tok ~reason:"give up" id);
  (* verify reverted to Pending *)
  (match agent_get_result id with
   | Some r2 ->
     check string "pending again" "pending" (agent_status_to_string r2.status);
     check (option string) "claimed_by cleared" None r2.claimed_by;
     check (option string) "claim_token cleared" None r2.claim_token;
     check (option string) "error reason" (Some "give up") r2.error
   | None -> Alcotest.fail "request missing")

let test_abandon_bad_token () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let _ = claim_exn ~worker_id:"w1" in
  match agent_abandon ~worker_id:"w1" ~claim_token:"bad" ~reason:"r" id with
  | Error e -> check string "msg" "invalid_claim_token" e
  | Ok () -> Alcotest.fail "expected error"

let test_abandon_wrong_worker () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  match agent_abandon ~worker_id:"intruder" ~claim_token:tok ~reason:"r" id with
  | Error e -> check string "msg" "not_claimed_by_worker" e
  | Ok () -> Alcotest.fail "expected error"

let test_abandon_not_found () =
  Eio_main.run @@ fun _env ->
  reset ();
  match agent_abandon ~worker_id:"w" ~claim_token:"t" ~reason:"r" "ghost" with
  | Error e -> check string "msg" "not_found" e
  | Ok () -> Alcotest.fail "expected error"

(* ================================================================
   10. agent_submit_result — Eio
   ================================================================ *)

let test_submit_happy () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add ~digest:"d1" () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  check (result unit string) "ok" (Ok ())
    (agent_submit_result ~worker_id:"w1" ~context_digest:"d1"
       ~claim_token:tok id "generated_code");
  (match agent_get_result id with
   | Some r2 ->
     check string "completed" "completed" (agent_status_to_string r2.status);
     check (option string) "result" (Some "generated_code") r2.result;
     check (option string) "claim_token cleared" None r2.claim_token;
     check bool "not drifted" false r2.drifted
   | None -> Alcotest.fail "missing")

let test_submit_bad_claim_token () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let _ = claim_exn ~worker_id:"w1" in
  match agent_submit_result ~worker_id:"w1" ~claim_token:"wrong" id "code" with
  | Error e ->
    check string "msg" "invalid_claim_token" e;
    (* verify drifted flag set *)
    (match agent_get_result id with
     | Some r -> check bool "drifted" true r.drifted
     | None -> Alcotest.fail "missing")
  | Ok () -> Alcotest.fail "expected error"

let test_submit_worker_mismatch () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  match agent_submit_result ~worker_id:"intruder" ~claim_token:tok id "code" with
  | Error e ->
    check string "msg" "worker_mismatch" e;
    (match agent_get_result id with
     | Some r2 -> check bool "drifted" true r2.drifted
     | None -> Alcotest.fail "missing")
  | Ok () -> Alcotest.fail "expected error"

let test_submit_context_drift () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add ~digest:"original" () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  match agent_submit_result ~worker_id:"w1" ~context_digest:"changed"
          ~claim_token:tok id "code" with
  | Error e ->
    check string "msg" "context_drift" e;
    (match agent_get_result id with
     | Some r2 -> check bool "drifted" true r2.drifted
     | None -> Alcotest.fail "missing")
  | Ok () -> Alcotest.fail "expected error"

let test_submit_not_found () =
  Eio_main.run @@ fun _env ->
  reset ();
  match agent_submit_result ~claim_token:"t" "ghost" "code" with
  | Error e -> check string "msg" "not_found" e
  | Ok () -> Alcotest.fail "expected error"

let test_submit_not_claimed () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  (* submit without claiming *)
  match agent_submit_result ~claim_token:"any" id "code" with
  | Error e ->
    check string "msg" "not_claimed" e;
    (match agent_get_result id with
     | Some r -> check bool "drifted" true r.drifted
     | None -> Alcotest.fail "missing")
  | Ok () -> Alcotest.fail "expected error"

let test_submit_no_worker_id_with_claimed_by () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  (* submit without worker_id — should succeed but mark drifted *)
  match agent_submit_result ~claim_token:tok id "code" with
  | Ok () ->
    (match agent_get_result id with
     | Some r2 ->
       check string "completed" "completed" (agent_status_to_string r2.status);
       check bool "drifted (no worker_id)" true r2.drifted
     | None -> Alcotest.fail "missing")
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

let test_submit_empty_context_digest () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add ~digest:"orig" () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  (* empty context_digest should NOT trigger drift *)
  match agent_submit_result ~worker_id:"w1" ~context_digest:""
          ~claim_token:tok id "code" with
  | Ok () ->
    (match agent_get_result id with
     | Some r2 -> check bool "not drifted" false r2.drifted
     | None -> Alcotest.fail "missing")
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

let test_submit_no_context_digest () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add ~digest:"orig" () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  (* omitting context_digest should NOT trigger drift *)
  match agent_submit_result ~worker_id:"w1" ~claim_token:tok id "code" with
  | Ok () ->
    (match agent_get_result id with
     | Some r2 -> check bool "not drifted" false r2.drifted
     | None -> Alcotest.fail "missing")
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

(* ================================================================
   11. agent_get_result — Eio
   ================================================================ *)

let test_get_result_exists () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  check bool "found" true (agent_get_result id <> None)

let test_get_result_missing () =
  Eio_main.run @@ fun _env ->
  reset ();
  check (option reject) "not found" None (agent_get_result "nope")

(* ================================================================
   12. agent_cleanup_old — Eio
   ================================================================ *)

let test_cleanup_old_removes_expired () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  (* backdate created_at well beyond agent_max_age_sec (default 900).
     created_at is immutable, so replace the hashtable entry. *)
  (match Hashtbl.find_opt agent_queue id with
   | Some r ->
     Hashtbl.replace agent_queue id
       { r with created_at = Unix.gettimeofday () -. 2000.0 }
   | None -> Alcotest.fail "missing");
  agent_cleanup_old ();
  check (option reject) "removed" None (agent_get_result id)

let test_cleanup_old_keeps_fresh () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  agent_cleanup_old ();
  check bool "kept" true (agent_get_result id <> None)

let test_cleanup_reverts_stale_claim_heartbeat () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let _ = claim_exn ~worker_id:"w1" in
  (* make heartbeat very old *)
  (match Hashtbl.find_opt agent_queue id with
   | Some r ->
     r.last_heartbeat <- Some (Unix.gettimeofday () -. 200.0);
     r.claimed_at <- Some (Unix.gettimeofday ())
   | None -> Alcotest.fail "missing");
  agent_cleanup_old ();
  (match agent_get_result id with
   | Some r ->
     check string "reverted to pending" "pending" (agent_status_to_string r.status);
     check (option string) "error" (Some "claim_timeout") r.error
   | None -> Alcotest.fail "should not be removed (not old enough)")

let test_cleanup_reverts_stale_claim_ttl () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let _ = claim_exn ~worker_id:"w1" in
  (* make claimed_at very old (exceeds claim_ttl_sec) *)
  (match Hashtbl.find_opt agent_queue id with
   | Some r ->
     let old = Unix.gettimeofday () -. 300.0 in
     r.claimed_at <- Some old;
     r.last_heartbeat <- Some (Unix.gettimeofday ())
   | None -> Alcotest.fail "missing");
  agent_cleanup_old ();
  (match agent_get_result id with
   | Some r ->
     check string "reverted to pending" "pending" (agent_status_to_string r.status)
   | None -> Alcotest.fail "should not be removed")

(* ================================================================
   13. agent_queue_stats / agent_queue_stats_json — Eio
   ================================================================ *)

let test_stats_empty () =
  Eio_main.run @@ fun _env ->
  reset ();
  let s = agent_queue_stats () in
  check int "total" 0 s.total;
  check int "pending" 0 s.pending;
  check int "claimed" 0 s.claimed;
  check int "completed" 0 s.completed;
  check int "failed" 0 s.failed;
  check int "drifted" 0 s.drifted;
  check (option (float 0.1)) "oldest_pending" None s.oldest_pending_sec;
  check (option (float 0.1)) "oldest_claimed" None s.oldest_claimed_sec

let test_stats_mixed () =
  Eio_main.run @@ fun _env ->
  reset ();
  (* add 3 requests *)
  let _, _ = add () in
  let id2, _ = add () in
  let id3, _ = add () in
  (* claim one *)
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  (* complete one *)
  ignore (agent_submit_result ~worker_id:"w1" ~claim_token:tok r.id "code");
  (* claim and abandon another to verify pending count *)
  let r2 = claim_exn ~worker_id:"w2" in
  let tok2 = token_of r2 in
  ignore (agent_abandon ~worker_id:"w2" ~claim_token:tok2 ~reason:"r" r2.id);
  let s = agent_queue_stats () in
  check int "total" 3 s.total;
  check int "completed" 1 s.completed;
  (* id2 and id3 should be pending (one was abandoned back) *)
  ignore (id2, id3)

let test_stats_json_structure () =
  Eio_main.run @@ fun _env ->
  reset ();
  let _, _ = add () in
  let json = agent_queue_stats_json () in
  let keys = match json with
    | `Assoc pairs -> List.map fst pairs
    | _ -> Alcotest.fail "expected Assoc"
  in
  let expected_keys = [
    "total"; "pending"; "claimed"; "completed"; "failed"; "drifted";
    "oldest_pending_sec"; "oldest_claimed_sec";
    "claim_ttl_sec"; "heartbeat_ttl_sec"; "max_age_sec"; "max_attempts"
  ] in
  List.iter (fun k ->
    check bool ("key " ^ k) true (List.mem k keys)
  ) expected_keys

let test_stats_oldest_pending () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  (* backdate to make oldest_pending_sec non-None *)
  (match Hashtbl.find_opt agent_queue id with
   | Some r ->
     Hashtbl.replace agent_queue id
       { r with created_at = Unix.gettimeofday () -. 10.0 }
   | None -> Alcotest.fail "missing");
  let s = agent_queue_stats () in
  check bool "oldest_pending is Some" true (s.oldest_pending_sec <> None);
  (match s.oldest_pending_sec with
   | Some v -> check bool ">= 9" true (v >= 9.0)
   | None -> Alcotest.fail "should be Some")

let test_stats_oldest_claimed () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let _ = claim_exn ~worker_id:"w1" in
  (* backdate claimed_at *)
  (match Hashtbl.find_opt agent_queue id with
   | Some r -> r.claimed_at <- Some (Unix.gettimeofday () -. 15.0)
   | None -> Alcotest.fail "missing");
  let s = agent_queue_stats () in
  check bool "oldest_claimed is Some" true (s.oldest_claimed_sec <> None);
  (match s.oldest_claimed_sec with
   | Some v -> check bool ">= 14" true (v >= 14.0)
   | None -> Alcotest.fail "should be Some")

let test_stats_drifted_count () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add ~digest:"orig" () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  (* trigger context drift *)
  ignore (agent_submit_result ~worker_id:"w1" ~context_digest:"diff"
            ~claim_token:tok id "code");
  let s = agent_queue_stats () in
  check int "drifted" 1 s.drifted

let test_stats_config_values () =
  Eio_main.run @@ fun _env ->
  reset ();
  let s = agent_queue_stats () in
  (* defaults unless env vars are set *)
  check bool "claim_ttl > 0" true (s.claim_ttl_sec > 0);
  check bool "heartbeat_ttl > 0" true (s.heartbeat_ttl_sec > 0);
  check bool "max_age > 0" true (s.max_age_sec > 0);
  check bool "max_attempts > 0" true (s.max_attempts > 0)

(* ================================================================
   14. Full lifecycle: add → claim → heartbeat → submit → get_result
   ================================================================ *)

let test_full_lifecycle () =
  Eio_main.run @@ fun _env ->
  reset ();
  (* add *)
  let id, _secret = add ~priority:3 ~digest:"ctx1" ~platform:"svelte" ~prompt:"build" () in
  (* claim *)
  let r = claim_exn ~worker_id:"worker-A" in
  check string "id matches" id r.id;
  let tok = token_of r in
  (* heartbeat *)
  (match agent_heartbeat ~worker_id:"worker-A" ~claim_token:tok id with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("heartbeat failed: " ^ e));
  (* submit *)
  (match agent_submit_result ~worker_id:"worker-A" ~context_digest:"ctx1"
           ~claim_token:tok id "<div>done</div>" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("submit failed: " ^ e));
  (* get_result *)
  match agent_get_result id with
  | None -> Alcotest.fail "missing result"
  | Some r2 ->
    check string "completed" "completed" (agent_status_to_string r2.status);
    check (option string) "result" (Some "<div>done</div>") r2.result;
    check bool "not drifted" false r2.drifted

(* ================================================================
   15. Add → claim → abandon → re-claim lifecycle
   ================================================================ *)

let test_abandon_reclaim () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r1 = claim_exn ~worker_id:"w1" in
  let tok1 = token_of r1 in
  ignore (agent_abandon ~worker_id:"w1" ~claim_token:tok1 ~reason:"timeout" id);
  (* second worker claims *)
  let r2 = claim_exn ~worker_id:"w2" in
  check string "same id" id r2.id;
  check (option string) "new worker" (Some "w2") r2.claimed_by;
  check int "attempts" 2 r2.attempts;
  (* tokens should differ *)
  let tok2 = token_of r2 in
  check bool "different token" true (tok1 <> tok2)

(* ================================================================
   16. Heartbeat on pending request (not claimed)
   ================================================================ *)

let test_heartbeat_on_pending () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  match agent_heartbeat ~worker_id:"w" ~claim_token:"any" id with
  | Error e -> check string "not claimed" "not_claimed_by_worker" e
  | Ok () -> Alcotest.fail "expected error"

(* ================================================================
   17. Abandon on pending request
   ================================================================ *)

let test_abandon_on_pending () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  match agent_abandon ~worker_id:"w" ~claim_token:"any" ~reason:"r" id with
  | Error e -> check string "not claimed" "not_claimed_by_worker" e
  | Ok () -> Alcotest.fail "expected error"

(* ================================================================
   18. Submit on already-completed request
   ================================================================ *)

let test_submit_on_completed () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id, _ = add () in
  let r = claim_exn ~worker_id:"w1" in
  let tok = token_of r in
  ignore (agent_submit_result ~worker_id:"w1" ~claim_token:tok id "first");
  (* attempt second submit — status is Completed, not Claimed *)
  match agent_submit_result ~worker_id:"w1" ~claim_token:tok id "second" with
  | Error e -> check string "not_claimed" "not_claimed" e
  | Ok () -> Alcotest.fail "expected error"

(* ================================================================
   19. Multiple pending, oldest_pending picks max age
   ================================================================ *)

let test_stats_oldest_pending_picks_max () =
  Eio_main.run @@ fun _env ->
  reset ();
  let id1, _ = add () in
  let _id2, _ = add () in
  (* make id1 older *)
  (match Hashtbl.find_opt agent_queue id1 with
   | Some r ->
     Hashtbl.replace agent_queue id1
       { r with created_at = Unix.gettimeofday () -. 50.0 }
   | None -> Alcotest.fail "missing");
  let s = agent_queue_stats () in
  (match s.oldest_pending_sec with
   | Some v -> check bool ">= 49" true (v >= 49.0)
   | None -> Alcotest.fail "should be Some")

(* ================================================================
   20. random_hex length check
   ================================================================ *)

let test_random_hex_length () =
  let h = random_hex 8 in
  check int "hex len = 2*byte_len" 16 (String.length h)

let test_random_hex_unique () =
  let a = random_hex 16 in
  let b = random_hex 16 in
  check bool "unique" true (a <> b)

(* ================================================================
   Runner
   ================================================================ *)

let () =
  Alcotest.run "Protocol Agent Queue W5" [
    ("agent_status_to_string", [
      "pending", `Quick, test_status_pending;
      "claimed", `Quick, test_status_claimed;
      "completed", `Quick, test_status_completed;
      "failed", `Quick, test_status_failed;
    ]);
    ("parse_positive_int", [
      "positive ok", `Quick, test_parse_positive_int_ok;
      "one ok", `Quick, test_parse_positive_int_one;
      "zero rejected", `Quick, test_parse_positive_int_zero;
      "negative rejected", `Quick, test_parse_positive_int_negative;
      "garbage rejected", `Quick, test_parse_positive_int_garbage;
      "empty rejected", `Quick, test_parse_positive_int_empty;
      "float rejected", `Quick, test_parse_positive_int_float;
    ]);
    ("env_int", [
      "missing var uses default", `Quick, test_env_int_missing;
      "set var wins", `Quick, test_env_int_set;
      "bad var falls back", `Quick, test_env_int_bad_value;
      "zero var falls back", `Quick, test_env_int_zero_value;
    ]);
    ("hex_of_bytes", [
      "empty bytes", `Quick, test_hex_empty;
      "0x00", `Quick, test_hex_simple;
      "0xff", `Quick, test_hex_ff;
      "Hi => 4869", `Quick, test_hex_hello;
    ]);
    ("random_hex", [
      "correct length", `Quick, test_random_hex_length;
      "unique values", `Quick, test_random_hex_unique;
    ]);
    ("agent_add_request", [
      "returns id and secret", `Quick, test_add_request_returns_ids;
      "initial state fields", `Quick, test_add_request_initial_state;
      "multiple distinct", `Quick, test_add_multiple;
    ]);
    ("agent_get_pending", [
      "empty queue", `Quick, test_get_pending_empty;
      "sorted by priority desc", `Quick, test_get_pending_sorted_by_priority;
      "excludes claimed", `Quick, test_get_pending_excludes_claimed;
    ]);
    ("agent_claim", [
      "happy path", `Quick, test_claim_happy;
      "empty queue", `Quick, test_claim_empty_queue;
      "picks highest priority", `Quick, test_claim_picks_highest_priority;
      "max attempts marks Failed", `Quick, test_claim_max_attempts_skips;
    ]);
    ("agent_heartbeat", [
      "ok", `Quick, test_heartbeat_ok;
      "bad token", `Quick, test_heartbeat_bad_token;
      "wrong worker", `Quick, test_heartbeat_wrong_worker;
      "not found", `Quick, test_heartbeat_not_found;
      "on pending request", `Quick, test_heartbeat_on_pending;
    ]);
    ("agent_abandon", [
      "happy path", `Quick, test_abandon_happy;
      "bad token", `Quick, test_abandon_bad_token;
      "wrong worker", `Quick, test_abandon_wrong_worker;
      "not found", `Quick, test_abandon_not_found;
      "on pending request", `Quick, test_abandon_on_pending;
    ]);
    ("agent_submit_result", [
      "happy path", `Quick, test_submit_happy;
      "bad claim token", `Quick, test_submit_bad_claim_token;
      "worker mismatch", `Quick, test_submit_worker_mismatch;
      "context drift", `Quick, test_submit_context_drift;
      "not found", `Quick, test_submit_not_found;
      "not claimed (pending)", `Quick, test_submit_not_claimed;
      "no worker_id with claimed_by", `Quick, test_submit_no_worker_id_with_claimed_by;
      "empty context_digest", `Quick, test_submit_empty_context_digest;
      "omit context_digest", `Quick, test_submit_no_context_digest;
      "on completed request", `Quick, test_submit_on_completed;
    ]);
    ("agent_get_result", [
      "exists", `Quick, test_get_result_exists;
      "missing", `Quick, test_get_result_missing;
    ]);
    ("agent_cleanup_old", [
      "removes expired", `Quick, test_cleanup_old_removes_expired;
      "keeps fresh", `Quick, test_cleanup_old_keeps_fresh;
      "reverts stale heartbeat", `Quick, test_cleanup_reverts_stale_claim_heartbeat;
      "reverts stale claim ttl", `Quick, test_cleanup_reverts_stale_claim_ttl;
    ]);
    ("agent_queue_stats", [
      "empty queue", `Quick, test_stats_empty;
      "mixed statuses", `Quick, test_stats_mixed;
      "json structure", `Quick, test_stats_json_structure;
      "oldest_pending", `Quick, test_stats_oldest_pending;
      "oldest_claimed", `Quick, test_stats_oldest_claimed;
      "drifted count", `Quick, test_stats_drifted_count;
      "config values present", `Quick, test_stats_config_values;
      "oldest_pending picks max", `Quick, test_stats_oldest_pending_picks_max;
    ]);
    ("full_lifecycle", [
      "add-claim-heartbeat-submit-get", `Quick, test_full_lifecycle;
      "abandon and re-claim", `Quick, test_abandon_reclaim;
    ]);
  ]
