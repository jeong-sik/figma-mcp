(** Agent queue state machine tests *)

open Alcotest

let ctx = "ctx-123"

let add_req () =
  Mcp_protocol_eio.agent_add_request ~priority:0 ~context_digest:ctx `Null "react" "prompt"

let get_req_exn id =
  match Mcp_protocol_eio.agent_get_result id with
  | Some req -> req
  | None -> failwith ("missing request: " ^ id)

let claim_exn ~worker_id =
  match Mcp_protocol_eio.agent_claim ~worker_id with
  | Some req -> req
  | None -> failwith "expected a pending request to claim"

let complete_exn ~worker_id id =
  ignore (claim_exn ~worker_id);
  match Mcp_protocol_eio.agent_submit_result ~worker_id ~context_digest:ctx id "code" with
  | Ok () -> ()
  | Error e -> failwith ("expected completion ok, got: " ^ e)

let test_submit_without_claim_is_rejected () =
  Eio_main.run @@ fun _env ->
  let id = add_req () in
  (match Mcp_protocol_eio.agent_submit_result ~worker_id:"w1" ~context_digest:ctx id "code" with
   | Ok () -> failwith "expected Error"
   | Error e -> check string "error" "not_claimed" e);
  let req = get_req_exn id in
  check string "status" "pending" (Mcp_protocol_eio.agent_status_to_string req.status);
  check bool "drifted" true req.drifted;
  check bool "no result" true (Option.is_none req.result);
  (* cleanup: complete it so later tests don't see pending *)
  complete_exn ~worker_id:"w1" id

let test_claim_then_submit_ok () =
  Eio_main.run @@ fun _env ->
  let id = add_req () in
  let req = claim_exn ~worker_id:"w1" in
  check string "claimed id" id req.id;
  (match Mcp_protocol_eio.agent_submit_result ~worker_id:"w1" ~context_digest:ctx id "code" with
   | Ok () -> ()
   | Error e -> failwith ("unexpected error: " ^ e));
  let req2 = get_req_exn id in
  check string "status" "completed" (Mcp_protocol_eio.agent_status_to_string req2.status);
  check bool "has result" true (Option.is_some req2.result)

let test_worker_mismatch_requeues () =
  Eio_main.run @@ fun _env ->
  let id = add_req () in
  ignore (claim_exn ~worker_id:"w1");
  (match Mcp_protocol_eio.agent_submit_result ~worker_id:"w2" ~context_digest:ctx id "code" with
   | Ok () -> failwith "expected Error"
   | Error e -> check string "error" "worker_mismatch" e);
  let req = get_req_exn id in
  check string "status" "pending" (Mcp_protocol_eio.agent_status_to_string req.status);
  check bool "drifted" true req.drifted;
  check bool "claim cleared" true (Option.is_none req.claimed_by);
  check bool "no result" true (Option.is_none req.result);
  (* cleanup *)
  complete_exn ~worker_id:"w1" id

let test_context_drift_requeues () =
  Eio_main.run @@ fun _env ->
  let id = add_req () in
  ignore (claim_exn ~worker_id:"w1");
  (match Mcp_protocol_eio.agent_submit_result ~worker_id:"w1" ~context_digest:"wrong" id "code" with
   | Ok () -> failwith "expected Error"
   | Error e -> check string "error" "context_drift" e);
  let req = get_req_exn id in
  check string "status" "pending" (Mcp_protocol_eio.agent_status_to_string req.status);
  check bool "drifted" true req.drifted;
  check bool "no result" true (Option.is_none req.result);
  (* cleanup *)
  complete_exn ~worker_id:"w1" id

let test_missing_worker_id_is_allowed_but_drifted () =
  Eio_main.run @@ fun _env ->
  let id = add_req () in
  ignore (claim_exn ~worker_id:"w1");
  (match Mcp_protocol_eio.agent_submit_result ~context_digest:ctx id "code" with
   | Ok () -> ()
   | Error e -> failwith ("unexpected error: " ^ e));
  let req = get_req_exn id in
  check string "status" "completed" (Mcp_protocol_eio.agent_status_to_string req.status);
  check bool "drifted" true req.drifted;
  check bool "has result" true (Option.is_some req.result)

let test_already_completed_rejected () =
  Eio_main.run @@ fun _env ->
  let id = add_req () in
  ignore (claim_exn ~worker_id:"w1");
  (match Mcp_protocol_eio.agent_submit_result ~worker_id:"w1" ~context_digest:ctx id "code" with
   | Ok () -> ()
   | Error e -> failwith ("unexpected error: " ^ e));
  (match Mcp_protocol_eio.agent_submit_result ~worker_id:"w1" ~context_digest:ctx id "new" with
   | Ok () -> failwith "expected Error"
   | Error e -> check string "error" "already_completed" e);
  let req = get_req_exn id in
  check (option string) "result unchanged" (Some "code") req.result

let () =
  run "Agent Queue" [
    "state", [
      "submit without claim rejected", `Quick, test_submit_without_claim_is_rejected;
      "claim then submit ok", `Quick, test_claim_then_submit_ok;
      "worker mismatch requeues", `Quick, test_worker_mismatch_requeues;
      "context drift requeues", `Quick, test_context_drift_requeues;
      "missing worker_id allowed", `Quick, test_missing_worker_id_is_allowed_but_drifted;
      "already completed rejected", `Quick, test_already_completed_rejected;
    ];
  ]

