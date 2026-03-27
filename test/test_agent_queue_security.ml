open Alcotest

let reset_queue () =
  Hashtbl.reset Figma_protocol_eio.agent_queue

let test_claim_issues_token_and_enforces_it () =
  Eio_main.run @@ fun _env ->
  reset_queue ();
  let node = Yojson.Safe.from_string "{}" in
  let req_id, request_secret =
    Figma_protocol_eio.agent_add_request
      ~priority:0
      ~context_digest:"ctx"
      node
      "react"
      "prompt"
  in
  check bool "request_secret non-empty" true (String.length request_secret > 0);
  match Figma_protocol_eio.agent_claim ~worker_id:"worker-1" with
  | None -> fail "expected claimed request"
  | Some req ->
      (match req.Figma_protocol_eio.claim_token with
       | None -> fail "expected claim_token"
       | Some token ->
           check bool "claim token non-empty" true (String.length token > 0);
           (match Figma_protocol_eio.agent_heartbeat ~worker_id:"worker-1" ~claim_token:"bad" req_id with
            | Ok () -> fail "expected invalid_claim_token"
            | Error _ -> ());
           (match Figma_protocol_eio.agent_heartbeat ~worker_id:"worker-1" ~claim_token:token req_id with
            | Ok () -> ()
            | Error e -> fail e);
           (match Figma_protocol_eio.agent_submit_result ~claim_token:"bad" req_id "code" with
            | Ok () -> fail "expected invalid_claim_token"
            | Error _ -> ());
           (match Figma_protocol_eio.agent_submit_result ~worker_id:"worker-1" ~claim_token:token req_id "code" with
            | Ok () -> ()
            | Error e -> fail e);
           let req2 =
             match Figma_protocol_eio.agent_get_result req_id with
             | None -> fail "expected request in queue"
             | Some r -> r
           in
           check string "status completed" "completed"
             (Figma_protocol_eio.agent_status_to_string req2.Figma_protocol_eio.status);
           check (option string) "result set" (Some "code") req2.Figma_protocol_eio.result;
           check (option string) "claim token cleared" None req2.Figma_protocol_eio.claim_token)

let () =
  run "Agent Queue Security" [
    "tokens", [
      "claim token enforced", `Quick, test_claim_issues_token_and_enforces_it;
    ];
  ]

