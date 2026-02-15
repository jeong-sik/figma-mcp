(** Test: In-flight request deduplication in figma_api_eio.ml

    Verifies that concurrent GET requests for the same URL are coalesced
    into a single HTTP call, with joiners receiving the leader's result.

    Uses Eio.Fiber.yield for deterministic synchronization instead of
    wall-clock sleeps. In Eio's cooperative scheduler, yield lets one
    ready fiber run; blocked fibers (on promises) stay blocked. *)

open Alcotest

(** Yield enough times for other fibers to enter dedup_get and find the
    in-flight entry. Each joiner runs to its promise-await point (blocking),
    so a single cascade through N-1 joiners is sufficient. We use 20 yields
    for robustness against scheduler implementation changes. *)
let hold_for_joiners () =
  for _ = 1 to 20 do Eio.Fiber.yield () done

(** dedup_get coalesces: 5 fibers requesting the same key,
    only 1 compute executes, all get the same result. *)
let test_dedup_coalesces () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:3 net in

  let compute_count = Atomic.make 0 in

  let task _i =
    Figma_api_eio.dedup_get client "same-url" (fun () ->
      ignore (Atomic.fetch_and_add compute_count 1);
      hold_for_joiners ();
      Ok (`Assoc [("status", `String "ok")]))
  in

  let results = Array.make 5 (Error (Figma_api_eio.Http_error (0, "", None))) in
  Eio.Fiber.all
    (List.init 5 (fun i -> fun () ->
       results.(i) <- task i));

  (* Only 1 compute should have run *)
  check int "compute ran exactly once" 1 (Atomic.get compute_count);

  (* All fibers got Ok *)
  Array.iteri (fun i r ->
    match r with
    | Ok _ -> ()
    | Error _ ->
      fail (Printf.sprintf "fiber %d got Error, expected Ok" i)
  ) results

(** Different keys run independently, not coalesced. *)
let test_dedup_different_keys_independent () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:5 net in

  let compute_count = Atomic.make 0 in

  let task key =
    Figma_api_eio.dedup_get client key (fun () ->
      ignore (Atomic.fetch_and_add compute_count 1);
      hold_for_joiners ();
      Ok (`String key))
  in

  Eio.Fiber.all
    [ (fun () -> ignore (task "url-a"))
    ; (fun () -> ignore (task "url-b"))
    ; (fun () -> ignore (task "url-c"))
    ];

  check int "each key triggers its own compute" 3 (Atomic.get compute_count)

(** Error from leader propagates to all joiners via raise. *)
let test_dedup_error_propagation () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:3 net in

  let compute_count = Atomic.make 0 in

  let task _i =
    try
      let _ : (Yojson.Safe.t, Figma_api_eio.api_error) result =
        Figma_api_eio.dedup_get client "fail-url" (fun () ->
          ignore (Atomic.fetch_and_add compute_count 1);
          hold_for_joiners ();
          raise (Failure "network down"))
      in
      `Ok
    with Failure msg -> `Failed msg
  in

  let results = Array.make 3 `Ok in
  Eio.Fiber.all
    (List.init 3 (fun i -> fun () ->
       results.(i) <- task i));

  check int "compute ran exactly once" 1 (Atomic.get compute_count);

  Array.iteri (fun i r ->
    match r with
    | `Failed msg ->
      check string (Printf.sprintf "fiber %d got correct error" i) "network down" msg
    | `Ok ->
      fail (Printf.sprintf "fiber %d should have raised, got Ok" i)
  ) results

(** After completion, inflight entry is cleaned up.
    A second call triggers a new compute. *)
let test_dedup_cleanup_after_completion () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:3 net in

  let compute_count = Atomic.make 0 in

  let call () =
    Figma_api_eio.dedup_get client "reuse-url" (fun () ->
      ignore (Atomic.fetch_and_add compute_count 1);
      Ok (`String "done"))
  in

  (* First call *)
  let _ = call () in
  check int "first call triggered compute" 1 (Atomic.get compute_count);

  (* Second call — key was cleaned up, so this triggers a new compute *)
  let _ = call () in
  check int "second call triggered new compute" 2 (Atomic.get compute_count)

(** Result-level errors (Ok/Error) are passed through, not raised. *)
let test_dedup_result_error_passthrough () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:3 net in

  let task _i =
    Figma_api_eio.dedup_get client "err-result-url" (fun () ->
      hold_for_joiners ();
      Error (Figma_api_eio.Http_error (429, "rate limited", None)))
  in

  let results =
    Array.make 3 (Ok (`Null) : (Yojson.Safe.t, Figma_api_eio.api_error) result)
  in
  Eio.Fiber.all
    (List.init 3 (fun i -> fun () ->
       results.(i) <- task i));

  Array.iteri (fun i r ->
    match r with
    | Error (Figma_api_eio.Http_error (429, _, _)) -> ()
    | _ ->
      fail (Printf.sprintf "fiber %d expected HttpError 429" i)
  ) results

let () =
  run "request_dedup"
    [ ( "dedup"
      , [ test_case "concurrent same-key coalesces to 1 compute" `Quick
            test_dedup_coalesces
        ; test_case "different keys run independently" `Quick
            test_dedup_different_keys_independent
        ; test_case "exception propagates to all joiners" `Quick
            test_dedup_error_propagation
        ; test_case "inflight cleaned up after completion" `Quick
            test_dedup_cleanup_after_completion
        ; test_case "result-level Error passes through (not raised)" `Quick
            test_dedup_result_error_passthrough
        ] )
    ]
