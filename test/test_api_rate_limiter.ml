(** Test: Eio.Semaphore-based API rate limiter in figma_api_eio.ml

    Spawns 5 concurrent fibers through a semaphore limited to 2 slots.
    Verifies that at no point more than 2 fibers execute simultaneously. *)

open Alcotest

let test_concurrency_bounded_by_semaphore () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:2 net in

  (* Shared atomic counters for lock-free concurrency tracking *)
  let current = Atomic.make 0 in
  let peak = Atomic.make 0 in
  let violations = Atomic.make 0 in

  let task _i =
    Figma_api_eio.with_api_limiter client (fun () ->
      let c = Atomic.fetch_and_add current 1 + 1 in
      (* CAS loop to update peak *)
      let rec update_peak () =
        let old_peak = Atomic.get peak in
        if c > old_peak then
          if not (Atomic.compare_and_set peak old_peak c) then update_peak ()
      in
      update_peak ();
      if c > 2 then Atomic.incr violations;
      (* Hold the slot long enough for other fibers to contend *)
      Eio.Time.sleep clock 0.05;
      ignore (Atomic.fetch_and_add current (-1)))
  in

  Eio.Fiber.all
    [ (fun () -> task 0)
    ; (fun () -> task 1)
    ; (fun () -> task 2)
    ; (fun () -> task 3)
    ; (fun () -> task 4)
    ];

  let peak_val = Atomic.get peak in
  let violation_count = Atomic.get violations in

  check int "no concurrency violations" 0 violation_count;
  check bool "peak concurrency <= 2" true (peak_val <= 2);
  check bool "peak concurrency >= 1 (sanity)" true (peak_val >= 1)

let test_single_slot_serializes () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let client = Figma_api_eio.make_client ~max_concurrent_api:1 net in

  let current = Atomic.make 0 in
  let peak = Atomic.make 0 in

  let task _i =
    Figma_api_eio.with_api_limiter client (fun () ->
      let c = Atomic.fetch_and_add current 1 + 1 in
      let rec update_peak () =
        let old_peak = Atomic.get peak in
        if c > old_peak then
          if not (Atomic.compare_and_set peak old_peak c) then update_peak ()
      in
      update_peak ();
      Eio.Time.sleep clock 0.02;
      ignore (Atomic.fetch_and_add current (-1)))
  in

  Eio.Fiber.all
    [ (fun () -> task 0)
    ; (fun () -> task 1)
    ; (fun () -> task 2)
    ];

  check int "peak concurrency is exactly 1" 1 (Atomic.get peak)

let () =
  run "api_rate_limiter"
    [ ( "concurrency"
      , [ test_case "bounded by semaphore (max=2, fibers=5)" `Quick
            test_concurrency_bounded_by_semaphore
        ; test_case "single slot serializes (max=1, fibers=3)" `Quick
            test_single_slot_serializes
        ] )
    ]
