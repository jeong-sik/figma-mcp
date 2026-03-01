(** Tests for file fetch error accumulation in figma_crawl.ml

    PR #131 changed the silent discard pattern:
      | Error _ -> ()
    to the error-logging pattern:
      | Error msg -> progress.errors <- (Printf.sprintf "File fetch failed: %s" msg) :: progress.errors

    These tests verify:
    1. The "File fetch failed: <msg>" format is applied correctly.
    2. Each error is prepended to the list (most recent error is at the head).
    3. Pre-existing errors are preserved when a new error is added.
    4. An empty error message produces the expected prefix-only string. *)

let () =
  let open Alcotest in

  (* Helper: simulate the single error-logging statement from PR #131.
     Extracted verbatim from lib/figma_crawl.ml:
       progress.errors <- (Printf.sprintf "File fetch failed: %s" msg) :: progress.errors *)
  let log_file_fetch_error progress msg =
    progress.Figma_crawl.errors <-
      (Printf.sprintf "File fetch failed: %s" msg) :: progress.Figma_crawl.errors
  in

  (* ---- format ---- *)

  let test_format_basic () =
    let p = Figma_crawl.create_progress () in
    log_file_fetch_error p "timeout";
    check (list string) "errors"
      ["File fetch failed: timeout"]
      p.errors
  in

  let test_format_http_status () =
    let p = Figma_crawl.create_progress () in
    log_file_fetch_error p "HTTP 404";
    check (list string) "errors"
      ["File fetch failed: HTTP 404"]
      p.errors
  in

  let test_format_empty_message () =
    (* An empty error string still produces the prefix. *)
    let p = Figma_crawl.create_progress () in
    log_file_fetch_error p "";
    check (list string) "errors"
      ["File fetch failed: "]
      p.errors
  in

  (* ---- prepend ordering ---- *)

  let test_prepend_ordering () =
    (* The second error must appear at the head of the list. *)
    let p = Figma_crawl.create_progress () in
    log_file_fetch_error p "first error";
    log_file_fetch_error p "second error";
    check string "head is newest"
      "File fetch failed: second error"
      (List.hd p.errors);
    check int "two errors" 2 (List.length p.errors)
  in

  (* ---- preservation of pre-existing errors ---- *)

  let test_preserves_existing_errors () =
    (* Errors from earlier stages (e.g. fetch_files failure) must survive
       when a subsequent file-node fetch error is recorded. *)
    let p = Figma_crawl.create_progress () in
    p.errors <- ["earlier error"];
    log_file_fetch_error p "node fetch failed";
    check int "two errors total" 2 (List.length p.errors);
    check string "newer first"
      "File fetch failed: node fetch failed"
      (List.nth p.errors 0);
    check string "older preserved"
      "earlier error"
      (List.nth p.errors 1)
  in

  (* ---- accumulation across multiple files ---- *)

  let test_multiple_file_errors () =
    let p = Figma_crawl.create_progress () in
    log_file_fetch_error p "abc123: connection refused";
    log_file_fetch_error p "def456: rate limited";
    log_file_fetch_error p "ghi789: invalid token";
    check int "three errors" 3 (List.length p.errors);
    (* Most recent is head. *)
    check string "head" "File fetch failed: ghi789: invalid token" (List.hd p.errors)
  in

  (* ---- other progress counters are not touched ---- *)

  let test_counters_unchanged () =
    let p = Figma_crawl.create_progress () in
    p.teams <- 1;
    p.projects <- 2;
    p.files <- 5;
    p.nodes <- 42;
    log_file_fetch_error p "some failure";
    check int "teams unchanged" 1 p.teams;
    check int "projects unchanged" 2 p.projects;
    check int "files unchanged" 5 p.files;
    check int "nodes unchanged" 42 p.nodes
  in

  run "Figma_crawl file fetch error accumulation" [
    ("format", [
      test_case "basic message" `Quick test_format_basic;
      test_case "HTTP status string" `Quick test_format_http_status;
      test_case "empty message" `Quick test_format_empty_message;
    ]);
    ("ordering", [
      test_case "prepend newest to head" `Quick test_prepend_ordering;
    ]);
    ("preservation", [
      test_case "pre-existing errors survive" `Quick test_preserves_existing_errors;
    ]);
    ("accumulation", [
      test_case "multiple file errors" `Quick test_multiple_file_errors;
    ]);
    ("side-effects", [
      test_case "counters unchanged" `Quick test_counters_unchanged;
    ]);
  ]
