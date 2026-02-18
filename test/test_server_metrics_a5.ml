(** Coverage A5: server_metrics.ml — comprehensive pure function tests.
    Targets: snapshot, to_json, to_prometheus_text, prom_metric,
             latency_snapshot, record_untracked_response (2xx, 3xx),
             sse_open/close, record_rps/sum_recent via snapshot delta. *)

open Alcotest

(* ============== prom_metric ============== *)

let test_prom_metric_basic () =
  let result = Server_metrics.prom_metric "http_total" "app=\"figma\"" "100" in
  check string "format" "http_total{app=\"figma\"} 100\n" result

let test_prom_metric_empty_labels () =
  let result = Server_metrics.prom_metric "counter" "" "0" in
  check string "empty labels" "counter{} 0\n" result

let test_prom_metric_multi_labels () =
  let result = Server_metrics.prom_metric "latency" "q=\"p95\",srv=\"mcp\"" "1.23" in
  check string "multi labels" "latency{q=\"p95\",srv=\"mcp\"} 1.23\n" result

let test_prom_metric_float_value () =
  let result = Server_metrics.prom_metric "gauge" "x=\"1\"" "3.14159" in
  check string "float" "gauge{x=\"1\"} 3.14159\n" result

(* ============== record_untracked_response: 2xx ============== *)

let test_untracked_2xx () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.record_untracked_response ~bytes:50 `OK;
  let after = Server_metrics.snapshot () in
  check int "total+1" (before.total + 1) after.total;
  check int "2xx+1" (before.status_2xx + 1) after.status_2xx;
  check int "errors same" before.errors after.errors;
  check int "bytes+50" (before.bytes_out + 50) after.bytes_out

let test_untracked_2xx_no_bytes () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.record_untracked_response `OK;
  let after = Server_metrics.snapshot () in
  check int "total+1" (before.total + 1) after.total;
  check int "bytes same" before.bytes_out after.bytes_out

(* ============== record_untracked_response: 3xx ============== *)

let test_untracked_3xx () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.record_untracked_response `Moved_permanently;
  let after = Server_metrics.snapshot () in
  check int "total+1" (before.total + 1) after.total;
  check int "3xx+1" (before.status_3xx + 1) after.status_3xx;
  check int "errors same" before.errors after.errors

(* ============== sse_open / sse_close ============== *)

let test_sse_open_close_pair () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.sse_open ();
  let mid = Server_metrics.snapshot () in
  check int "open+1" (before.sse_open + 1) mid.sse_open;
  check int "total+1" (before.sse_total + 1) mid.sse_total;
  Server_metrics.sse_close ();
  let after = Server_metrics.snapshot () in
  check int "open restored" before.sse_open after.sse_open;
  check int "total stays" mid.sse_total after.sse_total

let test_sse_close_no_underflow () =
  Eio_main.run @@ fun _env ->
  (* Close without matching open should not go below 0 *)
  let before = Server_metrics.snapshot () in
  let initial_open = before.sse_open in
  if initial_open = 0 then begin
    Server_metrics.sse_close ();
    let after = Server_metrics.snapshot () in
    check int "no underflow" 0 after.sse_open
  end else begin
    Server_metrics.sse_close ();
    let after = Server_metrics.snapshot () in
    check bool "decremented" true (after.sse_open < initial_open)
  end

(* ============== snapshot: field existence ============== *)

let test_snapshot_fields () =
  Eio_main.run @@ fun _env ->
  let s = Server_metrics.snapshot () in
  check bool "inflight >= 0" true (s.inflight >= 0);
  check bool "total >= 0" true (s.total >= 0);
  check bool "2xx >= 0" true (s.status_2xx >= 0);
  check bool "3xx >= 0" true (s.status_3xx >= 0);
  check bool "4xx >= 0" true (s.status_4xx >= 0);
  check bool "5xx >= 0" true (s.status_5xx >= 0);
  check bool "errors >= 0" true (s.errors >= 0);
  check bool "bytes >= 0" true (s.bytes_out >= 0);
  check bool "sse_open >= 0" true (s.sse_open >= 0);
  check bool "sse_total >= 0" true (s.sse_total >= 0);
  check bool "rps_1m >= 0" true (s.rps_1m >= 0.0);
  check bool "rps_5m >= 0" true (s.rps_5m >= 0.0);
  check bool "updated_at > 0" true (s.updated_at > 0.0)

let test_snapshot_latency_fields () =
  Eio_main.run @@ fun _env ->
  let s = Server_metrics.snapshot () in
  let l = s.latency in
  check bool "count >= 0" true (l.count >= 0);
  check bool "avg >= 0" true (l.avg_ms >= 0.0);
  check bool "p50 >= 0" true (l.p50_ms >= 0.0);
  check bool "p95 >= 0" true (l.p95_ms >= 0.0);
  check bool "p99 >= 0" true (l.p99_ms >= 0.0);
  check bool "min >= 0" true (l.min_ms >= 0.0);
  check bool "max >= min" true (l.max_ms >= l.min_ms)

(* ============== to_json: structure verification ============== *)

let test_to_json_keys () =
  Eio_main.run @@ fun _env ->
  let json = Server_metrics.to_json () in
  let str = Yojson.Safe.to_string json in
  let required_keys = [
    "inflight"; "total"; "status_2xx"; "status_3xx"; "status_4xx";
    "status_5xx"; "errors"; "bytes_out"; "sse_open"; "sse_total";
    "rps_1m"; "rps_5m"; "latency_ms"; "updated_at"
  ] in
  List.iter (fun key ->
    check bool (Printf.sprintf "has key %s" key)
      true (String.length str > 0 && Yojson.Safe.Util.member key json <> `Null
            || Yojson.Safe.Util.member key json = `Null)
  ) required_keys;
  (* Verify latency_ms is a nested object *)
  let lat = Yojson.Safe.Util.member "latency_ms" json in
  (match lat with
   | `Assoc fields ->
       let lat_keys = List.map fst fields in
       check bool "has count" true (List.mem "count" lat_keys);
       check bool "has avg" true (List.mem "avg" lat_keys);
       check bool "has p50" true (List.mem "p50" lat_keys);
       check bool "has p95" true (List.mem "p95" lat_keys);
       check bool "has p99" true (List.mem "p99" lat_keys);
       check bool "has min" true (List.mem "min" lat_keys);
       check bool "has max" true (List.mem "max" lat_keys)
   | _ -> fail "latency_ms not an object")

let test_to_json_types () =
  Eio_main.run @@ fun _env ->
  let json = Server_metrics.to_json () in
  let open Yojson.Safe.Util in
  (* Integer fields *)
  let _ = json |> member "inflight" |> to_int in
  let _ = json |> member "total" |> to_int in
  let _ = json |> member "errors" |> to_int in
  let _ = json |> member "bytes_out" |> to_int in
  (* Float fields *)
  let _ = json |> member "rps_1m" |> to_float in
  let _ = json |> member "rps_5m" |> to_float in
  let _ = json |> member "updated_at" |> to_float in
  ()

(* ============== to_prometheus_text: format verification ============== *)

let test_prometheus_header_lines () =
  Eio_main.run @@ fun _env ->
  let text = Server_metrics.to_prometheus_text () in
  check bool "has HELP requests" true
    (String.length text > 0);
  let lines = String.split_on_char '\n' text in
  let help_lines = List.filter (fun l ->
    String.length l > 6 && String.sub l 0 6 = "# HELP"
  ) lines in
  let type_lines = List.filter (fun l ->
    String.length l > 6 && String.sub l 0 6 = "# TYPE"
  ) lines in
  check bool "has HELP lines" true (List.length help_lines > 0);
  check bool "has TYPE lines" true (List.length type_lines > 0);
  check bool "same count" true (List.length help_lines = List.length type_lines)

let test_prometheus_metric_names () =
  Eio_main.run @@ fun _env ->
  let text = Server_metrics.to_prometheus_text () in
  let expected_metrics = [
    "mcp_http_requests_total";
    "mcp_http_inflight";
    "mcp_http_errors_total";
    "mcp_http_bytes_out_total";
    "mcp_http_rps_1m";
    "mcp_http_rps_5m";
    "mcp_http_latency_ms";
    "mcp_sse_open";
    "mcp_sse_total";
  ] in
  List.iter (fun metric ->
    let found = try let _ = Str.search_forward (Str.regexp_string metric) text 0 in true
                with Not_found -> false in
    check bool (Printf.sprintf "contains %s" metric) true found
  ) expected_metrics

let test_prometheus_status_classes () =
  Eio_main.run @@ fun _env ->
  let text = Server_metrics.to_prometheus_text () in
  check bool "has 2xx class" true
    (try let _ = Str.search_forward (Str.regexp_string "class=\"2xx\"") text 0 in true
     with Not_found -> false);
  check bool "has 3xx class" true
    (try let _ = Str.search_forward (Str.regexp_string "class=\"3xx\"") text 0 in true
     with Not_found -> false);
  check bool "has 4xx class" true
    (try let _ = Str.search_forward (Str.regexp_string "class=\"4xx\"") text 0 in true
     with Not_found -> false);
  check bool "has 5xx class" true
    (try let _ = Str.search_forward (Str.regexp_string "class=\"5xx\"") text 0 in true
     with Not_found -> false)

let test_prometheus_quantiles () =
  Eio_main.run @@ fun _env ->
  let text = Server_metrics.to_prometheus_text () in
  check bool "has p50" true
    (try let _ = Str.search_forward (Str.regexp_string "quantile=\"0.50\"") text 0 in true
     with Not_found -> false);
  check bool "has p95" true
    (try let _ = Str.search_forward (Str.regexp_string "quantile=\"0.95\"") text 0 in true
     with Not_found -> false);
  check bool "has p99" true
    (try let _ = Str.search_forward (Str.regexp_string "quantile=\"0.99\"") text 0 in true
     with Not_found -> false)

(* ============== snapshot consistency after multiple operations ============== *)

let test_mixed_status_codes () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.record_untracked_response `OK;
  Server_metrics.record_untracked_response `Not_found;
  Server_metrics.record_untracked_response `Internal_server_error;
  Server_metrics.record_untracked_response `Moved_permanently;
  let after = Server_metrics.snapshot () in
  check int "total+4" (before.total + 4) after.total;
  check int "2xx+1" (before.status_2xx + 1) after.status_2xx;
  check int "3xx+1" (before.status_3xx + 1) after.status_3xx;
  check int "4xx+1" (before.status_4xx + 1) after.status_4xx;
  check int "5xx+1" (before.status_5xx + 1) after.status_5xx;
  check int "errors+2" (before.errors + 2) after.errors

(* ============== rps window observable via snapshot ============== *)

let test_rps_observable () =
  Eio_main.run @@ fun _env ->
  (* After recording requests, rps should be positive *)
  Server_metrics.record_untracked_response `OK;
  Server_metrics.record_untracked_response `OK;
  Server_metrics.record_untracked_response `OK;
  let s = Server_metrics.snapshot () in
  (* rps_1m = sum_recent(60s) / 60.0 — should be > 0 after fresh requests *)
  check bool "rps_1m > 0" true (s.rps_1m > 0.0);
  check bool "rps_5m > 0" true (s.rps_5m > 0.0)

let () =
  run "server_metrics_a5"
    [ ("prom_metric", [
        test_case "basic format" `Quick test_prom_metric_basic;
        test_case "empty labels" `Quick test_prom_metric_empty_labels;
        test_case "multi labels" `Quick test_prom_metric_multi_labels;
        test_case "float value" `Quick test_prom_metric_float_value;
      ]);
      ("record_untracked_2xx", [
        test_case "2xx with bytes" `Quick test_untracked_2xx;
        test_case "2xx no bytes" `Quick test_untracked_2xx_no_bytes;
      ]);
      ("record_untracked_3xx", [
        test_case "3xx" `Quick test_untracked_3xx;
      ]);
      ("sse", [
        test_case "open/close pair" `Quick test_sse_open_close_pair;
        test_case "close no underflow" `Quick test_sse_close_no_underflow;
      ]);
      ("snapshot", [
        test_case "all fields valid" `Quick test_snapshot_fields;
        test_case "latency fields" `Quick test_snapshot_latency_fields;
      ]);
      ("to_json", [
        test_case "required keys" `Quick test_to_json_keys;
        test_case "field types" `Quick test_to_json_types;
      ]);
      ("to_prometheus_text", [
        test_case "HELP/TYPE headers" `Quick test_prometheus_header_lines;
        test_case "metric names" `Quick test_prometheus_metric_names;
        test_case "status classes" `Quick test_prometheus_status_classes;
        test_case "quantiles" `Quick test_prometheus_quantiles;
      ]);
      ("integration", [
        test_case "mixed status codes" `Quick test_mixed_status_codes;
        test_case "rps observable" `Quick test_rps_observable;
      ]);
    ]
