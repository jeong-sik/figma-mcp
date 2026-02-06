open Printf

type request_info = {
  started_at: float;
}

type bucket = {
  mutable sec: int;
  mutable count: int;
}

type latency_snapshot = {
  count: int;
  avg_ms: float;
  p50_ms: float;
  p95_ms: float;
  p99_ms: float;
  min_ms: float;
  max_ms: float;
}

type snapshot = {
  inflight: int;
  total: int;
  status_2xx: int;
  status_3xx: int;
  status_4xx: int;
  status_5xx: int;
  errors: int;
  bytes_out: int;
  sse_open: int;
  sse_total: int;
  rps_1m: float;
  rps_5m: float;
  latency: latency_snapshot;
  updated_at: float;
}

let ring_size = 256

let state_mutex = Eio.Mutex.create ()

(* Use physical equality to avoid polymorphic compare on Httpun.Reqd.t (contains functions). *)
module ReqdTbl = Hashtbl.Make(struct
  type t = Httpun.Reqd.t
  let equal = (==)
  let hash = Hashtbl.hash
end)

let requests : request_info ReqdTbl.t = ReqdTbl.create 1024

let latency_ring = Array.make ring_size 0.0
let latency_index = ref 0
let latency_count = ref 0

let rps_1m = Array.init 60 (fun _ -> { sec = 0; count = 0 })
let rps_5m = Array.init 300 (fun _ -> { sec = 0; count = 0 })

let inflight = ref 0
let total = ref 0
let status_2xx = ref 0
let status_3xx = ref 0
let status_4xx = ref 0
let status_5xx = ref 0
let errors = ref 0
let bytes_out = ref 0
let sse_open_ref = ref 0
let sse_total = ref 0

let now () = Unix.gettimeofday ()

let with_lock f = Eio.Mutex.use_rw ~protect:true state_mutex f

let record_rps buckets now_sec =
  let idx = now_sec mod Array.length buckets in
  let bucket = buckets.(idx) in
  if bucket.sec <> now_sec then begin
    bucket.sec <- now_sec;
    bucket.count <- 0
  end;
  bucket.count <- bucket.count + 1

let sum_recent buckets now_sec window =
  Array.fold_left (fun acc b ->
    if now_sec - b.sec < window then acc + b.count else acc
  ) 0 buckets

let record_latency ms =
  latency_ring.(!latency_index) <- ms;
  latency_index := (!latency_index + 1) mod ring_size;
  if !latency_count < ring_size then latency_count := !latency_count + 1

let latency_snapshot () =
  let count = !latency_count in
  if count = 0 then
    { count = 0; avg_ms = 0.0; p50_ms = 0.0; p95_ms = 0.0; p99_ms = 0.0; min_ms = 0.0; max_ms = 0.0 }
  else
    let arr = Array.init count (fun i -> latency_ring.(i)) in
    Array.sort compare arr;
    let idx p =
      let f = p *. float_of_int (count - 1) in
      int_of_float f
    in
    let sum = Array.fold_left ( +. ) 0.0 arr in
    let min_ms = arr.(0) in
    let max_ms = arr.(count - 1) in
    { count;
      avg_ms = sum /. float_of_int count;
      p50_ms = arr.(idx 0.50);
      p95_ms = arr.(idx 0.95);
      p99_ms = arr.(idx 0.99);
      min_ms;
      max_ms;
    }

let register_reqd reqd _request =
  let t = now () in
  with_lock (fun () ->
    if not (ReqdTbl.mem requests reqd) then begin
      ReqdTbl.add requests reqd { started_at = t };
      inflight := !inflight + 1
    end
  )

let finish_reqd ?bytes reqd status =
  let t = now () in
  let code = Httpun.Status.to_code status in
  with_lock (fun () ->
    let started = match ReqdTbl.find_opt requests reqd with
      | Some info ->
          ReqdTbl.remove requests reqd;
          if !inflight > 0 then inflight := !inflight - 1;
          Some info.started_at
      | None -> None
    in
    total := !total + 1;
    if code >= 200 && code < 300 then status_2xx := !status_2xx + 1
    else if code >= 300 && code < 400 then status_3xx := !status_3xx + 1
    else if code >= 400 && code < 500 then begin
      status_4xx := !status_4xx + 1;
      errors := !errors + 1
    end else if code >= 500 && code < 600 then begin
      status_5xx := !status_5xx + 1;
      errors := !errors + 1
    end;
    (match bytes with
     | Some n when n > 0 -> bytes_out := !bytes_out + n
     | _ -> ());
    let now_sec = int_of_float t in
    record_rps rps_1m now_sec;
    record_rps rps_5m now_sec;
    (match started with
     | Some s -> record_latency ((t -. s) *. 1000.0)
     | None -> ())
  )

(* Record a response when we don't have a tracked reqd (e.g., low-level httpun errors). *)
let record_untracked_response ?(bytes=0) status =
  let t = now () in
  let code = Httpun.Status.to_code status in
  with_lock (fun () ->
    total := !total + 1;
    if code >= 200 && code < 300 then status_2xx := !status_2xx + 1
    else if code >= 300 && code < 400 then status_3xx := !status_3xx + 1
    else if code >= 400 && code < 500 then begin
      status_4xx := !status_4xx + 1;
      errors := !errors + 1
    end else if code >= 500 && code < 600 then begin
      status_5xx := !status_5xx + 1;
      errors := !errors + 1
    end;
    if bytes > 0 then bytes_out := !bytes_out + bytes;
    let now_sec = int_of_float t in
    record_rps rps_1m now_sec;
    record_rps rps_5m now_sec
  )

let sse_open () =
  with_lock (fun () ->
    sse_open_ref := !sse_open_ref + 1;
    sse_total := !sse_total + 1
  )

let sse_close () =
  with_lock (fun () ->
    if !sse_open_ref > 0 then sse_open_ref := !sse_open_ref - 1
  )

let snapshot () =
  let t = now () in
  with_lock (fun () ->
    let now_sec = int_of_float t in
    let rps1 = float_of_int (sum_recent rps_1m now_sec 60) /. 60.0 in
    let rps5 = float_of_int (sum_recent rps_5m now_sec 300) /. 300.0 in
    {
      inflight = !inflight;
      total = !total;
      status_2xx = !status_2xx;
      status_3xx = !status_3xx;
      status_4xx = !status_4xx;
      status_5xx = !status_5xx;
      errors = !errors;
      bytes_out = !bytes_out;
      sse_open = !sse_open_ref;
      sse_total = !sse_total;
      rps_1m = rps1;
      rps_5m = rps5;
      latency = latency_snapshot ();
      updated_at = t;
    }
  )

let to_json () =
  let s = snapshot () in
  `Assoc [
    ("inflight", `Int s.inflight);
    ("total", `Int s.total);
    ("status_2xx", `Int s.status_2xx);
    ("status_3xx", `Int s.status_3xx);
    ("status_4xx", `Int s.status_4xx);
    ("status_5xx", `Int s.status_5xx);
    ("errors", `Int s.errors);
    ("bytes_out", `Int s.bytes_out);
    ("sse_open", `Int s.sse_open);
    ("sse_total", `Int s.sse_total);
    ("rps_1m", `Float s.rps_1m);
    ("rps_5m", `Float s.rps_5m);
    ("latency_ms", `Assoc [
      ("count", `Int s.latency.count);
      ("avg", `Float s.latency.avg_ms);
      ("p50", `Float s.latency.p50_ms);
      ("p95", `Float s.latency.p95_ms);
      ("p99", `Float s.latency.p99_ms);
      ("min", `Float s.latency.min_ms);
      ("max", `Float s.latency.max_ms);
    ]);
    ("updated_at", `Float s.updated_at);
  ]

let prom_metric name labels value =
  sprintf "%s{%s} %s\n" name labels value

let to_prometheus_text () =
  let s = snapshot () in
  let labels = sprintf "server=\"%s\"" Mcp_protocol.server_name in
  let header =
    "# HELP mcp_http_requests_total Total HTTP requests\n" ^
    "# TYPE mcp_http_requests_total counter\n" ^
    "# HELP mcp_http_inflight Inflight HTTP requests\n" ^
    "# TYPE mcp_http_inflight gauge\n" ^
    "# HELP mcp_http_errors_total Total HTTP errors\n" ^
    "# TYPE mcp_http_errors_total counter\n" ^
    "# HELP mcp_http_bytes_out_total Total HTTP response bytes\n" ^
    "# TYPE mcp_http_bytes_out_total counter\n" ^
    "# HELP mcp_http_rps_1m Requests per second (1m window)\n" ^
    "# TYPE mcp_http_rps_1m gauge\n" ^
    "# HELP mcp_http_rps_5m Requests per second (5m window)\n" ^
    "# TYPE mcp_http_rps_5m gauge\n" ^
    "# HELP mcp_http_latency_ms Request latency (ms)\n" ^
    "# TYPE mcp_http_latency_ms gauge\n" ^
    "# HELP mcp_sse_open SSE open connections\n" ^
    "# TYPE mcp_sse_open gauge\n" ^
    "# HELP mcp_sse_total SSE total connections\n" ^
    "# TYPE mcp_sse_total counter\n"
  in
  header ^
  prom_metric "mcp_http_inflight" labels (string_of_int s.inflight) ^
  prom_metric "mcp_http_errors_total" labels (string_of_int s.errors) ^
  prom_metric "mcp_http_bytes_out_total" labels (string_of_int s.bytes_out) ^
  prom_metric "mcp_http_rps_1m" labels (sprintf "%.4f" s.rps_1m) ^
  prom_metric "mcp_http_rps_5m" labels (sprintf "%.4f" s.rps_5m) ^
  prom_metric "mcp_sse_open" labels (string_of_int s.sse_open) ^
  prom_metric "mcp_sse_total" labels (string_of_int s.sse_total) ^
  prom_metric "mcp_http_requests_total" (labels ^ ",class=\"2xx\"") (string_of_int s.status_2xx) ^
  prom_metric "mcp_http_requests_total" (labels ^ ",class=\"3xx\"") (string_of_int s.status_3xx) ^
  prom_metric "mcp_http_requests_total" (labels ^ ",class=\"4xx\"") (string_of_int s.status_4xx) ^
  prom_metric "mcp_http_requests_total" (labels ^ ",class=\"5xx\"") (string_of_int s.status_5xx) ^
  prom_metric "mcp_http_latency_ms" (labels ^ ",quantile=\"0.50\"") (sprintf "%.3f" s.latency.p50_ms) ^
  prom_metric "mcp_http_latency_ms" (labels ^ ",quantile=\"0.95\"") (sprintf "%.3f" s.latency.p95_ms) ^
  prom_metric "mcp_http_latency_ms" (labels ^ ",quantile=\"0.99\"") (sprintf "%.3f" s.latency.p99_ms) ^
  prom_metric "mcp_http_latency_ms" (labels ^ ",stat=\"avg\"") (sprintf "%.3f" s.latency.avg_ms) ^
  prom_metric "mcp_http_latency_ms" (labels ^ ",stat=\"min\"") (sprintf "%.3f" s.latency.min_ms) ^
  prom_metric "mcp_http_latency_ms" (labels ^ ",stat=\"max\"") (sprintf "%.3f" s.latency.max_ms)
