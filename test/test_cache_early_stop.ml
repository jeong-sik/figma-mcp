(** 캐시 및 Early Stop 테스트 *)

open Alcotest

(** ============== Cache 테스트 ============== *)

let test_cache_key_generation () =
  let key1 = Figma_cache.make_cache_key
    ~file_key:"ABC123"
    ~node_id:"100:200"
    ~options:[] in
  let key2 = Figma_cache.make_cache_key
    ~file_key:"ABC123"
    ~node_id:"100:200"
    ~options:[] in
  let key3 = Figma_cache.make_cache_key
    ~file_key:"ABC123"
    ~node_id:"100:201"  (* 다른 노드 *)
    ~options:[] in
  check string "same inputs = same key" key1 key2;
  check bool "different node = different key" true (key1 <> key3)

let test_cache_key_with_options () =
  let key1 = Figma_cache.make_cache_key
    ~file_key:"ABC123"
    ~node_id:"100:200"
    ~options:["depth:5"; "geometry:paths"] in
  let key2 = Figma_cache.make_cache_key
    ~file_key:"ABC123"
    ~node_id:"100:200"
    ~options:["geometry:paths"; "depth:5"] in  (* 순서 다름 *)
  check string "options order independent" key1 key2

let test_cache_set_get () =
  (* 테스트 전 캐시 초기화 *)
  Figma_cache.invalidate ();

  let test_payload = `Assoc [("test", `String "value")] in

  (* 캐시에 저장 *)
  Figma_cache.set
    ~file_key:"TEST_FILE"
    ~node_id:"1:1"
    test_payload;

  (* 캐시에서 조회 *)
  let result = Figma_cache.get
    ~file_key:"TEST_FILE"
    ~node_id:"1:1"
    () in

  check bool "cache hit" true (Option.is_some result);
  match result with
  | Some json ->
    let value = Yojson.Safe.Util.(member "test" json |> to_string) in
    check string "correct value" "value" value
  | None -> fail "expected cache hit"

let test_cache_miss () =
  (* 테스트 전 캐시 초기화 *)
  Figma_cache.invalidate ();

  let result = Figma_cache.get
    ~file_key:"NONEXISTENT"
    ~node_id:"999:999"
    () in

  check bool "cache miss" true (Option.is_none result)

let test_cache_invalidate_all () =
  Figma_cache.invalidate ();

  (* 여러 항목 저장 *)
  Figma_cache.set ~file_key:"F1" ~node_id:"1:1" (`String "a");
  Figma_cache.set ~file_key:"F2" ~node_id:"2:2" (`String "b");

  (* 전체 무효화 *)
  Figma_cache.invalidate ();

  let r1 = Figma_cache.get ~file_key:"F1" ~node_id:"1:1" () in
  let r2 = Figma_cache.get ~file_key:"F2" ~node_id:"2:2" () in

  check bool "all invalidated" true (Option.is_none r1 && Option.is_none r2)

let test_cache_stats () =
  Figma_cache.invalidate ();

  Figma_cache.set ~file_key:"F1" ~node_id:"1:1" (`String "test");

  let stats = Figma_cache.stats () in
  let l1_count = Yojson.Safe.Util.(member "l1_entries" stats |> to_int) in

  check bool "stats shows entries" true (l1_count >= 1)

let cache_tests = [
  "key generation", `Quick, test_cache_key_generation;
  "key with options", `Quick, test_cache_key_with_options;
  "set and get", `Quick, test_cache_set_get;
  "cache miss", `Quick, test_cache_miss;
  "invalidate all", `Quick, test_cache_invalidate_all;
  "cache stats", `Quick, test_cache_stats;
]

(** ============== Early Stop 테스트 ============== *)

let test_early_stop_target_reached () =
  let detector = Figma_early_stop.create () in
  let result = Figma_early_stop.check detector
    ~current_ssim:0.95
    ~iteration:1
    () in

  check bool "should stop" true result.should_stop;
  check bool "target reason" true (result.reason = Figma_early_stop.Target_reached)

let test_early_stop_max_iterations () =
  let config = { Figma_early_stop.default_config with max_iterations = 3 } in
  let detector = Figma_early_stop.create ~config () in

  (* 3회까지 진행 *)
  let _ = Figma_early_stop.check detector ~current_ssim:0.70 ~iteration:1 () in
  let _ = Figma_early_stop.check detector ~current_ssim:0.75 ~iteration:2 () in
  let result = Figma_early_stop.check detector ~current_ssim:0.80 ~iteration:3 () in

  check bool "should stop at max" true result.should_stop;
  check bool "max_iterations reason" true (result.reason = Figma_early_stop.Max_iterations)

let test_early_stop_plateau () =
  let config = {
    Figma_early_stop.default_config with
    plateau_patience = 3;
    plateau_threshold = 0.01;  (* 1% *)
  } in
  let detector = Figma_early_stop.create ~config () in

  (* 정체 시뮬레이션: 4회 연속 <1% 개선 *)
  let _ = Figma_early_stop.check detector ~current_ssim:0.80 ~iteration:1 () in
  let _ = Figma_early_stop.check detector ~current_ssim:0.805 ~iteration:2 () in
  let _ = Figma_early_stop.check detector ~current_ssim:0.810 ~iteration:3 () in
  let result = Figma_early_stop.check detector ~current_ssim:0.815 ~iteration:4 () in

  check bool "plateau detected" true result.should_stop;
  check bool "plateau reason" true (result.reason = Figma_early_stop.Plateau)

let test_early_stop_continue () =
  let detector = Figma_early_stop.create () in
  let result = Figma_early_stop.check detector
    ~current_ssim:0.60
    ~iteration:1
    () in

  check bool "should continue" false result.should_stop;
  check bool "continue reason" true (result.reason = Figma_early_stop.Continue)

let test_early_stop_regression () =
  let detector = Figma_early_stop.create () in

  (* SSIM 상승 후 하락 *)
  let _ = Figma_early_stop.check detector ~current_ssim:0.80 ~iteration:1 () in
  let result = Figma_early_stop.check detector ~current_ssim:0.75 ~iteration:2 () in

  check bool "regression detected" true (result.reason = Figma_early_stop.Regression);
  (* Regression은 경고만, 종료는 안함 *)
  check bool "should not stop on regression" false result.should_stop

let test_early_stop_text_ceiling () =
  let config = {
    Figma_early_stop.default_config with
    text_ceiling = 0.88;
  } in
  let detector = Figma_early_stop.create ~config () in

  (* 텍스트 밀도 높은 UI에서 천장 도달 *)
  let result = Figma_early_stop.check detector
    ~current_ssim:0.89
    ~iteration:1
    ~text_density:0.5  (* 50% 텍스트 *)
    () in

  check bool "text ceiling hit" true result.should_stop;
  check bool "text ceiling reason" true (result.reason = Figma_early_stop.Text_ceiling)

let test_early_stop_summary () =
  let detector = Figma_early_stop.create () in

  let _ = Figma_early_stop.check detector ~current_ssim:0.70 ~iteration:1 () in
  let _ = Figma_early_stop.check detector ~current_ssim:0.80 ~iteration:2 () in
  let _ = Figma_early_stop.check detector ~current_ssim:0.85 ~iteration:3 () in

  let summary = Figma_early_stop.summary detector in

  check bool "summary contains iteration count" true
    (String.length summary > 0);
  check bool "summary mentions best ssim" true
    (Str.string_match (Str.regexp ".*Best.*") summary 0)

let test_early_stop_reset () =
  let detector = Figma_early_stop.create () in

  let _ = Figma_early_stop.check detector ~current_ssim:0.80 ~iteration:1 () in
  Figma_early_stop.reset detector;

  (* 리셋 후 상태 확인 *)
  let result = Figma_early_stop.check detector ~current_ssim:0.60 ~iteration:1 () in

  check bool "reset clears history" true (result.reason = Figma_early_stop.Continue)

let early_stop_tests = [
  "target reached", `Quick, test_early_stop_target_reached;
  "max iterations", `Quick, test_early_stop_max_iterations;
  "plateau detection", `Quick, test_early_stop_plateau;
  "continue", `Quick, test_early_stop_continue;
  "regression warning", `Quick, test_early_stop_regression;
  "text ceiling", `Quick, test_early_stop_text_ceiling;
  "summary", `Quick, test_early_stop_summary;
  "reset", `Quick, test_early_stop_reset;
]

(** ============== 메인 ============== *)

let () =
  run "Cache & Early Stop" [
    "Cache", cache_tests;
    "Early Stop", early_stop_tests;
  ]
