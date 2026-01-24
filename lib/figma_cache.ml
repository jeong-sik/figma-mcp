(** Figma Node Cache - 계층적 캐시 시스템 (L1 메모리 + L2 파일)

    API 호출 절감: 22회 반복 × 15초/API = ~5.5분 → 캐시 HIT 시 즉시 응답
    예상 효과: 반복당 ~5분 절감 (25% 시간 단축)
*)

open Printf

(** 캐시 설정 - Figma_config에서 가져옴 *)
module Config = struct
  let cache_dir = Figma_config.Cache.dir
  let ttl_hours = Figma_config.Cache.ttl_hours
  let ttl_variables_hours = Figma_config.Cache.ttl_variables_hours
  let max_l1_entries = Figma_config.Cache.l1_max
  let l2_max_mb = Figma_config.Cache.l2_max_mb
  let l2_max_bytes = l2_max_mb * 1024 * 1024
end

(** 캐시 키 생성 *)
let make_cache_key ~file_key ~node_id ~options =
  let options_str = match options with
    | [] -> ""
    | opts -> String.concat ":" (List.sort String.compare opts)
  in
  let content = sprintf "%s:%s:%s" file_key node_id options_str in
  (* MD5 해시의 앞 16자리 *)
  Digest.string content |> Digest.to_hex |> fun s -> String.sub s 0 16

(** 파일 시스템 유틸리티 *)
module FS = struct
  let ensure_dir path =
    if not (Sys.file_exists path) then
      Unix.mkdir path 0o755

  let read_file path =
    if Sys.file_exists path then
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      Some s
    else None

  let write_file path content =
    ensure_dir Config.cache_dir;
    let oc = open_out path in
    output_string oc content;
    close_out oc

  let touch_file path =
    if Sys.file_exists path then
      let now = Unix.gettimeofday () in
      try Unix.utimes path now now with _ -> ()

  let delete_file path =
    if Sys.file_exists path then
      Sys.remove path

  let file_age_hours path =
    if Sys.file_exists path then
      let stat = Unix.stat path in
      let now = Unix.gettimeofday () in
      (now -. stat.Unix.st_mtime) /. 3600.0
    else infinity

  let list_cache_files () =
    if Sys.file_exists Config.cache_dir then
      Sys.readdir Config.cache_dir
      |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".json")
      |> List.map (fun f ->
           let path = Filename.concat Config.cache_dir f in
           let stat = Unix.stat path in
           (path, stat.Unix.st_mtime, stat.Unix.st_size))
    else []
end

(** 캐시 엔트리 타입 *)
type cache_entry = {
  payload: Yojson.Safe.t;
  cached_at: float;  (* Unix timestamp *)
  last_access: float;
  file_key: string;
  node_id: string;
}

(** L1 메모리 캐시 (Hashtbl 기반) *)
let memory_cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 64

(** L1 캐시 용량 제한 (LRU 기반) *)
let enforce_l1_limit () =
  let max_entries = Config.max_l1_entries in
  let size = Hashtbl.length memory_cache in
  if size > max_entries then begin
    let entries =
      Hashtbl.fold (fun key entry acc -> (key, entry) :: acc) memory_cache []
    in
    let sorted =
      List.sort (fun (_, a) (_, b) -> compare a.last_access b.last_access) entries
    in
    let to_remove = size - max_entries in
    let rec drop n = function
      | [] -> ()
      | (key, _) :: rest ->
          if n > 0 then begin
            Hashtbl.remove memory_cache key;
            drop (n - 1) rest
          end
    in
    drop to_remove sorted
  end

(** L2 파일 캐시 용량 제한 (오래된 파일부터 삭제) *)
let enforce_l2_limit () =
  let files = FS.list_cache_files () in
  let total =
    List.fold_left (fun acc (_, _, size) -> acc + size) 0 files
  in
  if total > Config.l2_max_bytes then begin
    let sorted = List.sort (fun (_, a, _) (_, b, _) -> compare a b) files in
    let rec remove_until current = function
      | [] -> ()
      | (path, _, size) :: rest ->
          if current > Config.l2_max_bytes then begin
            FS.delete_file path;
            remove_until (current - size) rest
          end
    in
    remove_until total sorted
  end

let cached_at_of_json ~fallback json =
  match Yojson.Safe.Util.member "_cached_at" json with
  | `Float ts -> ts
  | `Int ts -> float_of_int ts
  | _ -> fallback

(** ============== Smart Context Caching (Early Declaration) ============== *)

(** Hit rate tracking for cache effectiveness *)
module Stats = struct
  let hits = ref 0
  let misses = ref 0
  let l1_hits = ref 0
  let l2_hits = ref 0
  let prefetch_hits = ref 0
  let bytes_saved = ref 0

  let record_hit ?(level=`L1) ?(bytes=0) () =
    incr hits;
    bytes_saved := !bytes_saved + bytes;
    match level with
    | `L1 -> incr l1_hits
    | `L2 -> incr l2_hits
    | `Prefetch -> incr prefetch_hits

  let record_miss () = incr misses

  let hit_rate () =
    let total = !hits + !misses in
    if total = 0 then 0.0 else float_of_int !hits /. float_of_int total *. 100.0

  let reset () =
    hits := 0; misses := 0; l1_hits := 0; l2_hits := 0;
    prefetch_hits := 0; bytes_saved := 0
end

(** Prefetch hint system - remember access patterns *)
module PrefetchHints = struct
  (* node_id -> list of frequently co-accessed node_ids *)
  let patterns : (string, string list) Hashtbl.t = Hashtbl.create 64

  (* Recent access chain for pattern detection *)
  let recent_accesses = ref []
  let max_recent = 10

  let record_access ~node_id =
    recent_accesses := node_id :: (List.filter ((<>) node_id) !recent_accesses);
    if List.length !recent_accesses > max_recent then
      recent_accesses := List.filteri (fun i _ -> i < max_recent) !recent_accesses

  let learn_pattern ~from_node ~to_node =
    let existing = Option.value ~default:[] (Hashtbl.find_opt patterns from_node) in
    if not (List.mem to_node existing) then begin
      let updated = to_node :: (List.filteri (fun i _ -> i < 5) existing) in
      Hashtbl.replace patterns from_node updated
    end

  let analyze_chain () =
    match !recent_accesses with
    | [] | [_] -> ()
    | current :: rest ->
        List.iter (fun prev -> learn_pattern ~from_node:prev ~to_node:current) rest

  let get_hints ~node_id =
    analyze_chain ();
    Option.value ~default:[] (Hashtbl.find_opt patterns node_id)

  let get_top_patterns ?(limit=10) () =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) patterns []
    |> List.sort (fun (_, a) (_, b) -> compare (List.length b) (List.length a))
    |> List.filteri (fun i _ -> i < limit)
end

(** File version tracking for smart invalidation - forward declaration *)
(* Note: VersionTracker is defined after invalidate() to avoid forward reference *)
let version_tracker_versions : (string, float) Hashtbl.t = Hashtbl.create 16

(** 캐시 조회 (L1 → L2 순서, with Smart Stats tracking) *)
let get ~file_key ~node_id ?(options=[]) ?(ttl_hours=Config.ttl_hours) () =
  let key = make_cache_key ~file_key ~node_id ~options in

  (* L1: 메모리 캐시 *)
  match Hashtbl.find_opt memory_cache key with
  | Some entry ->
    let age = (Unix.gettimeofday () -. entry.cached_at) /. 3600.0 in
    if age < ttl_hours then (
      let payload_size = String.length (Yojson.Safe.to_string entry.payload) in
      Stats.record_hit ~level:`L1 ~bytes:payload_size ();
      eprintf "[Cache] L1 HIT: %s (age: %.1fh, rate: %.1f%%)\n%!" node_id age (Stats.hit_rate ());
      let now = Unix.gettimeofday () in
      Hashtbl.replace memory_cache key { entry with last_access = now };
      Some entry.payload
    ) else (
      Hashtbl.remove memory_cache key;
      Stats.record_miss ();
      None
    )
  | None ->
    (* L2: 파일 캐시 *)
    let cache_file = sprintf "%s/%s.json" Config.cache_dir key in
    match FS.read_file cache_file with
    | None ->
        Stats.record_miss ();
        None
    | Some content ->
        let now = Unix.gettimeofday () in
        (match Yojson.Safe.from_string content with
         | json ->
             let cached_at =
               cached_at_of_json ~fallback:(now -. (FS.file_age_hours cache_file *. 3600.0)) json
             in
             let age = (now -. cached_at) /. 3600.0 in
             if age < ttl_hours then (
               let payload = Yojson.Safe.Util.member "payload" json in
               let payload_size = String.length content in
               Stats.record_hit ~level:`L2 ~bytes:payload_size ();
               let entry = {
                 payload;
                 cached_at;
                 last_access = now;
                 file_key;
                 node_id;
               } in
               (* L1에 승격 *)
               Hashtbl.replace memory_cache key entry;
               enforce_l1_limit ();
               FS.touch_file cache_file;
               eprintf "[Cache] L2 HIT → L1: %s (age: %.1fh, rate: %.1f%%)\n%!" node_id age (Stats.hit_rate ());
               Some payload
             ) else (
               Stats.record_miss ();
               FS.delete_file cache_file;  (* 만료된 캐시 삭제 *)
               None
             )
         | exception _ ->
             Stats.record_miss ();
             FS.delete_file cache_file;
             None)

(** 캐시 저장 (L1 + L2 동시, with Prefetch pattern learning) *)
let set ~file_key ~node_id ?(options=[]) payload =
  let key = make_cache_key ~file_key ~node_id ~options in
  let now = Unix.gettimeofday () in

  (* L1: 메모리 *)
  let entry = { payload; cached_at = now; last_access = now; file_key; node_id } in
  Hashtbl.replace memory_cache key entry;
  enforce_l1_limit ();

  (* L2: 파일 *)
  let cache_json = `Assoc [
    ("payload", payload);
    ("_cached_at", `Float now);
    ("_last_access", `Float now);
    ("_file_key", `String file_key);
    ("_node_id", `String node_id);
  ] in
  let cache_file = sprintf "%s/%s.json" Config.cache_dir key in
  FS.write_file cache_file (Yojson.Safe.to_string cache_json);
  enforce_l2_limit ();
  eprintf "[Cache] SET: %s → %s\n%!" node_id key

(** Record access for prefetch pattern learning - call when accessing a node *)
let record_access ~node_id =
  PrefetchHints.record_access ~node_id

(** 캐시 무효화 *)
let should_invalidate_entry ~file_key ~node_id entry =
  entry.file_key = file_key
  && match node_id with
     | None -> true
     | Some nid -> entry.node_id = nid

let should_invalidate_json ~file_key ~node_id json =
  match Yojson.Safe.Util.member "_file_key" json with
  | `String fk when fk = file_key ->
      (match node_id with
       | None -> true
       | Some nid ->
           (match Yojson.Safe.Util.member "_node_id" json with
            | `String nid' -> nid = nid'
            | _ -> false))
  | _ -> false

let invalidate ?file_key ?node_id () =
  match file_key with
  | None ->
    (* 전체 삭제 *)
    Hashtbl.clear memory_cache;
    if Sys.file_exists Config.cache_dir then
      Array.iter (fun f ->
        if Filename.check_suffix f ".json" then
          FS.delete_file (Filename.concat Config.cache_dir f)
    ) (Sys.readdir Config.cache_dir);
    eprintf "[Cache] INVALIDATE ALL\n%!"
  | Some fk ->
    (* 특정 파일/노드 삭제 *)
    Hashtbl.filter_map_inplace (fun _ v ->
      if should_invalidate_entry ~file_key:fk ~node_id v then None else Some v
    ) memory_cache;
    FS.list_cache_files ()
    |> List.iter (fun (path, _, _) ->
         match FS.read_file path with
         | None -> ()
         | Some content ->
             (match Yojson.Safe.from_string content with
              | json ->
                  if should_invalidate_json ~file_key:fk ~node_id json then
                    FS.delete_file path
              | exception _ -> ()));
    eprintf "[Cache] INVALIDATE: %s/%s\n%!" fk (Option.value node_id ~default:"*")

(** ============== Version Tracking (after invalidate is defined) ============== *)

(** File version tracking for smart invalidation *)
module VersionTracker = struct
  let versions = version_tracker_versions

  let update_version ~file_key ~version =
    Hashtbl.replace versions file_key version;
    eprintf "[Cache] Version updated: %s → %.0f\n%!" file_key version

  let check_version ~file_key ~current_version =
    match Hashtbl.find_opt versions file_key with
    | Some cached_ver when cached_ver < current_version ->
        (* File updated, invalidate all cache for this file *)
        eprintf "[Cache] Version mismatch: %s (cached: %.0f, current: %.0f) → invalidating\n%!"
          file_key cached_ver current_version;
        invalidate ~file_key ();
        Hashtbl.replace versions file_key current_version;
        `Invalidated
    | Some _ -> `Valid
    | None ->
        Hashtbl.replace versions file_key current_version;
        `NewFile

  let get_version ~file_key =
    Hashtbl.find_opt versions file_key
end

(** 캐시 통계 (Enhanced with smart tracking) *)
let stats () =
  let l1_count = Hashtbl.length memory_cache in
  let l2_files = FS.list_cache_files () in
  let l2_count = List.length l2_files in
  let l2_bytes = List.fold_left (fun acc (_, _, size) -> acc + size) 0 l2_files in
  let top_patterns = PrefetchHints.get_top_patterns ~limit:5 () in
  `Assoc [
    ("l1_entries", `Int l1_count);
    ("l2_entries", `Int l2_count);
    ("l1_max_entries", `Int Config.max_l1_entries);
    ("l2_max_bytes", `Int Config.l2_max_bytes);
    ("l2_bytes", `Int l2_bytes);
    ("cache_dir", `String Config.cache_dir);
    ("ttl_hours", `Float Config.ttl_hours);
    ("ttl_variables_hours", `Float Config.ttl_variables_hours);
    (* Smart caching stats *)
    ("hit_rate_percent", `Float (Stats.hit_rate ()));
    ("total_hits", `Int !(Stats.hits));
    ("total_misses", `Int !(Stats.misses));
    ("l1_hits", `Int !(Stats.l1_hits));
    ("l2_hits", `Int !(Stats.l2_hits));
    ("prefetch_hits", `Int !(Stats.prefetch_hits));
    ("bytes_saved", `Int !(Stats.bytes_saved));
    ("tracked_versions", `Int (Hashtbl.length VersionTracker.versions));
    ("learned_patterns", `Int (Hashtbl.length PrefetchHints.patterns));
    ("top_patterns", `List (List.map (fun (k, v) ->
        `Assoc [("node", `String k); ("hints", `List (List.map (fun s -> `String s) v))]
      ) top_patterns));
  ]

(** 캐시 래핑 함수 - API 호출을 캐시로 감싸기 *)
let with_cache ~file_key ~node_id ?(options=[]) ?(ttl_hours=Config.ttl_hours) fetch_fn =
  match get ~file_key ~node_id ~options ~ttl_hours () with
  | Some cached -> cached
  | None ->
    eprintf "[Cache] MISS: %s → fetching...\n%!" node_id;
    let result = fetch_fn () in
    set ~file_key ~node_id ~options result;
    result
