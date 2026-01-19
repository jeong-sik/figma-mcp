(** Figma Node Cache - 계층적 캐시 시스템 (L1 메모리 + L2 파일)

    API 호출 절감: 22회 반복 × 15초/API = ~5.5분 → 캐시 HIT 시 즉시 응답
    예상 효과: 반복당 ~5분 절감 (25% 시간 단축)
*)

open Printf

(** 캐시 설정 *)
module Config = struct
  let cache_dir =
    try Sys.getenv "FIGMA_CACHE_DIR"
    with Not_found -> "/tmp/figma-cache"
  let ttl_hours =
    try float_of_string (Sys.getenv "FIGMA_CACHE_TTL_HOURS")
    with Not_found | Failure _ -> 24.0  (* 기본 TTL: 24시간 *)
  let ttl_variables_hours =
    try float_of_string (Sys.getenv "FIGMA_CACHE_TTL_VARIABLES_HOURS")
    with Not_found | Failure _ -> 1.0  (* 변수는 1시간 *)
  let max_l1_entries =
    try int_of_string (Sys.getenv "FIGMA_CACHE_L1_MAX")
    with Not_found | Failure _ -> 256
  let l2_max_mb =
    try int_of_string (Sys.getenv "FIGMA_CACHE_L2_MAX_MB")
    with Not_found | Failure _ -> 200
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

(** 캐시 조회 (L1 → L2 순서) *)
let get ~file_key ~node_id ?(options=[]) ?(ttl_hours=Config.ttl_hours) () =
  let key = make_cache_key ~file_key ~node_id ~options in

  (* L1: 메모리 캐시 *)
  match Hashtbl.find_opt memory_cache key with
  | Some entry ->
    let age = (Unix.gettimeofday () -. entry.cached_at) /. 3600.0 in
    if age < ttl_hours then (
      eprintf "[Cache] L1 HIT: %s (age: %.1fh)\n%!" node_id age;
      let now = Unix.gettimeofday () in
      Hashtbl.replace memory_cache key { entry with last_access = now };
      Some entry.payload
    ) else (
      Hashtbl.remove memory_cache key;
      None
    )
  | None ->
    (* L2: 파일 캐시 *)
    let cache_file = sprintf "%s/%s.json" Config.cache_dir key in
    match FS.read_file cache_file with
    | None -> None
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
               eprintf "[Cache] L2 HIT → L1: %s (age: %.1fh)\n%!" node_id age;
               Some payload
             ) else (
               FS.delete_file cache_file;  (* 만료된 캐시 삭제 *)
               None
             )
         | exception _ ->
             FS.delete_file cache_file;
             None)

(** 캐시 저장 (L1 + L2 동시) *)
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

(** 캐시 통계 *)
let stats () =
  let l1_count = Hashtbl.length memory_cache in
  let l2_files = FS.list_cache_files () in
  let l2_count = List.length l2_files in
  let l2_bytes = List.fold_left (fun acc (_, _, size) -> acc + size) 0 l2_files in
  `Assoc [
    ("l1_entries", `Int l1_count);
    ("l2_entries", `Int l2_count);
    ("l1_max_entries", `Int Config.max_l1_entries);
    ("l2_max_bytes", `Int Config.l2_max_bytes);
    ("l2_bytes", `Int l2_bytes);
    ("cache_dir", `String Config.cache_dir);
    ("ttl_hours", `Float Config.ttl_hours);
    ("ttl_variables_hours", `Float Config.ttl_variables_hours);
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
