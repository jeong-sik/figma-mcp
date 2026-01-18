(** Figma Node Cache - 계층적 캐시 시스템 (L1 메모리 + L2 파일)

    API 호출 절감: 22회 반복 × 15초/API = ~5.5분 → 캐시 HIT 시 즉시 응답
    예상 효과: 반복당 ~5분 절감 (25% 시간 단축)
*)

open Printf

(** 캐시 설정 *)
module Config = struct
  let cache_dir = "/tmp/figma-cache"
  let ttl_hours = 24.0  (* 기본 TTL: 24시간 *)
  let ttl_variables_hours = 1.0  (* 변수는 1시간 *)
end

(** 캐시 키 생성 *)
let make_cache_key ~file_key ~node_id ~options =
  let options_str = match options with
    | [] -> ""
    | opts -> String.concat ":" (List.sort String.compare opts)
  in
  let content = sprintf "%s:%s:%s" file_key node_id options_str in
  (* SHA256 해시의 앞 16자리 *)
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

  let delete_file path =
    if Sys.file_exists path then
      Sys.remove path

  let file_age_hours path =
    if Sys.file_exists path then
      let stat = Unix.stat path in
      let now = Unix.gettimeofday () in
      (now -. stat.Unix.st_mtime) /. 3600.0
    else infinity
end

(** 캐시 엔트리 타입 *)
type cache_entry = {
  payload: Yojson.Safe.t;
  cached_at: float;  (* Unix timestamp *)
  file_key: string;
  node_id: string;
}

(** L1 메모리 캐시 (Hashtbl 기반) *)
let memory_cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 64

(** 캐시 조회 (L1 → L2 순서) *)
let get ~file_key ~node_id ?(options=[]) ?(ttl_hours=Config.ttl_hours) () =
  let key = make_cache_key ~file_key ~node_id ~options in

  (* L1: 메모리 캐시 *)
  match Hashtbl.find_opt memory_cache key with
  | Some entry ->
    let age = (Unix.gettimeofday () -. entry.cached_at) /. 3600.0 in
    if age < ttl_hours then (
      eprintf "[Cache] L1 HIT: %s (age: %.1fh)\n%!" node_id age;
      Some entry.payload
    ) else (
      Hashtbl.remove memory_cache key;
      None
    )
  | None ->
    (* L2: 파일 캐시 *)
    let cache_file = sprintf "%s/%s.json" Config.cache_dir key in
    let age = FS.file_age_hours cache_file in
    if age < ttl_hours then
      match FS.read_file cache_file with
      | Some content ->
        (try
          let json = Yojson.Safe.from_string content in
          let payload = Yojson.Safe.Util.member "payload" json in
          let entry = {
            payload;
            cached_at = Unix.gettimeofday () -. (age *. 3600.0);
            file_key;
            node_id;
          } in
          (* L1에 승격 *)
          Hashtbl.replace memory_cache key entry;
          eprintf "[Cache] L2 HIT → L1: %s (age: %.1fh)\n%!" node_id age;
          Some payload
        with _ -> None)
      | None -> None
    else (
      FS.delete_file cache_file;  (* 만료된 캐시 삭제 *)
      None
    )

(** 캐시 저장 (L1 + L2 동시) *)
let set ~file_key ~node_id ?(options=[]) payload =
  let key = make_cache_key ~file_key ~node_id ~options in
  let now = Unix.gettimeofday () in

  (* L1: 메모리 *)
  let entry = { payload; cached_at = now; file_key; node_id } in
  Hashtbl.replace memory_cache key entry;

  (* L2: 파일 *)
  let cache_json = `Assoc [
    ("payload", payload);
    ("_cached_at", `Float now);
    ("_file_key", `String file_key);
    ("_node_id", `String node_id);
  ] in
  let cache_file = sprintf "%s/%s.json" Config.cache_dir key in
  FS.write_file cache_file (Yojson.Safe.pretty_to_string cache_json);
  eprintf "[Cache] SET: %s → %s\n%!" node_id key

(** 캐시 무효화 *)
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
    let prefix = match node_id with
      | Some nid -> make_cache_key ~file_key:fk ~node_id:nid ~options:[]
      | None -> fk
    in
    Hashtbl.filter_map_inplace (fun k v ->
      if String.sub k 0 (min 8 (String.length k)) = String.sub prefix 0 8
      then None else Some v
    ) memory_cache;
    eprintf "[Cache] INVALIDATE: %s/%s\n%!" fk (Option.value node_id ~default:"*")

(** 캐시 통계 *)
let stats () =
  let l1_count = Hashtbl.length memory_cache in
  let l2_count =
    if Sys.file_exists Config.cache_dir then
      Sys.readdir Config.cache_dir
      |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".json")
      |> List.length
    else 0
  in
  `Assoc [
    ("l1_entries", `Int l1_count);
    ("l2_entries", `Int l2_count);
    ("cache_dir", `String Config.cache_dir);
    ("ttl_hours", `Float Config.ttl_hours);
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
