let expand_tilde path =
  if path <> "" && path.[0] = '~' then
    match Sys.getenv_opt "HOME" with
    | Some home -> home ^ String.sub path 1 (String.length path - 1)
    | None -> path
  else
    path

let telemetry_file =
  let from_env =
    match Sys.getenv_opt "FIGMA_MCP_TELEMETRY_FILE" with
    | Some p -> Some p
    | None -> Sys.getenv_opt "MCP_TELEMETRY_FILE"
  in
  from_env
  |> Option.value ~default:"data/telemetry.jsonl"
  |> expand_tilde

let rec ensure_dir path =
  if path = "" || path = "." || path = "/" then
    ()
  else if Sys.file_exists path then
    ()
  else (
    ensure_dir (Filename.dirname path);
    Unix.mkdir path 0o755
  )

let ensure_parent_dir path =
  let dir = Filename.dirname path in
  try ensure_dir dir with _ -> ()

let write_mutex = Mutex.create ()

let append_json (json : Yojson.Safe.t) =
  ensure_parent_dir telemetry_file;
  Mutex.lock write_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock write_mutex)
    (fun () ->
      try
        let oc =
          open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 telemetry_file
        in
        Fun.protect
          ~finally:(fun () -> close_out oc)
          (fun () ->
            output_string oc (Yojson.Safe.to_string json);
            output_char oc '\n';
            flush oc)
      with _ -> ())

let log_tool_called ~tool_name ~duration_ms ~success ~error =
  let error_json = match error with Some e -> `String e | None -> `Null in
  let payload =
    `Assoc
      [
        ("tool_name", `String tool_name);
        ("success", `Bool success);
        ("duration_ms", `Int duration_ms);
        ("agent_id", `Null);
        ("error", error_json);
      ]
  in
  let record =
    `Assoc
      [
        ("timestamp", `Float (Unix.gettimeofday ()));
        ("event", `List [ `String "Tool_called"; payload ]);
      ]
  in
  append_json record

