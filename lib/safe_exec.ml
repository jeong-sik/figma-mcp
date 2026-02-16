let default_timeout_ms = 20000
let default_output_limit = 256 * 1024
let kill_grace_seconds = 1.0

type output = {
  stdout: string;
  stderr: string;
  stdout_truncated: bool;
  stderr_truncated: bool;
  status: Unix.process_status;
  timed_out: bool;
}

let find_in_path bin =
  let path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  let dirs = String.split_on_char ':' path in
  let fallback_dirs = ["/opt/homebrew/bin"; "/usr/local/bin"; "/usr/bin"; "/bin"] in
  let all_dirs = dirs @ fallback_dirs in
  let rec loop = function
    | [] -> None
    | dir :: rest ->
        let candidate = Filename.concat dir bin in
        if Sys.file_exists candidate then Some candidate else loop rest
  in
  loop all_dirs

let command_exists name =
  match find_in_path name with
  | Some _ -> true
  | None -> false

let set_nonblock fd =
  try Unix.set_nonblock fd with Unix.Unix_error _ -> ()

let read_available ~fd ~buf ~limit ~truncated ~closed =
  if !closed then ()
  else
    let tmp = Bytes.create 4096 in
    try
      let n = Unix.read fd tmp 0 (Bytes.length tmp) in
      if n = 0 then closed := true
      else
        let remaining = limit - Buffer.length buf in
        if remaining > 0 then
          Buffer.add_subbytes buf tmp 0 (min remaining n);
        if n > remaining then truncated := true
    with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()

let waitpid_nohang pid =
  try
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> None
    | _, status -> Some status
  with Unix.Unix_error _ -> None

let kill_process pid =
  try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ()

let kill_process_force pid =
  try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()

let run ?(timeout_ms=default_timeout_ms) ?(output_limit=default_output_limit) cmd argv =
  try
    let stdout_r, stdout_w = Unix.pipe () in
    let stderr_r, stderr_w = Unix.pipe () in
    let pid = Unix.create_process cmd argv Unix.stdin stdout_w stderr_w in
    Unix.close stdout_w;
    Unix.close stderr_w;
    set_nonblock stdout_r;
    set_nonblock stderr_r;

    let stdout_buf = Buffer.create 1024 in
    let stderr_buf = Buffer.create 1024 in
    let stdout_truncated = ref false in
    let stderr_truncated = ref false in
    let stdout_closed = ref false in
    let stderr_closed = ref false in
    let status = ref None in
    let timed_out = ref false in
    let start = Unix.gettimeofday () in

    let rec loop () =
      let now = Unix.gettimeofday () in
      if (not !timed_out) && now -. start > (float_of_int timeout_ms /. 1000.0) then begin
        timed_out := true;
        kill_process pid
      end;

      let timeout = 0.1 in
      let fds =
        (if !stdout_closed then [] else [stdout_r]) @
        (if !stderr_closed then [] else [stderr_r])
      in
      if fds <> [] then begin
        (try
          let readable, _, _ = Unix.select fds [] [] timeout in
          List.iter (fun fd ->
            if fd = stdout_r then
              read_available ~fd:stdout_r ~buf:stdout_buf ~limit:output_limit ~truncated:stdout_truncated ~closed:stdout_closed
            else if fd = stderr_r then
              read_available ~fd:stderr_r ~buf:stderr_buf ~limit:output_limit ~truncated:stderr_truncated ~closed:stderr_closed
          ) readable
        with Unix.Unix_error (Unix.EINTR, _, _) -> ())
      end else
        (try ignore (Unix.select [] [] [] timeout)
         with Unix.Unix_error (Unix.EINTR, _, _) -> ());

      (match !status with
       | None -> status := waitpid_nohang pid
       | Some _ -> ());

      if !timed_out && now -. start > (float_of_int timeout_ms /. 1000.0 +. kill_grace_seconds) then
        kill_process_force pid;

      if (!status <> None) && !stdout_closed && !stderr_closed then ()
      else loop ()
    in
    loop ();

    Unix.close stdout_r;
    Unix.close stderr_r;

    match !status with
    | None -> Error "Failed to collect child process status"
    | Some st ->
        Ok {
          stdout = Buffer.contents stdout_buf;
          stderr = Buffer.contents stderr_buf;
          stdout_truncated = !stdout_truncated;
          stderr_truncated = !stderr_truncated;
          status = st;
          timed_out = !timed_out;
        }
  with
  | Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "Unix error: %s" (Unix.error_message e))
  | exn -> Error (Printexc.to_string exn)

let run_stdout ?timeout_ms ?output_limit cmd argv =
  match run ?timeout_ms ?output_limit cmd argv with
  | Error e -> Error e
  | Ok out ->
      if out.timed_out then
        Error "Process timed out"
      else
        (match out.status with
         | Unix.WEXITED 0 -> Ok out.stdout
         | Unix.WEXITED code ->
             Error (Printf.sprintf "Exit code %d: %s" code out.stderr)
         | Unix.WSIGNALED signal ->
             Error (Printf.sprintf "Killed by signal %d" signal)
         | Unix.WSTOPPED signal ->
             Error (Printf.sprintf "Stopped by signal %d" signal))
