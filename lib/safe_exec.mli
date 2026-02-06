type output = {
  stdout: string;
  stderr: string;
  stdout_truncated: bool;
  stderr_truncated: bool;
  status: Unix.process_status;
  timed_out: bool;
}

val run
  :  ?timeout_ms:int
  -> ?output_limit:int
  -> string
  -> string array
  -> (output, string) result

val run_stdout
  :  ?timeout_ms:int
  -> ?output_limit:int
  -> string
  -> string array
  -> (string, string) result

val find_in_path : string -> string option
val command_exists : string -> bool
