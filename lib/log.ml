(** Structured logging for figma-mcp.

    All output goes to stderr with timestamp, level, and module prefix.
    Replaces scattered Printf.eprintf calls with a consistent format. *)

type level = Debug | Info | Warn | Error

let level_to_string = function
  | Debug -> "DEBUG" | Info -> "INFO" | Warn -> "WARN" | Error -> "ERROR"

let min_level = ref Info

let set_level l = min_level := l

let level_to_int = function
  | Debug -> 0 | Info -> 1 | Warn -> 2 | Error -> 3

let should_log level = level_to_int level >= level_to_int !min_level

let timestamp () =
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime now in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let log level ~module_name fmt =
  Printf.ksprintf (fun msg ->
    if should_log level then
      Printf.eprintf "[%s] [%s] [%s] %s\n%!"
        (timestamp ()) (level_to_string level) module_name msg
  ) fmt

(** Functor for per-module loggers *)
module Make (M : sig val name : string end) = struct
  let debug fmt = log Debug ~module_name:M.name fmt
  let info fmt = log Info ~module_name:M.name fmt
  let warn fmt = log Warn ~module_name:M.name fmt
  let error fmt = log Error ~module_name:M.name fmt
end

(** Pre-built loggers for common modules *)
module Cache = Make(struct let name = "Cache" end)
module Api = Make(struct let name = "Api" end)
module Plugin = Make(struct let name = "Plugin" end)
module Config = Make(struct let name = "Config" end)
module Mcp = Make(struct let name = "MCP" end)
module Visual = Make(struct let name = "Visual" end)
module Effects = Make(struct let name = "Effects" end)
module Telemetry = Make(struct let name = "Telemetry" end)
