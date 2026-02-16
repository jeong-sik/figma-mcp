[@@@coverage off]
(* Version auto-synced from dune-project via dune-build-info.
   Build_info.V1.version() is a compile-time constant that bisect_ppx
   cannot exercise (returns None in instrumented test builds). *)

let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v
