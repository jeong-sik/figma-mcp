(** Figma MCP Effect System

    OCaml 5.x Algebraic Effects for type-safe side effect tracking.
    Enables:
    - Deterministic testing with mock handlers
    - Clear separation of pure Figma parsing logic and HTTP I/O
    - Composable effect handlers for production vs test environments

    Based on masc-mcp Effect System pattern.
*)

(** {1 Effect Declarations} *)

(** Figma API result type *)
type figma_result = (Yojson.Safe.t, string) result
type file_result = (string, string) result

(** Figma API effects - all HTTP calls to api.figma.com *)
type _ Effect.t +=
  | Figma_get_file : {
      token: string;
      file_key: string;
      depth: int option;
      geometry: string option;
      plugin_data: string option;
      version: string option;
    } -> figma_result Effect.t
  | Figma_get_nodes : {
      token: string;
      file_key: string;
      node_ids: string list;
      depth: int option;
      geometry: string option;
      plugin_data: string option;
      version: string option;
    } -> figma_result Effect.t
  | Figma_get_images : {
      token: string;
      file_key: string;
      node_ids: string list;
      format: string;
      scale: int;
      use_absolute_bounds: bool option;
      version: string option;
    } -> figma_result Effect.t
  | Figma_get_file_images : {
      token: string;
      file_key: string;
      version: string option;
    } -> figma_result Effect.t
  | Figma_get_file_meta : {
      token: string;
      file_key: string;
      version: string option;
    } -> figma_result Effect.t
  | Figma_get_file_components : {
      token: string;
      file_key: string;
    } -> figma_result Effect.t
  | Figma_get_team_components : {
      token: string;
      team_id: string;
    } -> figma_result Effect.t
  | Figma_get_file_component_sets : {
      token: string;
      file_key: string;
    } -> figma_result Effect.t
  | Figma_get_team_component_sets : {
      token: string;
      team_id: string;
    } -> figma_result Effect.t
  | Figma_get_file_styles : {
      token: string;
      file_key: string;
    } -> figma_result Effect.t
  | Figma_get_team_styles : {
      token: string;
      team_id: string;
    } -> figma_result Effect.t
  | Figma_get_component : {
      token: string;
      component_key: string;
    } -> figma_result Effect.t
  | Figma_get_component_set : {
      token: string;
      component_set_key: string;
    } -> figma_result Effect.t
  | Figma_get_style : {
      token: string;
      style_key: string;
    } -> figma_result Effect.t
  | Figma_get_file_versions : {
      token: string;
      file_key: string;
    } -> figma_result Effect.t
  | Figma_get_file_comments : {
      token: string;
      file_key: string;
    } -> figma_result Effect.t
  | Figma_post_file_comment : {
      token: string;
      file_key: string;
      message: string;
      client_meta: Yojson.Safe.t;
    } -> figma_result Effect.t
  | Figma_download_url : { url: string; path: string } -> file_result Effect.t
  | Figma_get_me : { token: string } -> figma_result Effect.t
  | Figma_get_team_projects : { token: string; team_id: string } -> figma_result Effect.t
  | Figma_get_project_files : { token: string; project_id: string } -> figma_result Effect.t
  | Figma_get_variables : { token: string; file_key: string } -> figma_result Effect.t

(** Logging effects *)
type _ Effect.t +=
  | Log_debug : string -> unit Effect.t
  | Log_info : string -> unit Effect.t
  | Log_error : string -> unit Effect.t

(** {1 Effect Performers - API for business logic} *)

module Perform = struct
  (** Get a Figma file *)
  let get_file ?depth ?geometry ?plugin_data ?version ~token ~file_key () =
    Effect.perform (Figma_get_file { token; file_key; depth; geometry; plugin_data; version })

  (** Get specific nodes from a file *)
  let get_nodes ?depth ?geometry ?plugin_data ?version ~token ~file_key ~node_ids () =
    Effect.perform (Figma_get_nodes { token; file_key; node_ids; depth; geometry; plugin_data; version })

  (** Export images for nodes *)
  let get_images ?use_absolute_bounds ?version ~token ~file_key ~node_ids ~format ~scale () =
    Effect.perform (Figma_get_images { token; file_key; node_ids; format; scale; use_absolute_bounds; version })

  (** Get image fills from a file *)
  let get_file_images ?version ~token ~file_key () =
    Effect.perform (Figma_get_file_images { token; file_key; version })

  (** Get file meta (components/styles/componentSets) *)
  let get_file_meta ?version ~token ~file_key () =
    Effect.perform (Figma_get_file_meta { token; file_key; version })

  (** Get file components *)
  let get_file_components ~token ~file_key =
    Effect.perform (Figma_get_file_components { token; file_key })

  (** Get team components *)
  let get_team_components ~token ~team_id =
    Effect.perform (Figma_get_team_components { token; team_id })

  (** Get file component sets *)
  let get_file_component_sets ~token ~file_key =
    Effect.perform (Figma_get_file_component_sets { token; file_key })

  (** Get team component sets *)
  let get_team_component_sets ~token ~team_id =
    Effect.perform (Figma_get_team_component_sets { token; team_id })

  (** Get file styles *)
  let get_file_styles ~token ~file_key =
    Effect.perform (Figma_get_file_styles { token; file_key })

  (** Get team styles *)
  let get_team_styles ~token ~team_id =
    Effect.perform (Figma_get_team_styles { token; team_id })

  (** Get a component by key *)
  let get_component ~token ~component_key =
    Effect.perform (Figma_get_component { token; component_key })

  (** Get a component set by key *)
  let get_component_set ~token ~component_set_key =
    Effect.perform (Figma_get_component_set { token; component_set_key })

  (** Get a style by key *)
  let get_style ~token ~style_key =
    Effect.perform (Figma_get_style { token; style_key })

  (** Get file versions *)
  let get_file_versions ~token ~file_key =
    Effect.perform (Figma_get_file_versions { token; file_key })

  (** Get file comments *)
  let get_file_comments ~token ~file_key =
    Effect.perform (Figma_get_file_comments { token; file_key })

  (** Post a file comment *)
  let post_file_comment ~token ~file_key ~message ~client_meta =
    Effect.perform (Figma_post_file_comment { token; file_key; message; client_meta })

  (** Download asset by URL *)
  let download_url ~url ~path =
    Effect.perform (Figma_download_url { url; path })

  (** Get current user info *)
  let get_me ~token =
    Effect.perform (Figma_get_me { token })

  (** Get team projects *)
  let get_team_projects ~token ~team_id =
    Effect.perform (Figma_get_team_projects { token; team_id })

  (** Get project files *)
  let get_project_files ~token ~project_id =
    Effect.perform (Figma_get_project_files { token; project_id })

  (** Get local variables *)
  let get_variables ~token ~file_key =
    Effect.perform (Figma_get_variables { token; file_key })

  (** Logging *)
  let log_debug msg = Effect.perform (Log_debug msg)
  let log_info msg = Effect.perform (Log_info msg)
  let log_error msg = Effect.perform (Log_error msg)
end

(** {1 Production Handler - Real Figma API calls via Lwt} *)

(** Run computation with real Figma API handler.
    Uses Lwt_main.run internally for each HTTP call. *)
let run_with_real_api : 'a. (unit -> 'a) -> 'a = fun computation ->
  Effect.Deep.match_with computation ()
    { retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_get_file { token; file_key; depth; geometry; plugin_data; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file ~token ~file_key ?depth ?geometry ?plugin_data ?version ()) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_nodes { token; file_key; node_ids; depth; geometry; plugin_data; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file_nodes ~token ~file_key ~node_ids ?depth ?geometry ?plugin_data ?version ()) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_images { token; file_key; node_ids; format; scale; use_absolute_bounds; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_main.run (Figma_api.get_images ~token ~file_key ~node_ids ~format ~scale ?use_absolute_bounds ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_images { token; file_key; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_main.run (Figma_api.get_file_images ~token ~file_key ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_meta { token; file_key; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_main.run (Figma_api.get_file_meta ~token ~file_key ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_components { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file_components ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_components { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_team_components ~token ~team_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_component_sets { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file_component_sets ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_component_sets { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_team_component_sets ~token ~team_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_styles { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file_styles ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_styles { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_team_styles ~token ~team_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_component { token; component_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_component ~token ~component_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_component_set { token; component_set_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_component_set ~token ~component_set_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_style { token; style_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_style ~token ~style_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_versions { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file_versions ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_comments { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_file_comments ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_post_file_comment { token; file_key; message; client_meta } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_main.run (Figma_api.post_file_comment ~token ~file_key ~message ~client_meta)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_download_url { url; path } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.download_url ~url ~path) in
              Effect.Deep.continue k result)

        | Figma_get_me { token } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_me ~token) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_projects { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_team_projects ~token ~team_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_project_files { token; project_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_project_files ~token ~project_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_variables { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_main.run (Figma_api.get_local_variables ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Log_debug msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[DEBUG] %s\n%!" msg;
              Effect.Deep.continue k ())

        | Log_info msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[INFO] %s\n%!" msg;
              Effect.Deep.continue k ())

        | Log_error msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[ERROR] %s\n%!" msg;
              Effect.Deep.continue k ())

        | _ -> None
    }

(** {1 Eio Handler - Real Figma API calls via Lwt_eio bridge} *)

(** Run computation with real Figma API handler inside Eio runtime.
    Uses Lwt_eio bridge - MUST be called within Eio_main.run context. *)
let run_with_eio_api : 'a. (unit -> 'a) -> 'a = fun computation ->
  Effect.Deep.match_with computation ()
    { retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_get_file { token; file_key; depth; geometry; plugin_data; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file ~token ~file_key ?depth ?geometry ?plugin_data ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_nodes { token; file_key; node_ids; depth; geometry; plugin_data; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () ->
                  Figma_api.get_file_nodes ~token ~file_key ~node_ids ?depth ?geometry ?plugin_data ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_images { token; file_key; node_ids; format; scale; use_absolute_bounds; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () ->
                  Figma_api.get_images ~token ~file_key ~node_ids ~format ~scale ?use_absolute_bounds ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_images { token; file_key; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_images ~token ~file_key ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_meta { token; file_key; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_meta ~token ~file_key ?version ())
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_components { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_components ~token ~file_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_components { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_team_components ~token ~team_id)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_component_sets { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_component_sets ~token ~file_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_component_sets { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_team_component_sets ~token ~team_id)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_styles { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_styles ~token ~file_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_styles { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_team_styles ~token ~team_id)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_component { token; component_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_component ~token ~component_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_component_set { token; component_set_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_component_set ~token ~component_set_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_style { token; style_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_style ~token ~style_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_versions { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_versions ~token ~file_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_comments { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () -> Figma_api.get_file_comments ~token ~file_key)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_post_file_comment { token; file_key; message; client_meta } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Lwt_eio.run_lwt (fun () ->
                  Figma_api.post_file_comment ~token ~file_key ~message ~client_meta)
              in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_download_url { url; path } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_eio.run_lwt (fun () -> Figma_api.download_url ~url ~path) in
              Effect.Deep.continue k result)

        | Figma_get_me { token } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_eio.run_lwt (fun () -> Figma_api.get_me ~token) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_projects { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_eio.run_lwt (fun () -> Figma_api.get_team_projects ~token ~team_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_project_files { token; project_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_eio.run_lwt (fun () -> Figma_api.get_project_files ~token ~project_id) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_variables { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Lwt_eio.run_lwt (fun () -> Figma_api.get_local_variables ~token ~file_key) in
              let result' = Result.map_error Figma_api.error_to_string result in
              Effect.Deep.continue k result')

        | Log_debug msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[DEBUG] %s\n%!" msg;
              Effect.Deep.continue k ())

        | Log_info msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[INFO] %s\n%!" msg;
              Effect.Deep.continue k ())

        | Log_error msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[ERROR] %s\n%!" msg;
              Effect.Deep.continue k ())

        | _ -> None
    }

(** {1 Mock Handler for Testing} *)

(** Mock data store for testing *)
type mock_store = {
  files: (string, Yojson.Safe.t) Hashtbl.t;
  nodes: (string, Yojson.Safe.t) Hashtbl.t;
  file_images: (string, Yojson.Safe.t) Hashtbl.t;
  file_meta: (string, Yojson.Safe.t) Hashtbl.t;
  me: Yojson.Safe.t option ref;
  projects: (string, Yojson.Safe.t) Hashtbl.t;
  project_files: (string, Yojson.Safe.t) Hashtbl.t;
  variables: (string, Yojson.Safe.t) Hashtbl.t;
  images: (string, Yojson.Safe.t) Hashtbl.t;
}

let create_mock_store () = {
  files = Hashtbl.create 16;
  nodes = Hashtbl.create 16;
  file_images = Hashtbl.create 16;
  file_meta = Hashtbl.create 16;
  me = ref None;
  projects = Hashtbl.create 16;
  project_files = Hashtbl.create 16;
  variables = Hashtbl.create 16;
  images = Hashtbl.create 16;
}

(** Run computation with mock handler for deterministic testing. *)
let run_with_mock : 'a. mock_store -> (unit -> 'a) -> 'a = fun store computation ->
  Effect.Deep.match_with computation ()
    { retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_get_file { token = _; file_key; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match Hashtbl.find_opt store.files file_key with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: file %s not found" file_key)
              in
              Effect.Deep.continue k result)

        | Figma_get_nodes { token = _; file_key; node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let key = file_key ^ ":" ^ String.concat "," node_ids in
              let result = match Hashtbl.find_opt store.nodes key with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: nodes %s not found" key)
              in
              Effect.Deep.continue k result)

        | Figma_get_images { token = _; file_key; node_ids; format = _; scale = _; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let key = file_key ^ ":" ^ String.concat "," node_ids in
              let result = match Hashtbl.find_opt store.images key with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: images %s not found" key)
              in
              Effect.Deep.continue k result)

        | Figma_get_file_images { token = _; file_key; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match Hashtbl.find_opt store.file_images file_key with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: file images for %s not found" file_key)
              in
              Effect.Deep.continue k result)

        | Figma_get_file_meta { token = _; file_key; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match Hashtbl.find_opt store.file_meta file_key with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: file meta for %s not found" file_key)
              in
              Effect.Deep.continue k result)

        | Figma_get_file_components { token = _; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: file components for %s not implemented" file_key) in
              Effect.Deep.continue k result)

        | Figma_get_team_components { token = _; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: team components for %s not implemented" team_id) in
              Effect.Deep.continue k result)

        | Figma_get_file_component_sets { token = _; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: file component sets for %s not implemented" file_key) in
              Effect.Deep.continue k result)

        | Figma_get_team_component_sets { token = _; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: team component sets for %s not implemented" team_id) in
              Effect.Deep.continue k result)

        | Figma_get_file_styles { token = _; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: file styles for %s not implemented" file_key) in
              Effect.Deep.continue k result)

        | Figma_get_team_styles { token = _; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: team styles for %s not implemented" team_id) in
              Effect.Deep.continue k result)

        | Figma_get_component { token = _; component_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: component %s not implemented" component_key) in
              Effect.Deep.continue k result)

        | Figma_get_component_set { token = _; component_set_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: component set %s not implemented" component_set_key) in
              Effect.Deep.continue k result)

        | Figma_get_style { token = _; style_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: style %s not implemented" style_key) in
              Effect.Deep.continue k result)

        | Figma_get_file_versions { token = _; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: file versions for %s not implemented" file_key) in
              Effect.Deep.continue k result)

        | Figma_get_file_comments { token = _; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: file comments for %s not implemented" file_key) in
              Effect.Deep.continue k result)

        | Figma_post_file_comment { token = _; file_key; message = _; client_meta = _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: post comment for %s not implemented" file_key) in
              Effect.Deep.continue k result)

        | Figma_download_url { url = _; path } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Error (Printf.sprintf "Mock: download not available (%s)" path) in
              Effect.Deep.continue k result)

        | Figma_get_me { token = _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match !(store.me) with
                | Some json -> Ok json
                | None -> Error "Mock: me not set"
              in
              Effect.Deep.continue k result)

        | Figma_get_team_projects { token = _; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match Hashtbl.find_opt store.projects team_id with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: projects for team %s not found" team_id)
              in
              Effect.Deep.continue k result)

        | Figma_get_project_files { token = _; project_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match Hashtbl.find_opt store.project_files project_id with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: files for project %s not found" project_id)
              in
              Effect.Deep.continue k result)

        | Figma_get_variables { token = _; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = match Hashtbl.find_opt store.variables file_key with
                | Some json -> Ok json
                | None -> Error (Printf.sprintf "Mock: variables for %s not found" file_key)
              in
              Effect.Deep.continue k result)

        | Log_debug _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              (* Silent in tests *)
              Effect.Deep.continue k ())

        | Log_info _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              (* Silent in tests *)
              Effect.Deep.continue k ())

        | Log_error _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              (* Silent in tests *)
              Effect.Deep.continue k ())

        | _ -> None
    }

(** {1 Pure Eio Handler - No Lwt bridge} *)

(** Run computation with pure Eio Figma API handler.
    Uses Figma_api_eio directly without Lwt_eio bridge.
    MUST be called within Eio_main.run context with valid sw and client.

    @param sw Eio switch for resource management
    @param client Cohttp_eio HTTP client
    @param computation The effectful computation to run *)
let run_with_pure_eio_api ~sw ~clock ~client computation =
  Effect.Deep.match_with computation ()
    { retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_get_file { token; file_key; depth; geometry; plugin_data; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file ~sw ~clock ~client ~token ~file_key ?depth ?geometry ?plugin_data ?version ()
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_nodes { token; file_key; node_ids; depth; geometry; plugin_data; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_nodes ~sw ~clock ~client ~token ~file_key ~node_ids ?depth ?geometry ?plugin_data ?version ()
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_images { token; file_key; node_ids; format; scale; use_absolute_bounds; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_images ~sw ~clock ~client ~token ~file_key ~node_ids ~format ~scale ?use_absolute_bounds ?version ()
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_images { token; file_key; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_images ~sw ~clock ~client ~token ~file_key ?version ()
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_meta { token; file_key; version } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_meta ~sw ~clock ~client ~token ~file_key ?version ()
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_components { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_components ~sw ~clock ~client ~token ~file_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_components { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_team_components ~sw ~clock ~client ~token ~team_id
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_component_sets { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_component_sets ~sw ~clock ~client ~token ~file_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_component_sets { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_team_component_sets ~sw ~clock ~client ~token ~team_id
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_styles { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_styles ~sw ~clock ~client ~token ~file_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_styles { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_team_styles ~sw ~clock ~client ~token ~team_id
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_component { token; component_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_component ~sw ~clock ~client ~token ~component_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_component_set { token; component_set_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_component_set ~sw ~clock ~client ~token ~component_set_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_style { token; style_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_style ~sw ~clock ~client ~token ~style_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_versions { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_versions ~sw ~clock ~client ~token ~file_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_file_comments { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.get_file_comments ~sw ~clock ~client ~token ~file_key
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_post_file_comment { token; file_key; message; client_meta } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result =
                Figma_api_eio.post_file_comment ~sw ~clock ~client ~token ~file_key ~message ~client_meta
              in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_download_url { url; path } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Figma_api_eio.download_url ~sw ~clock ~client ~url ~path in
              (* download_url returns (unit, api_error), but effect expects (string, string) *)
              let result' = Result.map (fun () -> path) result in
              let result'' = Result.map_error Figma_api_eio.api_error_to_string result' in
              Effect.Deep.continue k result'')

        | Figma_get_me { token } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Figma_api_eio.get_me ~sw ~clock ~client ~token in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_team_projects { token; team_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Figma_api_eio.get_team_projects ~sw ~clock ~client ~token ~team_id in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_project_files { token; project_id } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Figma_api_eio.get_project_files ~sw ~clock ~client ~token ~project_id in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Figma_get_variables { token; file_key } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let result = Figma_api_eio.get_local_variables ~sw ~clock ~client ~token ~file_key in
              let result' = Result.map_error Figma_api_eio.api_error_to_string result in
              Effect.Deep.continue k result')

        | Log_debug msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[DEBUG] %s\n%!" msg;
              Effect.Deep.continue k ())

        | Log_info msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[INFO] %s\n%!" msg;
              Effect.Deep.continue k ())

        | Log_error msg ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Printf.eprintf "[ERROR] %s\n%!" msg;
              Effect.Deep.continue k ())

        | _ -> None
    }
