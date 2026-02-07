(** Figma MCP Configuration

    Centralized environment variable management following 12-Factor App principles.
    All env vars use FIGMA_MCP_* prefix for consistency.

    Usage:
      let cache_dir = Figma_config.Cache.dir
      let ttl = Figma_config.Plugin.ttl_seconds
*)

(** Safe getters with defaults *)
let get_string ~default name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None -> default

let get_int ~default name =
  match Sys.getenv_opt name with
  | Some v -> (try int_of_string v with _ -> default)
  | None -> default

let get_float ~default name =
  match Sys.getenv_opt name with
  | Some v -> (try float_of_string v with _ -> default)
  | None -> default

let get_bool ~default name =
  match Sys.getenv_opt name with
  | None -> default
  | Some v ->
      let lower = String.lowercase_ascii v in
      if lower = "1" || lower = "true" || lower = "yes" || lower = "y" || lower = "on" then true
      else if lower = "0" || lower = "false" || lower = "no" || lower = "n" || lower = "off" then false
      else default

let get_string_list ~default name =
  match Sys.getenv_opt name with
  | None -> default
  | Some v ->
      let items =
        v
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      if items = [] then default else items

(** {1 Cache Configuration} *)

module Cache = struct
  (** Cache directory path *)
  let dir =
    get_string ~default:"/tmp/figma_mcp_cache" "FIGMA_MCP_CACHE_DIR"

  (** TTL for general cache entries (hours) *)
  let ttl_hours =
    get_float ~default:24.0 "FIGMA_MCP_CACHE_TTL_HOURS"

  (** TTL for variable cache entries (hours) *)
  let ttl_variables_hours =
    get_float ~default:1.0 "FIGMA_MCP_CACHE_TTL_VARIABLES_HOURS"

  (** Maximum L1 (memory) cache entries *)
  let l1_max =
    get_int ~default:100 "FIGMA_MCP_CACHE_L1_MAX"

  (** Maximum L2 (disk) cache size in MB *)
  let l2_max_mb =
    get_int ~default:500 "FIGMA_MCP_CACHE_L2_MAX_MB"
end

(** {1 Plugin Bridge Configuration} *)

module Plugin = struct
  (** Plugin connection TTL (seconds) *)
  let ttl_seconds =
    get_float ~default:300.0 "FIGMA_MCP_PLUGIN_TTL"

  (** Maximum poll wait time (milliseconds) *)
  let poll_max_ms =
    get_int ~default:30000 "FIGMA_MCP_PLUGIN_POLL_MAX_MS"

  (** Maximum queued commands per channel *)
  let max_commands =
    get_int ~default:256 "FIGMA_MCP_PLUGIN_MAX_COMMANDS"

  (** Maximum stored results per channel *)
  let max_results =
    get_int ~default:512 "FIGMA_MCP_PLUGIN_MAX_RESULTS"

  (** Maximum payload size (bytes), 0 disables limit *)
  let max_payload_bytes =
    get_int ~default:(5 * 1024 * 1024) "FIGMA_MCP_PLUGIN_MAX_PAYLOAD_BYTES"

  (** Maximum waiting pollers per channel *)
  let max_waiters =
    get_int ~default:128 "FIGMA_MCP_PLUGIN_MAX_WAITERS"

  (** TTL for stored results (seconds) *)
  let result_ttl_seconds =
    get_float ~default:120.0 "FIGMA_MCP_PLUGIN_RESULT_TTL"

  (** Cleanup interval (seconds) *)
  let cleanup_interval_seconds =
    get_float ~default:30.0 "FIGMA_MCP_PLUGIN_CLEANUP_INTERVAL"
end

(** {1 Visual Verification Configuration} *)

module Visual = struct
  (** Temporary directory for visual comparison *)
  let temp_dir =
    get_string ~default:"/tmp/figma_visual" "FIGMA_MCP_VISUAL_TEMP_DIR"

  (** Path to render script *)
  let render_script =
    Sys.getenv_opt "FIGMA_MCP_RENDER_SCRIPT"
end

(** {1 HTTP Configuration} *)

module Http = struct
  (** Figma API request timeout (seconds) *)
  let timeout_seconds =
    get_float ~default:30.0 "FIGMA_MCP_FIGMA_API_TIMEOUT_SECONDS"

  (** Figma API max response body size (bytes) *)
  let max_body_bytes =
    get_int ~default:(100 * 1024 * 1024) "FIGMA_MCP_FIGMA_API_MAX_BODY_BYTES"

  (** Log HTTP response body preview on errors *)
  let log_response_body =
    get_bool ~default:false "FIGMA_MCP_LOG_API_BODY"
end

(** {1 Response Handling Configuration} *)

module Response = struct
  (** Maximum inline response size (bytes) *)
  let max_inline =
    get_int ~default:50000 "FIGMA_MCP_MAX_INLINE_RESPONSE"

  (** Directory for large response files *)
  let large_dir =
    get_string ~default:"/tmp/figma_responses" "FIGMA_MCP_LARGE_RESPONSE_DIR"

  (** Response file TTL (seconds) *)
  let ttl_seconds =
    get_int ~default:3600 "FIGMA_MCP_RESPONSE_TTL"

  (** Maximum response directory size (MB), 0 disables size cleanup *)
  let max_dir_mb =
    get_int ~default:1024 "FIGMA_MCP_RESPONSE_MAX_DIR_MB"
end

(** {1 CORS Configuration} *)

module Cors = struct
  type profile =
    | Compat
    | Strict

  let get_string_nonempty ~default name =
    match Sys.getenv_opt name with
    | Some v when String.trim v <> "" -> v
    | _ -> default

  let profile () =
    match get_string_nonempty ~default:"compat" "FIGMA_MCP_CORS_PROFILE" |> String.lowercase_ascii with
    | "strict" -> Strict
    | _ -> Compat

  let allowed_origins_default () =
    match profile () with
    | Compat -> ["http://localhost:*"; "http://127.0.0.1:*"; "https://www.figma.com"; "null"]
    | Strict -> ["https://www.figma.com"]

  let allow_private_network_default () =
    match profile () with
    | Compat -> true
    | Strict -> false

  let allow_headers_default () =
    match profile () with
    | Compat ->
        "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Authorization, X-API-Key, X-MCP-API-Key, Access-Control-Request-Private-Network"
    | Strict ->
        "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Authorization, X-API-Key, X-MCP-API-Key"

  (** restrict | permissive *)
  let mode () =
    get_string_nonempty ~default:"restrict" "FIGMA_MCP_CORS_MODE"

  (** Allowed origins, supports :* port wildcard (e.g., http://localhost:* ) *)
  let allowed_origins () =
    get_string_list
      ~default:(allowed_origins_default ())
      "FIGMA_MCP_CORS_ALLOWED_ORIGINS"

  (** Allow private network access from browser preflight *)
  let allow_private_network () =
    get_bool ~default:(allow_private_network_default ()) "FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK"

  (** Allow headers list for preflight *)
  let allow_headers () =
    get_string_nonempty ~default:(allow_headers_default ()) "FIGMA_MCP_CORS_ALLOW_HEADERS"
end

(** {1 Asset Configuration} *)

module Asset = struct
  (** Asset directory path (with fallback chain) *)
  let dir =
    match Sys.getenv_opt "FIGMA_MCP_ASSET_DIR" with
    | Some d -> d
    | None ->
        match Sys.getenv_opt "ME_ROOT" with
        | Some root -> Filename.concat root "workspace/yousleepwhen/figma-mcp/assets"
        | None ->
            match Sys.getenv_opt "HOME" with
            | Some home -> Filename.concat home ".figma-mcp/assets"
            | None -> "/tmp/figma-mcp/assets"
end

(** {1 Authentication Configuration} *)

module Auth = struct
  (** Figma API token (external convention, no MCP prefix) *)
  let token = Sys.getenv_opt "FIGMA_TOKEN"

  (** Get token or raise *)
  let require_token () =
    match token with
    | Some t -> t
    | None -> failwith "FIGMA_TOKEN environment variable not set"
end

(** {1 LLM Integration Configuration} *)

module Llm = struct
  (** LLM provider (reserved for future batch processing) *)
  let provider =
    get_string ~default:"" "LLM_PROVIDER"
end

(** Print configuration summary *)
let print_summary () =
  Printf.eprintf "[figma_config] Cache: dir=%s ttl=%.1fh l1_max=%d l2_max=%dMB\n%!"
    Cache.dir Cache.ttl_hours Cache.l1_max Cache.l2_max_mb;
  Printf.eprintf "[figma_config] Plugin: ttl=%.0fs poll_max=%dms max_cmd=%d max_res=%d max_waiters=%d\n%!"
    Plugin.ttl_seconds Plugin.poll_max_ms Plugin.max_commands Plugin.max_results Plugin.max_waiters;
  Printf.eprintf "[figma_config] Response: max_inline=%d dir=%s ttl=%ds\n%!"
    Response.max_inline Response.large_dir Response.ttl_seconds
