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
  (** LLM provider (openai, anthropic, gemini, ollama, llm-mcp) *)
  let provider =
    get_string ~default:"" "LLM_PROVIDER"

  (** LLM model name (e.g., gpt-4o, claude-sonnet-4-20250514, gemini-2.0-flash) *)
  let model =
    get_string ~default:"" "LLM_MODEL"

  (** LLM API endpoint (for custom endpoints or llm-mcp) *)
  let endpoint =
    get_string ~default:"http://127.0.0.1:8932" "LLM_ENDPOINT"

  (** Enable LLM-enhanced hint generation *)
  let hint_enabled =
    get_string ~default:"false" "FIGMA_MCP_LLM_HINT_ENABLED" = "true"

  (** Temperature for hint generation (0.0-1.0) *)
  let hint_temperature =
    get_float ~default:0.3 "FIGMA_MCP_LLM_HINT_TEMPERATURE"

  (** Maximum tokens for hint response *)
  let hint_max_tokens =
    get_int ~default:500 "FIGMA_MCP_LLM_HINT_MAX_TOKENS"

  (** Timeout for LLM requests (milliseconds) *)
  let timeout_ms =
    get_int ~default:30000 "FIGMA_MCP_LLM_TIMEOUT_MS"

  (** Check if LLM is configured and available *)
  let is_available () =
    provider <> "" || endpoint <> "http://127.0.0.1:8932"

  (** Get configuration summary for debugging *)
  let summary () =
    Printf.sprintf "provider=%s model=%s endpoint=%s hint=%b temp=%.1f"
      provider model endpoint hint_enabled hint_temperature
end

(** Print configuration summary *)
let print_summary () =
  Printf.eprintf "[figma_config] Cache: dir=%s ttl=%.1fh l1_max=%d l2_max=%dMB\n%!"
    Cache.dir Cache.ttl_hours Cache.l1_max Cache.l2_max_mb;
  Printf.eprintf "[figma_config] Plugin: ttl=%.0fs poll_max=%dms\n%!"
    Plugin.ttl_seconds Plugin.poll_max_ms;
  Printf.eprintf "[figma_config] Response: max_inline=%d dir=%s ttl=%ds\n%!"
    Response.max_inline Response.large_dir Response.ttl_seconds;
  if Llm.is_available () then
    Printf.eprintf "[figma_config] LLM: %s\n%!" (Llm.summary ())
