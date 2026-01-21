(** LLM Provider abstraction (Pure Eio) *)

(** Response type from LLM MCP calls *)
type response = Llm_mcp_client_eio.llm_response

type provider = {
  id: string;
  default_url: string;
  call_tool:
    'a. sw:Eio.Switch.t ->
    clock:'a Eio.Time.clock ->
    client:Llm_mcp_client_eio.client ->
    url:string ->
    name:string ->
    arguments:Yojson.Safe.t ->
    (response, string) result;
}

let mcp_http_provider : provider = {
  id = "mcp-http";
  default_url = Llm_mcp_client_eio.llm_url ();
  call_tool = (fun ~sw ~clock ~client ~url ~name ~arguments ->
    Llm_mcp_client_eio.call_tool ~sw ~clock ~client ~url ~name ~arguments);
}

let stub_provider : provider = {
  id = "stub";
  default_url = "";
  call_tool = (fun ~sw:_ ~clock:_ ~client:_ ~url:_ ~name:_ ~arguments:_ ->
    Error "LLM provider not configured");
}

let provider_of_string value =
  let normalized = String.lowercase_ascii value in
  match normalized with
  | "mcp" | "mcp-http" | "mcp_http" | "slot" -> Ok mcp_http_provider
  | "stub" -> Ok stub_provider
  | _ -> Error ("Unknown LLM provider: " ^ value)

let default_provider () =
  match Sys.getenv_opt "LLM_PROVIDER" with
  | Some value ->
      (match provider_of_string value with
       | Ok provider -> provider
       | Error _ -> stub_provider)
  | None -> mcp_http_provider

let resolve ?provider () =
  match provider with
  | Some value -> provider_of_string value
  | None -> Ok (default_provider ())
