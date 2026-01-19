(** LLM Provider abstraction *)

open Lwt.Syntax

type response = Llm_mcp_client.llm_response

type provider = {
  id: string;
  default_url: string;
  call_tool:
    url:string ->
    name:string ->
    arguments:Yojson.Safe.t ->
    (response, string) result Lwt.t;
}

let mcp_http_provider : provider = {
  id = "mcp-http";
  default_url = Llm_mcp_client.llm_url ();
  call_tool = (fun ~url ~name ~arguments ->
    let* result = Llm_mcp_client.call_tool ~url ~name ~arguments in
    Lwt.return result);
}

let stub_provider : provider = {
  id = "stub";
  default_url = "";
  call_tool = (fun ~url:_ ~name:_ ~arguments:_ ->
    Lwt.return (Error "LLM provider not configured"));
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
