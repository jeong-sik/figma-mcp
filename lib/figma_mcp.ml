(** Figma MCP - 메인 모듈 *)

module Types = Figma_types
module Parser = Figma_parser
module Codegen = Figma_codegen
module Api = Figma_api
module Protocol = Mcp_protocol
module Protocol_eio = Mcp_protocol_eio
module Tools = Mcp_tools
module Effects = Figma_effects
module Grpc_server = Figma_grpc_server

(** stdin에서 JSON 읽어서 변환 *)
let process_stdin ~format ~components =
  let buf = Buffer.create 8192 in
  (try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done
  with End_of_file -> ());
  let json_str = Buffer.contents buf in

  match Parser.parse_json_string json_str with
  | None -> "Error: Failed to parse JSON"
  | Some node ->
      if components then
        Codegen.split_to_components node
      else
        match format with
        | "compact" | "dsl" -> Codegen.generate_compact node
        | "html" -> Codegen.generate_html node
        | _ -> "Unknown format. Use: compact, html"

(** 문자열 JSON에서 변환 *)
let process_json_string ~format json_str =
  match Parser.parse_json_string json_str with
  | None -> Error "Failed to parse JSON"
  | Some node ->
      Ok (match format with
          | "compact" | "dsl" -> Codegen.generate_compact node
          | "verbose" -> Codegen.generate_verbose node
          | "html" -> Codegen.generate_html node
          | _ -> "Unknown format")

(** 화면 목록 추출 *)
let list_screens json_str =
  match Parser.parse_json_string json_str with
  | None -> []
  | Some node -> Codegen.extract_screens node
