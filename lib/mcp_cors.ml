(** CORS handling for MCP HTTP server.

    Supports permissive and restrict modes with origin pattern matching.
    Configuration sourced from [Figma_config.Cors]. *)

let mode () = String.lowercase_ascii (Figma_config.Cors.mode ())
let allowed_origins () = Figma_config.Cors.allowed_origins ()
let allow_private_network () = Figma_config.Cors.allow_private_network ()
let allow_headers () = Figma_config.Cors.allow_headers ()

let origin_of reqd =
  let request = Httpun.Reqd.request reqd in
  Httpun.Headers.get request.Httpun.Request.headers "origin"

let normalize_lower s = String.lowercase_ascii s

let default_port_for_scheme = function
  | "http" -> 80
  | "https" -> 443
  | _ -> 0

let parse_origin_components value =
  try
    let uri = Uri.of_string value in
    match (Uri.scheme uri, Uri.host uri) with
    | (Some scheme, Some host) ->
        let scheme = normalize_lower scheme in
        let host = normalize_lower host in
        let path = Uri.path uri in
        let query = Uri.query uri in
        let fragment = Uri.fragment uri in
        if (path <> "" && path <> "/") || query <> [] || fragment <> None then None
        else if scheme <> "http" && scheme <> "https" then None
        else Some (scheme, host, Uri.port uri)
    | _ -> None
  with _ -> None

type origin_value =
  | Origin_null
  | Origin of { scheme : string; host : string; port : int option }

let parse_origin_value value =
  let value = String.trim value in
  if value = "null" then Some Origin_null
  else
    match parse_origin_components value with
    | Some (scheme, host, port) -> Some (Origin { scheme; host; port })
    | None -> None

type origin_pattern =
  | Any
  | Null
  | Exact of { scheme : string; host : string; port : int option }
  | Any_port of { scheme : string; host : string }

let parse_pattern pattern =
  let pattern = String.trim pattern in
  if pattern = "*" then Some Any
  else if pattern = "null" then Some Null
  else
    let len = String.length pattern in
    let ends_with suffix =
      let ls = String.length suffix in
      len >= ls && String.sub pattern (len - ls) ls = suffix
    in
    if ends_with ":*" then
      let base = String.sub pattern 0 (len - 2) in
      match parse_origin_components base with
      | Some (scheme, host, _) -> Some (Any_port { scheme; host })
      | None -> None
    else if ends_with "*" then
      let base = String.sub pattern 0 (len - 1) in
      match parse_origin_components base with
      | Some (scheme, host, _) -> Some (Any_port { scheme; host })
      | None -> None
    else
      match parse_origin_components pattern with
      | Some (scheme, host, port) -> Some (Exact { scheme; host; port })
      | None -> None

let normalized_port scheme port_opt =
  match port_opt with
  | Some p -> p
  | None -> default_port_for_scheme scheme

let matches_pattern origin pattern =
  match (origin, pattern) with
  | (_, Any) -> true
  | (Origin_null, Null) -> true
  | (Origin_null, _) -> false
  | (Origin o, Exact p) ->
      o.scheme = p.scheme
      && o.host = p.host
      && normalized_port o.scheme o.port = normalized_port p.scheme p.port
  | (Origin o, Any_port p) ->
      o.scheme = p.scheme && o.host = p.host
  | _ -> false

let origin_allowed origin =
  match parse_origin_value origin with
  | None -> false
  | Some origin_value ->
      let allowed = allowed_origins () in
      List.exists
        (fun pattern ->
          match parse_pattern pattern with
          | Some parsed -> matches_pattern origin_value parsed
          | None -> false)
        allowed

let is_allowed reqd =
  match mode () with
  | "restrict" -> (
      match origin_of reqd with
      | None -> true
      | Some origin -> origin_allowed origin)
  | _ -> true

let allow_origin_value_of_origin_opt origin_opt =
  match mode () with
  | "permissive" -> Some "*"
  | "restrict" -> (
      match origin_opt with
      | Some origin when origin_allowed origin -> Some origin
      | _ -> None)
  | _ -> Some "*"

let headers_for_origin_opt origin_opt ~include_methods ~include_headers =
  match allow_origin_value_of_origin_opt origin_opt with
  | None -> []
  | Some origin ->
      let base =
        let vary = if origin = "*" then [] else [("vary", "Origin")] in
        ("access-control-allow-origin", origin) :: vary
      in
      let headers =
        if include_methods then
          ("access-control-allow-methods", "GET, POST, OPTIONS") :: base
        else base
      in
      let headers =
        if include_headers then
          ("access-control-allow-headers", allow_headers ()) :: headers
        else headers
      in
      if allow_private_network () then
        ("access-control-allow-private-network", "true") :: headers
      else headers

let headers reqd ~include_methods ~include_headers =
  headers_for_origin_opt (origin_of reqd) ~include_methods ~include_headers
