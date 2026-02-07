type api_key_error =
  | Missing
  | Invalid

let env_truthy name =
  match Sys.getenv_opt name with
  | None -> false
  | Some v ->
      let lower = String.lowercase_ascii (String.trim v) in
      lower = "1" || lower = "true" || lower = "yes" || lower = "y" || lower = "on"

let header_value_ci headers name =
  let target = String.lowercase_ascii name in
  Httpun.Headers.to_list headers
  |> List.find_map (fun (k, v) ->
       if String.lowercase_ascii k = target then Some v else None)

let normalize_key value =
  let trimmed = String.trim value in
  if trimmed = "" then None else Some trimmed

let normalize_key_opt value_opt =
  Option.bind value_opt normalize_key

let extract_bearer value =
  let prefix = "Bearer " in
  let prefix_len = String.length prefix in
  if String.length value > prefix_len &&
     String.sub value 0 prefix_len = prefix then
    normalize_key (String.sub value prefix_len (String.length value - prefix_len))
  else None

let extract_api_key headers =
  match normalize_key_opt (header_value_ci headers "x-mcp-api-key") with
  | Some v -> Some v
  | None ->
      match normalize_key_opt (header_value_ci headers "x-api-key") with
      | Some v -> Some v
      | None ->
          Option.bind (header_value_ci headers "authorization") extract_bearer

let expected_api_key env_name =
  match normalize_key_opt (Sys.getenv_opt env_name) with
  | Some v -> Some v
  | None -> None

let constant_time_equal a b =
  let la = String.length a in
  let lb = String.length b in
  if la <> lb then false
  else
    let diff = ref 0 in
    for i = 0 to la - 1 do
      diff := !diff lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !diff = 0

let check_api_key ~env_name ~allow_no_auth headers =
  if allow_no_auth then Ok ()
  else
    match expected_api_key env_name with
    | None -> Error Missing
    | Some expected -> (
        match extract_api_key headers with
        | Some provided when constant_time_equal provided expected -> Ok ()
        | _ -> Error Invalid)
