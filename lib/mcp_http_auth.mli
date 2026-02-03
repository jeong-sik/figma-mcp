type api_key_error =
  | Missing
  | Invalid

val env_truthy : string -> bool
val extract_api_key : Httpun.Headers.t -> string option
val check_api_key :
  env_name:string ->
  allow_no_auth:bool ->
  Httpun.Headers.t ->
  (unit, api_key_error) result
