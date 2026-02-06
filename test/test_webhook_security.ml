open Alcotest

let test_no_secret_auth_enabled_ok () =
  match Mcp_protocol_eio.validate_webhook_passcode
          ~allow_no_auth:false ~secret_opt:None ~passcode:"" with
  | Ok () -> ()
  | Error msg -> fail msg

let test_no_secret_no_auth_rejected () =
  match Mcp_protocol_eio.validate_webhook_passcode
          ~allow_no_auth:true ~secret_opt:None ~passcode:"" with
  | Ok () -> fail "expected rejection when no-auth is enabled and no secret is configured"
  | Error _ -> ()

let test_secret_requires_passcode () =
  match Mcp_protocol_eio.validate_webhook_passcode
          ~allow_no_auth:false ~secret_opt:(Some "s3cr3t") ~passcode:"" with
  | Ok () -> fail "expected rejection when secret is set but passcode missing"
  | Error msg -> check string "err" "Missing webhook passcode" msg

let test_secret_mismatch_rejected () =
  match Mcp_protocol_eio.validate_webhook_passcode
          ~allow_no_auth:false ~secret_opt:(Some "s3cr3t") ~passcode:"nope" with
  | Ok () -> fail "expected rejection on mismatch"
  | Error msg -> check string "err" "Invalid webhook passcode" msg

let test_secret_match_ok () =
  match Mcp_protocol_eio.validate_webhook_passcode
          ~allow_no_auth:false ~secret_opt:(Some "s3cr3t") ~passcode:"s3cr3t" with
  | Ok () -> ()
  | Error msg -> fail msg

let () =
  run "webhook-security" [
    ("passcode", [
      test_case "no secret, auth enabled ok" `Quick test_no_secret_auth_enabled_ok;
      test_case "no secret, no-auth rejected" `Quick test_no_secret_no_auth_rejected;
      test_case "secret requires passcode" `Quick test_secret_requires_passcode;
      test_case "secret mismatch rejected" `Quick test_secret_mismatch_rejected;
      test_case "secret match ok" `Quick test_secret_match_ok;
    ]);
  ]

