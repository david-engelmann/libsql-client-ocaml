open Alcotest
open Libsql.Auth

let sample_basic_cred_defaults : Auth.auth = {
    token = "your.test.token"
  }

let sample_basic_cred : Auth.auth = {
    token = "another.test.token"
  }

let test_sample_basic_cred_defaults () =
  match sample_basic_cred_defaults with
   | { token; _ } ->
      check string "same token" "your.test.token" token

let test_sample_basic_cred () =
  match sample_basic_cred with
   | { token; _ } ->
      check string "same token" "another.test.token" token

let test_get_base_url_from_env () =
    let base_url = Auth.get_base_url_from_env in
    check string "same base_url" "0.0.0.0:8000" base_url

let test_get_token_from_env () =
  let token = Auth.get_token_from_env in
  check (neg string) "token set" "" token

let () =
  Alcotest.run "Auth Test Suite" [
    "Auth",
    [
    ("test_sample_basic_cred_defaults", `Quick, test_sample_basic_cred_defaults);
    ("test_sample_basic_cred", `Quick, test_sample_basic_cred);
    ("test_get_base_url_from_env", `Quick, test_get_base_url_from_env);
    ("test_get_token_from_env", `Quick, test_get_token_from_env);
    ]
  ]
