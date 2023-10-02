open Alcotest
open Libsql.Auth

let sample_basic_cred_defaults : Auth.auth = { token = "your.test.token" }
let sample_basic_cred : Auth.auth = { token = "another.test.token" }

let sample_http_config_http : Auth.config =
  {
    scheme = "http";
    authority = "localhost";
    path = "local.db";
    auth_token = Some "your.test.token";
    tls = false;
  }

let sample_http_config_https : Auth.config =
  {
    scheme = "https";
    authority = "localhost";
    path = "local.db";
    auth_token = Some "your.test.token";
    tls = true;
  }

let test_sample_basic_cred_defaults () =
  match sample_basic_cred_defaults with
  | { token; _ } -> check string "same token" "your.test.token" token

let test_sample_basic_cred () =
  match sample_basic_cred with
  | { token; _ } -> check string "same token" "another.test.token" token

let test_get_base_url_from_env () =
  let base_url = Auth.get_base_url_from_env in
  check string "same base_url" "0.0.0.0:8000" base_url

let test_get_token_from_env () =
  let token = Auth.get_token_from_env () in
  check (neg string) "token set" "" token

let test_sample_http_config_http_scheme () =
  match sample_http_config_http with
  | { scheme; _ } -> check string "scheme matches" "http" scheme

let test_sample_http_config_http_authority () =
  match sample_http_config_http with
  | { authority; _ } -> check string "authority matches" "localhost" authority

let test_sample_http_config_http_path () =
  match sample_http_config_http with
  | { path; _ } -> check string "path matches" "local.db" path

let test_sample_http_config_http_auth_token () =
  match sample_http_config_http with
  | { auth_token; _ } ->
      check (option string) "auth_token matches" (Some "your.test.token")
        auth_token

let test_sample_http_config_http_tls () =
  match sample_http_config_http with
  | { tls; _ } -> check bool "tls is false" false tls

let test_sample_http_config_https_scheme () =
  match sample_http_config_https with
  | { scheme; _ } -> check string "scheme matches" "https" scheme

let test_sample_http_config_https_authority () =
  match sample_http_config_https with
  | { authority; _ } -> check string "authority matches" "localhost" authority

let test_sample_http_config_https_path () =
  match sample_http_config_https with
  | { path; _ } -> check string "path matches" "local.db" path

let test_sample_http_config_https_auth_token () =
  match sample_http_config_https with
  | { auth_token; _ } ->
      check (option string) "auth_token matches" (Some "your.test.token")
        auth_token

let test_sample_http_config_https_tls () =
  match sample_http_config_https with
  | { tls; _ } -> check bool "tls is true" true tls

let test_validate_http_config_http =
  test_case "validate_http_config with http does not raise an exception" `Quick
    (fun () -> Auth.validate_http_config sample_http_config_http)

let test_validate_http_config_https =
  test_case "validate_http_config with https does not raise an exception" `Quick
    (fun () -> Auth.validate_http_config sample_http_config_https)

let sync_suite =
  [
    ("test_sample_basic_cred_defaults", `Quick, test_sample_basic_cred_defaults);
    ("test_sample_basic_cred", `Quick, test_sample_basic_cred);
    ("test_get_base_url_from_env", `Quick, test_get_base_url_from_env);
    ("test_get_token_from_env", `Quick, test_get_token_from_env);
    ( "test_sample_http_config_http_scheme",
      `Quick,
      test_sample_http_config_http_scheme );
    ( "test_sample_http_config_http_authority",
      `Quick,
      test_sample_http_config_http_authority );
    ( "test_sample_http_config_http_path",
      `Quick,
      test_sample_http_config_http_path );
    ( "test_sample_http_config_http_auth_token",
      `Quick,
      test_sample_http_config_http_auth_token );
    ( "test_sample_http_config_http_tls",
      `Quick,
      test_sample_http_config_http_tls );
    ( "test_sample_http_config_https_scheme",
      `Quick,
      test_sample_http_config_https_scheme );
    ( "test_sample_http_config_https_authority",
      `Quick,
      test_sample_http_config_https_authority );
    ( "test_sample_http_config_https_path",
      `Quick,
      test_sample_http_config_https_path );
    ( "test_sample_http_config_https_auth_token",
      `Quick,
      test_sample_http_config_https_auth_token );
    ( "test_sample_http_config_https_tls",
      `Quick,
      test_sample_http_config_https_tls );
    test_validate_http_config_http;
    test_validate_http_config_https;
  ]

let () =
  print_endline "Running Sync Suite";
  Alcotest.run "Auth Test Suite" [ ("Auth", sync_suite) ]
