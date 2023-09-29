open Alcotest
open Libsql.Auth
open Libsql.Cohttp_client

let sample_basic_cred_defaults : Auth.auth = { token = "your.test.token" }
let sample_basic_cred : Auth.auth = { token = "another.test.token" }

let sample_cohttp_config : Cohttp_client.config =
  {
    scheme = "https";
    authority = "localhost";
    path = "local.db";
    auth_token = Some "your.test.token";
    tls = false;
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
  let token = Auth.get_token_from_env in
  check (neg string) "token set" "" token

let test_sample_cohttp_config_scheme () =
  match sample_cohttp_config with
  | { scheme; _ } -> check string "scheme matches" "https" scheme

let test_sample_cohttp_config_authority () =
  match sample_cohttp_config with
  | { authority; _ } -> check string "authority matches" "localhost" authority

let test_sample_cohttp_config_path () =
  match sample_cohttp_config with
  | { path; _ } -> check string "path matches" "local.db" path

let test_sample_cohttp_config_auth_token () =
  match sample_cohttp_config with
  | { auth_token; _ } ->
      check (option string) "auth_token matches" (Some "your.test.token")
        auth_token

let test_sample_cohttp_config_tls () =
  match sample_cohttp_config with
  | { tls; _ } -> check bool "tls is false" false tls

let () =
  Alcotest.run "Auth Test Suite"
    [
      ( "Auth",
        [
          ( "test_sample_basic_cred_defaults",
            `Quick,
            test_sample_basic_cred_defaults );
          ("test_sample_basic_cred", `Quick, test_sample_basic_cred);
          ("test_get_base_url_from_env", `Quick, test_get_base_url_from_env);
          ("test_get_token_from_env", `Quick, test_get_token_from_env);
          ( "test_sample_cohttp_config_scheme",
            `Quick,
            test_sample_cohttp_config_scheme );
          ( "test_sample_cohttp_config_authority",
            `Quick,
            test_sample_cohttp_config_authority );
          ( "test_sample_cohttp_config_path",
            `Quick,
            test_sample_cohttp_config_path );
          ( "test_sample_cohttp_config_auth_token",
            `Quick,
            test_sample_cohttp_config_auth_token );
          ( "test_sample_cohttp_config_tls",
            `Quick,
            test_sample_cohttp_config_tls );
        ] );
    ]
