open Alcotest
open Libsql.Hrana

let sample_hrana_config : Hrana.config =
  {
    scheme = "ws";
    authority = "localhost";
    path = "local.db";
    auth_token = Some "your.test.token";
    tls = false;
  }

let test_sample_hrana_config_scheme () =
  match sample_hrana_config with
  | { scheme; _ } -> check string "scheme matches" "ws" scheme

let test_sample_hrana_config_authority () =
  match sample_hrana_config with
  | { authority; _ } -> check string "authority matches" "localhost" authority

let test_sample_hrana_config_path () =
  match sample_hrana_config with
  | { path; _ } -> check string "path matches" "local.db" path

let test_sample_hrana_config_auth_token () =
  match sample_hrana_config with
  | { auth_token; _ } ->
      check (option string) "auth_token matches" (Some "your.test.token")
        auth_token

let test_sample_hrana_config_tls () =
  match sample_hrana_config with
  | { tls; _ } -> check bool "tls is false" false tls

let () =
  Alcotest.run "Hrana Test Suite"
    [
      ( "Hrana",
        [
          ( "test_sample_hrana_config_scheme",
            `Quick,
            test_sample_hrana_config_scheme );
          ( "test_sample_hrana_config_authority",
            `Quick,
            test_sample_hrana_config_authority );
          ( "test_sample_hrana_config_path",
            `Quick,
            test_sample_hrana_config_path );
          ( "test_sample_hrana_config_auth_token",
            `Quick,
            test_sample_hrana_config_auth_token );
          ("test_sample_hrana_config_tls", `Quick, test_sample_hrana_config_tls);
        ] );
    ]
