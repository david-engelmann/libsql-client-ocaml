open Alcotest
open Libsql.Hrana

let sample_hrana_config_ws : Hrana.config =
  {
    scheme = "ws";
    authority = "localhost";
    path = "local.db";
    auth_token = Some "your.test.token";
    tls = false;
  }

let sample_hrana_config_wss : Hrana.config =
  {
    scheme = "wss";
    authority = "localhost";
    path = "local.db";
    auth_token = Some "your.test.token";
    tls = true;
  }

let test_sample_hrana_config_ws_scheme () =
  match sample_hrana_config_ws with
  | { scheme; _ } -> check string "scheme matches" "ws" scheme

let test_sample_hrana_config_ws_authority () =
  match sample_hrana_config_ws with
  | { authority; _ } -> check string "authority matches" "localhost" authority

let test_sample_hrana_config_ws_path () =
  match sample_hrana_config_ws with
  | { path; _ } -> check string "path matches" "local.db" path

let test_sample_hrana_config_ws_auth_token () =
  match sample_hrana_config_ws with
  | { auth_token; _ } ->
      check (option string) "auth_token matches" (Some "your.test.token")
        auth_token

let test_sample_hrana_config_ws_tls () =
  match sample_hrana_config_ws with
  | { tls; _ } -> check bool "tls is false" false tls

let test_sample_hrana_config_wss_scheme () =
  match sample_hrana_config_wss with
  | { scheme; _ } -> check string "scheme matches" "wss" scheme

let test_sample_hrana_config_wss_authority () =
  match sample_hrana_config_wss with
  | { authority; _ } -> check string "authority matches" "localhost" authority

let test_sample_hrana_config_wss_path () =
  match sample_hrana_config_wss with
  | { path; _ } -> check string "path matches" "local.db" path

let test_sample_hrana_config_wss_auth_token () =
  match sample_hrana_config_wss with
  | { auth_token; _ } ->
      check (option string) "auth_token matches" (Some "your.test.token")
        auth_token

let test_sample_hrana_config_wss_tls () =
  match sample_hrana_config_wss with
  | { tls; _ } -> check bool "tls is true" true tls

let test_validate_hrana_config_ws =
  test_case "validate_hrana_config with ws does not raise an exception" `Quick
    (fun () -> Hrana.validate_hrana_config sample_hrana_config_ws)

let test_validate_hrana_config_wss =
  test_case "validate_hrana_config with wss does not raise an exception" `Quick
    (fun () -> Hrana.validate_hrana_config sample_hrana_config_wss)

let () =
  Alcotest.run "Hrana Test Suite"
    [
      ( "Hrana",
        [
          ( "test_sample_hrana_config_ws_scheme",
            `Quick,
            test_sample_hrana_config_ws_scheme );
          ( "test_sample_hrana_config_ws_authority",
            `Quick,
            test_sample_hrana_config_ws_authority );
          ( "test_sample_hrana_config_ws_path",
            `Quick,
            test_sample_hrana_config_ws_path );
          ( "test_sample_hrana_config_ws_auth_token",
            `Quick,
            test_sample_hrana_config_ws_auth_token );
          ( "test_sample_hrana_config_ws_tls",
            `Quick,
            test_sample_hrana_config_ws_tls );
          ( "test_sample_hrana_config_wss_scheme",
            `Quick,
            test_sample_hrana_config_wss_scheme );
          ( "test_sample_hrana_config_wss_authority",
            `Quick,
            test_sample_hrana_config_wss_authority );
          ( "test_sample_hrana_config_wss_path",
            `Quick,
            test_sample_hrana_config_wss_path );
          ( "test_sample_hrana_config_wss_auth_token",
            `Quick,
            test_sample_hrana_config_wss_auth_token );
          ( "test_sample_hrana_config_wss_tls",
            `Quick,
            test_sample_hrana_config_wss_tls );
          test_validate_hrana_config_ws;
          test_validate_hrana_config_wss;
        ] );
    ]
