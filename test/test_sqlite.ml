open Alcotest
open Libsql.Sqlite

let sample_basic_sqlite_config : Sqlite.config =
  { scheme = "file"; authority = "localhost"; path = "local.db" }

let test_sample_basic_sqlite_config_scheme () =
  match sample_basic_sqlite_config with
  | { scheme; _ } -> check string "same scheme" "file" scheme

let test_sample_basic_sqlite_config_authority () =
  match sample_basic_sqlite_config with
  | { authority; _ } -> check string "same authority" "localhost" authority

let test_sample_basic_sqlite_config_path () =
  match sample_basic_sqlite_config with
  | { path; _ } -> check string "same path" "local.db" path

let test_validate_sqlite_config =
  test_case "validate_sqlite_config with file does not raise an exception"
    `Quick (fun () -> Sqlite.validate_sqlite_config sample_basic_sqlite_config)

let test_execute_query =
  test_case "execute_query does not raise an exception" `Quick (fun () ->
      let config = Sqlite.parse_url_to_sqlite_config "file:test.db" in
      let c = Sqlite.open_client config in
      Sqlite.execute_query c
        "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, name TEXT \
         NOT NULL);")

let () =
  Alcotest.run "Sqlite Test Suite"
    [
      ( "Sqlite",
        [
          ( "test_sample_basic_sqlite_config_scheme",
            `Quick,
            test_sample_basic_sqlite_config_scheme );
          ( "test_sample_basic_sqlite_config_authority",
            `Quick,
            test_sample_basic_sqlite_config_authority );
          ( "test_sample_basic_sqlite_config_path",
            `Quick,
            test_sample_basic_sqlite_config_path );
          test_validate_sqlite_config;
          test_execute_query;
        ] );
    ]
