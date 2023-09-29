open Alcotest
open Libsql.Path

let sample_local_path : Path.path = {
    path = "file:test"
  }

let test_sample_local_path () =
  match sample_local_path with
  | {path} -> check string "same path" path "file:test"

let () =
  Alcotest.run "Path Test Suite" [
    "Path",
    [
    ("test_sample_local_path", `Quick, test_sample_local_path);
    ]
  ]
