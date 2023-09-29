open OUnit2
open Libsql.Path

let sample_local_path : Path.path = {
    path = "file:test"
  }

let test_sample_local_path _ =
  match sample_local_path with
  | {path} -> OUnit2.assert_equal path "file:test"

let suite =
  "suite"
  >::: [
    "test_sample_local_path" >:: test_sample_local_path;
  ]

let () = run_test_tt_main suite
