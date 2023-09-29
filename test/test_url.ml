
open OUnit2
open Libsql.Url

let sample_remote_url : Url.url = {
    url = "libsql://ocaml-david-engelmann.turso.io"
  }

let test_sample_remote_url _ =
  match sample_remote_url with
  | {url} -> OUnit2.assert_equal url "libsql://ocaml-david-engelmann.turso.io"

let suite =
  "suite"
  >::: [
    "test_sample_remote_url" >:: test_sample_remote_url;
  ]

let () = run_test_tt_main suite
