open Alcotest
open Libsql.Url

let sample_remote_url : Url.url = {
    url = "libsql://ocaml-david-engelmann.turso.io"
  }

let test_sample_remote_url () =
  match sample_remote_url with
  | {url} -> check string "same url" url "libsql://ocaml-david-engelmann.turso.io"

let () =
  Alcotest.run "Url Test Suite" [
    "Url",
    [
    ("test_sample_remote_url", `Quick, test_sample_remote_url);
    ]
  ]
