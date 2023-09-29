open Alcotest
open Libsql.Auth

let sample_basic_cred_defaults : Auth.basic_cred = {
   username = "root";
   password = "";
  }

let sample_basic_cred : Auth.basic_cred = {
   username = "libsql";
   password = "julyjackson";
  }

let test_sample_basic_cred_defaults () =
  match sample_basic_cred_defaults with
   | { username; _ } ->
      check string "same username" "root" username

let test_sample_basic_cred_username () =
  match sample_basic_cred with
   | { username; _ } ->
      check string "same username" "libsql" username

let test_sample_basic_cred_password () =
  match sample_basic_cred with
   | { password; _ } ->
    check string "same password" "julyjackson" password

let test_get_base_url_from_env () =
    let base_url = Auth.get_base_url_from_env in
    check string "same base_url" "localhost:8000" base_url

let () =
  Alcotest.run "Auth Test Suite" [
    "Auth",
    [
    ("test_sample_basic_cred_defaults", `Quick, test_sample_basic_cred_defaults);
    ("test_sample_basic_cred_username", `Quick, test_sample_basic_cred_username);
    ("test_sample_basic_cred_password", `Quick, test_sample_basic_cred_password);
    ("test_get_base_url_from_env", `Quick, test_get_base_url_from_env);
    ]
  ]
