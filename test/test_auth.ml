open OUnit2
open Libsql.Auth

let sample_basic_cred_defaults : Auth.basic_cred = {
   username = "root";
   password = "";
  }

let sample_basic_cred : Auth.basic_cred = {
   username = "libsql";
   password = "julyjackson";
  }

let test_sample_basic_cred_defaults _ =
  match sample_basic_cred_defaults with
   | { username; _ } ->
      OUnit2.assert_equal "root" username

let test_sample_basic_cred_username _ =
  match sample_basic_cred with
   | { username; _ } ->
      OUnit2.assert_equal "libsql" username

let test_sample_basic_cred_password _ =
  match sample_basic_cred with
   | { password; _ } ->
      OUnit2.assert_equal "julyjackson" password

let test_get_base_url_from_env _ =
    let base_url = Auth.get_base_url_from_env in
    Printf.printf "Base Url: %s\n" base_url;
    OUnit2.assert_equal "localhost:8000" base_url

let suite =
  "suite"
  >::: [
         "test_sample_basic_cred_defaults" >:: test_sample_basic_cred_defaults;
         "test_sample_basic_cred_username" >:: test_sample_basic_cred_username;
         "test_sample_basic_cred_password" >:: test_sample_basic_cred_password;
         "test_get_base_url_from_env" >:: test_get_base_url_from_env;
       ]

let () = run_test_tt_main suite
