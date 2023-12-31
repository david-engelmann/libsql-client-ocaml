open Libsql.Hrana

let create_test_https_config =
  Hrana.parse_url_to_hrana_config
    "libsql://ocaml-david-engelmann.turso.io?tls=1"

let call_get_api_tokens_with_client c = Hrana.get_all_api_tokens c

let call_create_auth_token_with_client c =
  Hrana.create_auth_token c "ocaml-create-auth-token"

let call_get_api_tokens_with_https () =
  let client = Hrana.create_client create_test_https_config in
  print_endline ("Url of client: " ^ Uri.to_string client.url);
  print_endline ("Auth token of client: " ^ Option.get client.config.auth_token);
  let api_tokens = call_get_api_tokens_with_client client in
  api_tokens

let call_create_auth_token_with_https () =
  let client = Hrana.create_client create_test_https_config in
  let api_token = call_create_auth_token_with_client client in
  api_token

let test_get_all_api_tokens_with_https _switch () =
  let open Lwt.Infix in
  print_endline "Running test_get_all_api_tokens_with_https";
  call_get_api_tokens_with_https () >>= fun api_tokens_result ->
  (match api_tokens_result with
  | Ok api_tokens ->
      print_endline ("API tokens: " ^ api_tokens);
      print_endline
        ("Length of API tokens: " ^ string_of_int (String.length api_tokens));
      (*Alcotest.(check (neg string)) "api_tokens returned" "" api_tokens*)
      Alcotest.(check string) "api_tokens returned" "" api_tokens
  | _ -> Alcotest.fail "API tokens retrieval failed");
  Lwt.return_unit

let test_create_auth_token_with_https _switch () =
  let open Lwt.Infix in
  print_endline "Running test_create_auth_token_with_https";
  call_create_auth_token_with_https () >>= fun api_token_result ->
  (match api_token_result with
  | Ok api_token ->
      print_endline ("Created API token: " ^ api_token);
      print_endline
        ("Length of Created API token: "
        ^ string_of_int (String.length api_token));
      (*Alcotest.(check (neg string)) "api_token returned" "" api_token*)
      Alcotest.(check string) "api_token returned" "" api_token
  | _ -> Alcotest.fail "API token retrieval failed");
  Lwt.return_unit

let lwt_suite =
  [
    Alcotest_lwt.test_case "test_get_all_api_tokens_with_https" `Quick
      test_get_all_api_tokens_with_https;
    Alcotest_lwt.test_case "test_create_auth_token_with_https" `Quick
      test_create_auth_token_with_https;
  ]

let () =
  print_endline "Running Async Hrana Suite";
  Lwt_main.run (Alcotest_lwt.run "HranaTestSuiteAsync" [ ("Hrana", lwt_suite) ])
