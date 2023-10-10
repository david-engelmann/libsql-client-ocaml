open Libsql.Auth

let create_test_https_config =
  Auth.parse_url_to_http_config "https://ocaml-david-engelmann.turso.io?tls=1"

let call_get_api_tokens_with_client c = Auth.get_all_api_tokens c

let call_create_auth_token_with_client c =
  Auth.create_auth_token c "ocaml-create-auth-token"

let call_get_health_with_client c = Auth.get_health c

let call_get_api_tokens_with_https () =
  let client = Auth.create_client create_test_https_config in
  print_endline ("Url of client: " ^ Uri.to_string client.url);
  print_endline ("Auth token of client: " ^ Option.get client.config.auth_token);
  let api_tokens = call_get_api_tokens_with_client client in
  api_tokens

let call_create_auth_token_with_https () =
  let client = Auth.create_client create_test_https_config in
  let api_token = call_create_auth_token_with_client client in
  api_token

let call_get_health_wtih_https () =
  let client = Auth.create_client create_test_https_config in
  let health = call_get_health_with_client client in
  health

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

let test_get_health_with_https _switch () =
  let open Lwt.Infix in
  print_endline "Running test_create_auth_token_with_https";
  call_get_health_wtih_https () >>= fun health_request ->
  (match health_request with
  | Ok health ->
      print_endline ("Health Response: " ^ health);
      (*Alcotest.(check (neg string)) "api_token returned" "" api_token*)
      Alcotest.(check string) "Health returned" "" health
  | _ -> Alcotest.fail "Health retrieval failed");
  Lwt.return_unit

let lwt_suite =
  [
    Alcotest_lwt.test_case "test_get_all_api_tokens_with_https" `Quick
      test_get_all_api_tokens_with_https;
    Alcotest_lwt.test_case "test_create_auth_token_with_https" `Quick
      test_create_auth_token_with_https;
    Alcotest_lwt.test_case "test_get_health_with_https" `Quick
      test_get_health_with_https;
  ]

let () =
  print_endline "Running Https Auth Async Suite";
  Lwt_main.run
    (Alcotest_lwt.run "HttpsAuthTestSuiteAsync" [ ("HttpsAuth", lwt_suite) ])
