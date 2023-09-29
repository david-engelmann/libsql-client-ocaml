open Cohttp_client
open Jose.Jwt

module Auth = struct
  type auth =
    {
      exp : int;
      iat : int;
      iss : string;
      token : string;
      preferred_username : string;
    }

  type basic_cred =
    {
      username : string;
      password : string;
    }

  let cred_to_string (cred : basic_cred) : string =
    let cred_data = `Assoc [ ("username", `String cred.username); ("password", `String cred.password)]
    in
    Yojson.Basic.to_string cred_data

  let create_basic_auth_header (cred : basic_cred) : (string * string) =
    ("authorization", ("Basic " ^ cred.username ^ ":" ^ cred.password))

  let create_bearer_auth_header (auth : auth) : (string * string) =
    ("authorization", ("Bearer " ^ auth.token))

  let create_basic_cred (username : string) (password : string) : basic_cred =
    { username; password }

  let make_auth_token_request (cred : basic_cred) (base_url : string) : string =
    let url = Printf.sprintf "%s/_open/auth" base_url in
    (*let data = cred_to_string cred in*)
    let data = Printf.sprintf "{\"username\":\"%s\",\"password\":\"%s\"}" cred.username cred.password in
    Printf.printf "url: %s\n" url;
    Printf.printf "data: %s\n" data;
    let body = Lwt_main.run (Cohttp_client.post_data url data) in
    body

  let make_auth_token_refresh (cred : basic_cred) (base_url : string) : string =
    let url = Printf.sprintf "%s/_admin/server/jwt" base_url in
    (*let data = cred_to_string cred in*)
    let data = Printf.sprintf "{\"username\":\"%s\",\"password\":\"%s\"}" cred.username cred.password in
    Printf.printf "url: %s\n" url;
    Printf.printf "data: %s\n" data;
    let body = Lwt_main.run (Cohttp_client.post_data url data) in
    body

  let parse_auth json : auth =
    let open Yojson.Safe.Util in
    let token = json |> member "jwt" |> to_string in
    match unsafe_of_string token with
    | Ok jwt ->
      let claims = jwt.payload in
      let exp = claims |> member "exp" |> to_int in
      let iat = claims |> member "iat" |> to_int in
      let iss = claims |> member "iss" |> to_string in
      let preferred_username = claims |> member "preferred_username" |> to_string in
      { exp; iat; iss; token; preferred_username; }
    | Error _ -> failwith "Invalid JWT token"

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let username_and_password_from_env : basic_cred =
    let username = try Sys.getenv "LIBSQL_USER" with Not_found -> "root" in
    let password = try Sys.getenv "LIBSQL_PASSWORD" with Not_found -> "" in
    { username; password }

    let port_from_env : int =
      try
          let port_str = Sys.getenv "LIBSQL_PORT" in
          try
              int_of_string port_str
          with Failure _ -> 8000
      with Not_found -> 8000

  let hostname_from_env : string =
    let hostname = try Sys.getenv "LIBSQL_HOST" with Not_found -> "localhost" in
    hostname

  let remove_last_char str =
    let str_len = String.length str in
    if str_len = 0 then str else String.sub str 0 (str_len - 1)

  let get_base_url_from_env : string =
    let hostname = hostname_from_env in
    let hostname = if String.get hostname (String.length hostname - 1) = '/' then remove_last_char hostname else hostname in
    let port = port_from_env in
    let hostname = hostname ^ ":" ^ (string_of_int port) in
    hostname

  let is_token_expired (a : auth) : bool =
    let expired_at = Ptime.of_float_s (float_of_int a.exp) |> Option.get in
    let expired_at = Ptime.add_span expired_at (Ptime.Span.of_int_s (-1 * 60)) |> Option.get in
    let datatime_now = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
    Ptime.is_later ~than:expired_at datatime_now

end
