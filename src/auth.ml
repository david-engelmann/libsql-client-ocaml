(*open Cohttp_client*)

module Auth = struct
  type auth =
    {
      token : string;
    }

  let create_bearer_auth_header (auth : auth) : (string * string) =
    ("authorization", ("Bearer " ^ auth.token))


  let parse_auth json : auth =
    let open Yojson.Safe.Util in
    let token = json |> member "token" |> to_string in
    { token }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

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

  let token_from_env : string =
    let token = try Sys.getenv "LIBSQL_TOKEN" with Not_found -> "" in
    token

  let remove_last_char str =
    let str_len = String.length str in
    if str_len = 0 then str else String.sub str 0 (str_len - 1)

  let get_base_url_from_env : string =
    let hostname = hostname_from_env in
    let hostname = if String.get hostname (String.length hostname - 1) = '/' then remove_last_char hostname else hostname in
    let port = port_from_env in
    let hostname = hostname ^ ":" ^ (string_of_int port) in
    hostname

  let get_token_from_env : string =
    let token = token_from_env in
    token

end
