open Cohttp_client

module Auth = struct
  exception Http_clientError of string * string

  type auth = { token : string }

  type config = {
    scheme : string;
    authority : string;
    path : string;
    auth_token : string option;
    tls : bool;
  }

  type client = { config : config; url : Uri.t }

  let create_client (c : config) : client =
    let url =
      Uri.of_string (String.concat "" [ c.scheme; "://"; c.authority; c.path ])
    in
    { config = c; url }

  let create_auth_from_token (token : string) : auth = { token }

  let create_bearer_auth_header (auth : auth) : string * string =
    ("authorization", "Bearer " ^ auth.token)

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
      try int_of_string port_str with Failure _ -> 8000
    with Not_found -> 8000

  let hostname_from_env : string =
    let hostname =
      try Sys.getenv "LIBSQL_HOST" with Not_found -> "localhost"
    in
    hostname

  let token_from_env () : string =
    let token = try Sys.getenv "LIBSQL_TOKEN" with Not_found -> "" in
    token

  let remove_last_char str =
    let str_len = String.length str in
    if str_len = 0 then str else String.sub str 0 (str_len - 1)

  let get_base_url_from_env : string =
    let hostname = hostname_from_env in
    let hostname =
      if String.get hostname (String.length hostname - 1) = '/' then
        remove_last_char hostname
      else hostname
    in
    let port = port_from_env in
    let hostname = hostname ^ ":" ^ string_of_int port in
    hostname

  let get_token_from_env () : string =
    let token = token_from_env () in
    token

  let create_auth : auth =
    let token = get_token_from_env () in
    { token }

  let parse_url_to_http_config (url_str : string) : config =
    let url = Uri.of_string url_str in
    let scheme = Uri.scheme url |> Option.value ~default:"" in
    let authority = Uri.host_with_default ~default:"" url in
    let path = Uri.path url in
    let initial_auth_token = get_token_from_env () in
    let auth_token =
      if initial_auth_token <> "" then Some initial_auth_token
      else
        match Uri.get_query_param url "authToken" with
        | Some token when token <> "" -> Some token
        | _ -> None
    in
    let tls_value = Uri.get_query_param url "tls" |> Option.value ~default:"" in
    let tls =
      match String.lowercase_ascii tls_value with
      | "1" | "true" -> true
      | "0" | "false" -> false
      | _ -> false
    in
    { scheme; authority; path; auth_token; tls }

  let validate_http_config (config : config) : unit =
    match String.lowercase_ascii config.scheme with
    | "http" when config.tls ->
        raise
          (Http_clientError
             ("A 'http:' URL cannot opt into TLS by using ?tls=1", "URL_INVALID"))
    | "https" when not config.tls ->
        raise
          (Http_clientError
             ( "A 'https:' URL cannot opt out of TLS by using ?tls=0",
               "URL_INVALID" ))
    | "http" | "https" -> ()
    | _ ->
        raise
          (Http_clientError
             ("The scheme must be either 'http' or 'https'", "URL_INVALID"))

  let execute_query (c : client) (stmt : string) (args : (string * string) list)
      =
    if Uri.to_string c.url = "" then
      raise (Http_clientError ("The client URL is empty", "CLIENT_CLOSED"));

    let headers =
      Cohttp_client.create_headers_from_pairs
        [ ("Content-Type", "application/json") ]
    in
    let headers =
      match c.config.auth_token with
      | Some token ->
          Cohttp.Header.add_list headers
            [ create_bearer_auth_header (create_auth_from_token token) ]
      | None -> headers
    in
    let query_params = [ ("stmt", stmt) ] @ args in
    let uri_with_query = Uri.with_query' c.url query_params in

    Lwt.bind
      (Cohttp_client.get_request_with_headers
         (Uri.to_string uri_with_query)
         headers)
      (fun body -> Lwt.return (Ok body))

  let close_client client = { client with url = Uri.of_string "" }
end
