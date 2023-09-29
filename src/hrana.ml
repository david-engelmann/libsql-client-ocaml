open Auth

module Hrana = struct
  exception HranaError of string * string

  type config =
  {
    scheme : string;
    authority : string;
    path : string;
    auth_token : string option;
    tls : bool;
  }


  let parse_url_to_hrana_config (url_str : string) : config =
    let url = Uri.of_string url_str in
    let scheme = Uri.scheme url |> Option.value ~default:"" in
    let authority = Uri.host_with_default ~default:"" url in
    let path = Uri.path url in
    let initial_auth_token = Auth.get_token_from_env in
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

  let validate_hrana_config (config: config) : unit =
    match String.lowercase_ascii config.scheme with
    | "http" when config.tls ->
      raise (HranaError ("A 'http:' URL cannot opt into TLS by using ?tls=1", "URL_INVALID"))
    | "https" when not config.tls ->
      raise (HranaError ("A 'https:' URL cannot opt out of TLS by using ?tls=0", "URL_INVALID"))
    | "ws" when config.tls ->
      raise (HranaError ("A 'ws:' URL cannot opt into TLS by using ?tls=1", "URL_INVALID"))
    | "wss" when not config.tls ->
      raise (HranaError ("A 'wss:' URL cannot opt out of TLS by using ?tls=0", "URL_INVALID"))
    | "http" | "https" | "ws" | "wss" -> ()
    | _ -> raise (HranaError ("The scheme must be either 'http', 'https', 'ws', or 'wss'", "URL_INVALID"))
end