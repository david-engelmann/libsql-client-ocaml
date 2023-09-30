open Sqlite3

module Sqlite = struct
  exception SqliteError of string * string

  type config = { scheme : string; authority : string; path : string }
  type client = db

  let parse_url_to_sqlite_config (url_str : string) : config =
    let url = Uri.of_string url_str in
    let scheme = Uri.scheme url |> Option.value ~default:"" in
    let authority = Uri.host_with_default ~default:"" url in
    let path = Uri.path url in
    { scheme; authority; path }

  let validate_sqlite_config (config : config) : unit =
    match String.lowercase_ascii config.scheme with
    | "file"
      when not
             (config.authority = ""
             || String.lowercase_ascii config.authority = "localhost") ->
        raise
          (SqliteError
             ( Printf.sprintf "Invalid authority in file URL: %s"
                 config.authority,
               "URL_INVALID" ))
    | "file" -> ()
    | _ -> raise (SqliteError ("The scheme must be 'file'", "URL_INVALID"))

  let open_client (config : config) : client =
    validate_sqlite_config config;
    db_open config.path

  let execute_query (c : client) (query : string) : unit =
    match exec c query with
    | Rc.OK -> ()
    | error ->
        raise
          (SqliteError
             ( Printf.sprintf "Query failed with error: %s" (Rc.to_string error),
               "QUERY_ERROR" ))
end
