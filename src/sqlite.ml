module Sqlite = struct
  type config = {
    scheme : string;
    authority : string;
    path : string;
    auth_token : string option;
    tls : bool;
  }
end
