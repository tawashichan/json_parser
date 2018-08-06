module Ast = struct

  type json =
      Assoc of (string * json) list
    | String of string
    | Int of int
    | Float of float
    | Null
    | List of json list

  let sample =
    Assoc [("hoge",List [Assoc[("huga",Int 123);("tekitou",String "aaaa")];Null;Int(22)])]
end
