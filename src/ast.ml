module Ast = struct
  type jsonValue  = String of string | Int of int | Float of float | List of jsonValue list
  type mapping = Pair of string * jsonValue | Node of string * mapping list
  type json = Json of mapping list
end
