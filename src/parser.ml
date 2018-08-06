
module Parser = struct
  let is_json lst =
    match lst with
      _ -> true

  let parse_list lst =
    let rec sub ls result = match ls with
      | [] -> result,[]
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.LBRACE :: rest ->
        let (res,r) = sub rest []
        in sub r (Ast.Node(s,res) :: result )
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.INT(i) :: rest -> sub rest (Ast.Pair(s,(Ast.Int i)) :: result)
      | Lexer.COMMA :: rest -> sub rest result
      | Lexer.RBRACE :: rest -> result,rest
      | _ :: rest -> result,rest
    in sub lst []

  let rec parse_token lst = match lst with
      Lexer.LBRACE :: rest -> let res,_ = parse_list rest in Ast.Json (res)
    | _ -> Ast.Json []

end
