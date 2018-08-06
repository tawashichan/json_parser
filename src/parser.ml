
module Parser = struct

  let parse_array lst = match lst with
      _ :: rest -> [],rest
    | _ -> [],[]

  let parse_list lst =
    let rec sub ls result = match ls with
        Lexer.STRING(s) :: Lexer.COLON ::Lexer.LBRACE :: rest ->
        let (r,res) = sub rest []
        in sub r ((s,res) :: result)
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.INT(i) :: rest -> sub rest ((s,Ast.Int i) :: result)
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.FLOAT(f) :: rest -> sub rest ((s,Ast.Float f) :: result)
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.STRING(v) :: rest -> sub rest ((s,Ast.String v) :: result)
      | Lexer.RBRACE :: rest -> rest,Ast.Assoc result
      | Lexer.COMMA :: rest -> sub rest result
      | _ -> [],Ast.Assoc result
    in sub lst []

  let rec parse_token lst = match lst with
      Lexer.LBRACE :: rest -> let (res,json) = parse_list rest in json
    | _ -> Ast.Assoc [("aaa",Ast.String("ddd"))]

end
