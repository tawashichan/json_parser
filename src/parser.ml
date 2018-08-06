
module Parser = struct

  let parse_array lst = match lst with
      _ :: rest -> [],rest
    | _ -> [],[]

  let parse_list lst =
    let rec sub ls result = match ls with
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.LBRACKET :: rest ->
        let rec sub_ary re acm =
          match re with
            Lexer.RBRACKET :: r -> r,Ast.List acm
          | _ ->
            let (r,res) = sub re []
            in sub_ary r (res :: acm)
        in let (rr,l) = sub_ary rest [] (* ここでAst.Listを返す *)
        in sub rr ((s,l) :: result)
      | Lexer.STRING(s) :: Lexer.COLON ::Lexer.LBRACE :: rest ->
        let (r,res) = sub rest []
        in sub r ((s,res) :: result)
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.INT(i) :: rest -> sub rest ((s,Ast.Int i) :: result)
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.FLOAT(f) :: rest -> sub rest ((s,Ast.Float f) :: result)
      | Lexer.STRING(s) :: Lexer.COLON :: Lexer.STRING(v) :: rest -> sub rest ((s,Ast.String v) :: result)
      | Lexer.RBRACE :: rest -> rest,Ast.Assoc result
      | Lexer.COMMA :: rest -> sub rest result
      | Lexer.INT(i) :: rest -> rest,Ast.Int i
      | Lexer.STRING(s) :: rest -> rest,Ast.String s
      | Lexer.FLOAT(f) :: rest -> rest,Ast.Float f
      | Lexer.LBRACE :: rest ->
        let (r,res) = sub rest []
        in r,res
      | Lexer.LBRACKET :: rest ->
        let rec sub_ary re acm =
          match re with
            Lexer.RBRACKET :: r -> r,Ast.List acm
          | _ ->
            let (r,res) = sub re []
            in sub_ary r (res :: acm)
        in let (rr,l) = sub_ary rest [] (* ここでAst.Listを返す *)
        in rr,l
      | _ -> [],Ast.Assoc result
    in sub lst []

  let rec parse_tokens lst = match lst with
      Lexer.LBRACE :: rest -> let (res,json) = parse_list rest in json
    | _ -> Ast.Assoc [("foo",Ast.Null)]

end
