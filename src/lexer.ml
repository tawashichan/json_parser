module Lexer = struct

  type token = LBRACE | RBRACE | STRING of string | LBRACKET | RBRACKET | INT of int | FLOAT of float | QUOTE | SQUOTE | EOF | COLON | COMMA

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  let sam = explode  "{\"hoge\": {\"hugs\": 21,\"haha\": 456},\"taro\":122,\"jiro\": {\"ichiro\" : 44}}"

  let sample_list = ['{';'\"';'m';'\"';':';'\"';'1';'\"';',';'\"';'h';'\"';':';'{';'\"';'t';'\"';':';'6';'}';'}']


  let is_num n = List.mem n ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']

  let get_str lst =
    let rec get_str_sub ls result = match ls with
        '\"' :: rest -> Some result,rest
      | [] -> Some result,[]
      | c :: rest -> get_str_sub rest (result ^ Char.escaped c)
    in (match lst with
          '\"' :: rest -> get_str_sub rest ""
        | _ -> None,lst)

  let get_num lst =
    let rec sub ls result = match ls with
        [] -> Some (int_of_string result),[]
      | c :: rest ->
        if is_num c then sub rest (result ^ Char.escaped c) else Some(int_of_string result),ls
    in sub lst ""

  let rec next_token lst =
    match lst with
      [] -> EOF,[]
    | ' ' :: rest -> next_token rest
    | '{' :: rest -> LBRACE,rest
    | '}' :: rest -> RBRACE,rest
    | '[' :: rest -> LBRACKET,rest
    | ']' :: rest -> RBRACKET,rest
    | ':' :: rest -> COLON,rest
    | ',' :: rest -> COMMA,rest
    | _ as c :: rest -> (
        if is_num c then
          (match get_num lst with
             Some(n),rest -> INT(n),rest
           | None,rest -> EOF,rest)
        else
          (match get_str lst with
            None,rest -> EOF,rest
           | Some(s),rest -> STRING(s),rest
          )
    )

  let char_to_tokens lst =
    let rec sub ls result = match next_token ls with
        EOF,_ -> result
      | tok,rest -> sub rest (tok :: result)
    in List.rev (sub lst [])

  let sample = char_to_tokens sam

end
