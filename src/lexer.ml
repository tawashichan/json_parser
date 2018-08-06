module Lexer = struct

  type token = LBRACE | RBRACE | STRING of string | LBRACKET | RBRACKET | INT of int | FLOAT of float | QUOTE | SQUOTE | EOF | COLON | COMMA

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  let sam = explode "{\"hoge\": {\"hugs\": [[21.5],[32.6]],\"haha\": [{\"aaa\":45},{\"BBB\":78}]},\"taro\":[122.6,44.6],\"jiro\": {\"ichiro\" : 44}}"

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

  let get_num_str lst =
    let rec sub ls result is_float = match ls with
        [] -> Some (result),[],is_float
      | c :: rest ->
        if is_num c then sub rest (result ^ Char.escaped c) is_float
        else if c = '.' then
          sub rest (result ^ Char.escaped c) true
        else  Some(result),ls,is_float
    in sub lst "" false

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
          (match get_num_str lst with
             Some(n),rest,is_f -> if is_f then FLOAT(float_of_string n),rest else INT(int_of_string n),rest
           | None,rest,is_f -> EOF,rest)
        else
          (match get_str lst with
            None,rest -> EOF,rest
           | Some(s),rest -> STRING(s),rest
          )
    )

  let chars_to_tokens lst =
    let rec sub ls result = match next_token ls with
        EOF,_ -> result
      | tok,rest -> sub rest (tok :: result)
    in List.rev (sub lst [])

  let sample = chars_to_tokens sam

end
