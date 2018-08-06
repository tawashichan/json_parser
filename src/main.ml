module Main = struct
    let run() =
    let str = read_line()
    in let tokens = Lexer.chars_to_tokens(Lexer.explode(str))
    in Parser.parse_tokens tokens
end
