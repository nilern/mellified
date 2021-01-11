module L = SedlexMenhir

let letter = [%sedlex.regexp? ll | lm | lo | lt | lu | pc]
let number = [%sedlex.regexp? nd | nl | no]

let initial = [%sedlex.regexp? letter | '_']
let constituent = [%sedlex.regexp? initial | number]

let identifier = [%sedlex.regexp? initial, Star constituent]

let string = [%sedlex.regexp? '"', Star (Compl '"'), '"']
let integer = [%sedlex.regexp? Plus ('0'..'9')]

let rec token ({SedlexMenhir.stream; _} as lexbuf) =
    match%sedlex stream with
    | Plus (Chars " \t\r") -> L.update lexbuf; token lexbuf 
    | '\n' -> L.update lexbuf; L.new_line lexbuf; token lexbuf
    | "(*)", Star (Compl '\n') -> L.update lexbuf; token lexbuf
    | eof -> L.update lexbuf; Parser.EOF

    | '='  -> L.update lexbuf; EQ

    | "val" -> L.update lexbuf; VAL
    | "do" -> L.update lexbuf; DO

    | string ->
        let tok = L.lexeme lexbuf in
        L.update lexbuf; STRING (String.sub tok 1 (String.length tok - 2))
    | integer -> L.update lexbuf; INT (int_of_string (L.lexeme lexbuf))

    | identifier -> L.update lexbuf; ID (L.lexeme lexbuf)

    | _ -> L.raise_ParseError lexbuf

