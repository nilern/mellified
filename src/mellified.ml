let prompt = "mellified> "

let rec repl () =
    match LNoise.linenoise prompt with
    | Some input ->
        (try begin
            ignore (LNoise.history_add input);
            print_endline input;
            let lexbuf = Sedlexing.Utf8.from_string input
                |> SedlexMenhir.create_lexbuf "REPL input" in
            let _ = SedlexMenhir.sedlex_with_menhir Lexer.token Parser.stmts lexbuf in
            ()
        end with SedlexMenhir.ParseError err ->
            print_endline @@ SedlexMenhir.string_of_ParseError err);
        repl ()
    | None -> ()

let repl_t =
    let doc = "Mellified REPL" in
    Cmdliner.Term.(const repl $ const (), info "mellified" ~doc)

let () = Cmdliner.Term.(exit (eval repl_t))

