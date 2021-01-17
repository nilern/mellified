module Vec = CCImmutArray
let (%) = CCFun.(%)

module T = Type

let pprint = Util.pprint

let prompt = "mellified> "

let rec repl top =
    match LNoise.linenoise prompt with
    | Some input ->
        (try begin
            ignore (LNoise.history_add input);
            let lexbuf = Sedlexing.Utf8.from_string input
                |> SedlexMenhir.create_lexbuf "REPL input" in
            let stmts = SedlexMenhir.sedlex_with_menhir Lexer.token Parser.stmts lexbuf in
            Vec.iter (Util.pprint % Ast.Stmt.to_doc) stmts;
            Constrain.constrain stmts
            |> Solver.solve
        end with SedlexMenhir.ParseError err ->
            print_endline @@ SedlexMenhir.string_of_ParseError err);
        repl top
    | None -> ()

let repl_t =
    let doc = "Mellified REPL" in
    Cmdliner.Term.(const repl $ const Type.Top, info "mellified" ~doc)

let () = Cmdliner.Term.(exit (eval repl_t))

