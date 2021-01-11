let prompt = "mellified> "

let rec repl () =
    match LNoise.linenoise prompt with
    | Some input ->
        ignore (LNoise.history_add input);
        print_endline input;
        repl ()
    | None -> ()

let repl_t =
    let doc = "Mellified REPL" in
    Cmdliner.Term.(const repl $ const (), info "mellified" ~doc)

let () = Cmdliner.Term.(exit (eval repl_t))

