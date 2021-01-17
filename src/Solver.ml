let solve ~top (constr : Constraint.t) =
    let unifs = Queue.create () in
    constr.unifs |> CCVector.iter (Fun.flip Queue.add unifs);
    let instas = Queue.create () in
    constr.instas |> CCVector.iter (Fun.flip Queue.add instas);

    let rec solve () =
        match Queue.take_opt unifs with
        | Some (span, t, t') ->
            Type.unify span t t';
            solve ()
        | None ->
            (match Queue.take_opt instas with
            | Some (span, gen, t, t') ->
                let t = Type.expand ~top gen t t' (fun t t' ->
                    Queue.add (span, t, t') unifs
                ) in
                Queue.add (span, t, t') unifs;
                solve ()
            | None -> ()) in
    solve ()

