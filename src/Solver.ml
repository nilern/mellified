let solve ({Constraint.instas; unifs} as constr) =
    let rec solve () =
        match Queue.take_opt unifs with
        | Some (span, t, t') ->
            Type.unify span t t';
            solve ()
        | None ->
            (match Queue.take_opt instas with
            | Some (span, gen, t, t') ->
                let t = Type.expand (Constraint.unify constr span) gen t t' in
                Constraint.unify constr span t t';
                solve ()
            | None -> ()) in
    solve ()

