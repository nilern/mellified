let solve (constr : Constraint.t) =
    let unifs = Queue.create () in
    constr.unifs |> CCVector.iter (Fun.flip Queue.add unifs);
    let instas = Queue.create () in
    constr.instas |> CCVector.iter (Fun.flip Queue.add instas);
