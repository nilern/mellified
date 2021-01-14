type insta = Type.gen * Type.t * Type.t
type unif = Type.t * Type.t

type t = {instas : insta CCVector.vector; unifs : unif CCVector.vector}

let create () = {instas = CCVector.create (); unifs = CCVector.create ()}

let instantiate {instas; unifs = _} gen t t' = CCVector.push instas (gen, t, t')

let unify {instas = _; unifs} t t' = CCVector.push unifs (t, t')

