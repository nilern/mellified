type span = Util.span

type insta = span * Type.gen * Type.t * Type.t
type unif = span * Type.t * Type.t

type t = {instas : insta CCVector.vector; unifs : unif CCVector.vector}

let create () = {instas = CCVector.create (); unifs = CCVector.create ()}

let instantiate {instas; unifs = _} span gen t t' =
    CCVector.push instas (span, gen, t, t')

let unify {instas = _; unifs} span t t' = CCVector.push unifs (span, t, t')

