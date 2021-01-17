type span = Util.span

type insta = span * Type.gen * Type.t * Type.t
type unif = span * Type.t * Type.t

type t = {instas : insta Queue.t; unifs : unif Queue.t}

let create () = {instas = Queue.create (); unifs = Queue.create ()}

let instantiate {instas; unifs = _} span gen t t' =
    Queue.push (span, gen, t, t') instas

let unify {instas = _; unifs} span t t' = Queue.push (span, t, t') unifs

