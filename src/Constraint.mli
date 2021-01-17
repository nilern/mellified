type span = Util.span

type insta = span * Type.gen * Type.t * Type.t
type unif = span * Type.t * Type.t

type t = {instas : insta CCVector.vector; unifs : unif CCVector.vector}

val create : unit -> t
val instantiate : t -> span -> Type.gen -> Type.t -> Type.t -> unit
val unify : t -> span -> Type.t -> Type.t -> unit

