type span = Util.span

type t

val create : unit -> t
val instantiate : t -> span -> Type.gen -> Type.t -> Type.t -> unit
val unify : t -> span -> Type.t -> Type.t -> unit

