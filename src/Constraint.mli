type t

val create : unit -> t
val instantiate : t -> Type.gen -> Type.t -> Type.t -> unit
val unify : t -> Type.t -> Type.t -> unit

