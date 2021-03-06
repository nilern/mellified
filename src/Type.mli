type flag = IRUtil.flag

type gen =
    | Local of gen
    | Top

and t = private
    | Arrow of {mutable binder : binder; domain : t; codomain : t}
    | Uv of {mutable binder : binder; mutable v : t option}
    | Prim of {mutable binder : binder; p : Prim.t}

and binder =
    | Gen of flag * gen
    | Typ of flag * t

val level : gen -> gen

val uv : gen -> t
val arrow : gen -> t -> t -> t
val prim : gen -> Prim.t -> t

val to_doc : t -> PPrint.document

val of_syn : gen -> Ast.Type.t -> t

val expand : (t -> t -> unit) -> gen -> t -> t -> t
val unify : Util.span -> t -> t -> unit

