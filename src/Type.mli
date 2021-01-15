type flag = Flex | Rigid

type gen =
    | Local of gen
    | Top

and t = private
    | Arrow of {mutable binder : binder; domain : t; codomain : t}
    | Uv of {mutable binder : binder; mutable v : t option}
    | Prim of Prim.t

and binder =
    | Gen of flag * gen
    | Typ of flag * t

val level : gen -> gen

val uv : gen -> t
val arrow : gen -> t -> t -> t
val prim : Prim.t -> t

val to_doc : t -> PPrint.document

type syn =
    | SynForAll of Name.t * flag * syn * syn
    | SynVar of Name.t
    | SynArrow of {domain : syn; codomain : syn}
    | SynWild
    | SynPrim of Prim.t

val of_syn : gen -> syn -> t

