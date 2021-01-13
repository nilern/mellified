type flag = Flex | Rigid

type gen = {mutable binder : gen option; typs : t CCVector.vector}

and t =
    | Arrow of {mutable binder : binder; domain : t; codomain : t}
    | Uv of {mutable binder : binder; mutable v : t option}
    | Prim of Prim.t

and binder =
    | Gen of flag * gen
    | Typ of flag * t

type syn =
    | SynForAll of Name.t * flag * syn * syn
    | SynVar of Name.t
    | SynArrow of {domain : syn; codomain : syn}
    | SynWild
    | SynPrim of Prim.t

val of_syn : gen -> syn -> t

val to_doc : t -> PPrint.document

