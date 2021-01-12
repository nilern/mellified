type flag = Flex | Rigid

type t' =
    | Arrow of {domain : t; codomain : t}
    | Uv of t option ref
    | Prim of Prim.t

and t = {term : t'; mutable binder : t option; mutable flag : flag option}

type syn =
    | SynForAll of Name.t * flag * syn * syn
    | SynVar of Name.t
    | SynArrow of {domain : syn; codomain : syn}
    | SynWild
    | SynPrim of Prim.t

val of_syn : syn -> t

val to_doc : t -> PPrint.document

