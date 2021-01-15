type span = Util.span

type t =
    | ForAll of span * Name.t * IRUtil.flag * t * t
    | Var of span * Name.t
    | Arrow of {span : span; domain : t; codomain : t}
    | Wild of span
    | Prim of span * Prim.t

val to_doc : t -> PPrint.document

