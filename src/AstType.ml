type span = Util.span
let prec_parens = Util.prec_parens

type t =
    | ForAll of span * Name.t * IRUtil.flag * t * t
    | Var of span * Name.t
    | Arrow of {span : span; domain : t; codomain : t}
    | Wild of span
    | Prim of span * Prim.t

let universal_prec = 0
let arrow_prec = 1

let rec to_doc_prec prec t =
    let open PPrint in
    match t with
    | ForAll (_, param, flag, bound, body) ->
        prefix 4 1
            (string "forall" ^^ blank 1
                ^^ infix 4 1 (string (IRUtil.flag_to_string flag))
                    (Name.to_doc param) (to_doc_prec 0 bound))
            (dot ^^ blank 1 ^^ to_doc_prec universal_prec body)
        |> prec_parens (prec > universal_prec)
    | Var (_, name) -> Name.to_doc name
    | Arrow {span = _; domain; codomain} ->
        infix 4 1 (string "->")
            (to_doc_prec (arrow_prec + 1) domain) (to_doc_prec arrow_prec codomain)
        |> prec_parens (prec > arrow_prec)
    | Wild _ -> string "_"
    | Prim (_, p) -> string "__" ^^ Prim.to_doc p

let to_doc = to_doc_prec 0

