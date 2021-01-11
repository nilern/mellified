module S = AstSigs
type span = Util.span

module Make (Expr : S.EXPR) = struct
    type expr = Expr.t
    type pat = Expr.Pat.t

    type t =
        | Val of span * pat * expr
        | Do of span * expr

    let span = function
        | Val (span, _, _)
        | Do (span, _) -> span

    let to_doc =
        let open PPrint in
        function
        | Val (_, pat, expr) -> infix 4 1 equals
            (string "val" ^^ blank 1 ^^ Expr.Pat.to_doc pat)
            (Expr.to_doc expr)
        | Do (_, expr) -> string "do" ^^ blank 1 ^^ Expr.to_doc expr
end

