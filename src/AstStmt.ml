module S = AstSigs
type span = Util.span

module Make (Expr : S.EXPR) = struct
    type expr = Expr.t
    type pat = Expr.pat

    type t =
        | Val of span * pat * expr
        | Do of expr
end

