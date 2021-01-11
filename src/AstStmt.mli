module S = AstSigs

module Make (Expr : S.EXPR) : S.STMT
    with type expr = Expr.t
    with type pat = Expr.Pat.t

