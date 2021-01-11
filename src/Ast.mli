module S = AstSigs

module rec Expr : (S.EXPR with type stmt = Stmt.t)
and Stmt : (S.STMT
    with type expr = Expr.t
    with type pat = Expr.pat)

