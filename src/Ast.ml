module S = AstSigs

module rec Expr : (S.EXPR with type stmt = Stmt.t)
= AstExpr.Make (Stmt)

and Stmt : (S.STMT
    with type expr = Expr.t
    with type pat = Expr.pat)
= AstStmt.Make (Expr)

