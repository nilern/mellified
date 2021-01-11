module S = AstSigs

module Make (Stmt : S.STMT) : S.EXPR with type stmt = Stmt.t

