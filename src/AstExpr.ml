module S = AstSigs
type span = Util.span

module Make (Stmt : S.STMT) = struct
    type stmt = Stmt.t

    type pat = span * Name.t

    type t =
        | Const of span * Const.t
end

