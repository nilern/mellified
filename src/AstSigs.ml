type span = Util.span

module type EXPR = sig
    type stmt

    type pat = span * Name.t

    type t =
        | Const of span * Const.t
end

module type STMT = sig
    type expr
    type pat

    type t =
        | Val of span * pat * expr
        | Do of expr
end

