type span = Util.span

module type EXPR = sig
    type stmt

    module Pat : sig
        type t = span * Name.t

        val to_doc : t -> PPrint.document
    end

    type t =
        | Fn of span * Pat.t * t
        | App of span * t * t
        | Var of span * Name.t
        | Const of span * Const.t

    val span : t -> span

    val to_doc : t -> PPrint.document
end

module type STMT = sig
    type expr
    type pat

    type t =
        | Val of span * pat * expr
        | Do of span * expr

    val span : t -> span

    val to_doc : t -> PPrint.document
end

