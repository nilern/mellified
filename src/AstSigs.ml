module Vec = CCImmutArray

type span = Util.span

module type EXPR = sig
    type stmt

    module Pat : sig
        type t = span * Name.t

        val to_doc : t -> PPrint.document
    end

    type t = private
        | Fn of span * Pat.t * t
        | App of span * t * t
        | Let of span * stmt Vec.t * t
        | Var of span * Name.t
        | Const of span * Const.t

    val fn : span -> Pat.t -> t -> t
    val app : span -> t -> t -> t
    val let' : span -> stmt Vec.t -> t -> t
    val var : span -> Name.t -> t
    val const : span -> Const.t -> t

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

