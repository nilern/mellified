module S = AstSigs
type span = Util.span

module Make (Stmt : S.STMT) = struct
    type stmt = Stmt.t

    module Pat = struct
        type t = span * Name.t

        let to_doc (_, name) = Name.to_doc name
    end

    type t =
        | Fn of span * Pat.t * t
        | App of span * t * t
        | Var of span * Name.t
        | Const of span * Const.t

    let span = function
        | Fn (span, _, _)
        | App (span, _, _)
        | Var (span, _)
        | Const (span, _) -> span

    let fn_prec = 0
    let app_prec = 1

    let prec_parens wrap doc = if wrap then PPrint.parens doc else doc

    let rec to_doc_prec prec =
        let open PPrint in
        function
        | Fn (_, param, body) ->
            prefix 4 1 (string "fun" ^^ blank 1 ^^ Pat.to_doc param ^^ blank 1 ^^ string "=>")
                (to_doc_prec fn_prec body)
            |> prec_parens (prec > fn_prec)
        | App (_, callee, arg) -> prefix 4 1 (to_doc_prec app_prec callee)
                (to_doc_prec (app_prec + 1) arg)
            |> prec_parens (prec > app_prec)
        | Var (_, name) -> Name.to_doc name
        | Const (_, c) -> Const.to_doc c

    let to_doc = to_doc_prec 0
end

