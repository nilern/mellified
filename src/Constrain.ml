module Vec = CCImmutArray

module Expr = Ast.Expr
module Stmt = Ast.Stmt
let instantiate = Constraint.instantiate
let unify = Constraint.unify

type binding =
    | Let of Type.gen * Type.t
    | Param of Type.t

module Env = Name.HashMap

let constrain stmts =
    let constr = Constraint.create () in

    let const_type : Const.t -> Prim.t = function
        | String _ -> String
        | Int _ -> Int in

    let rec typeof gen env : Expr.t -> Type.t = function
        | Let (_, stmts, body) ->
                let (_, gen, env) = Vec.fold constrain_stmt ([], gen, env) stmts in
            typeof gen env body

        | Var (span, name) -> (match Env.get_exn name env with (* FIXME: can raise *)
            | Let (vgen, vt) ->
                let t = Type.uv gen in
                instantiate constr span vgen vt t;
                t

            | Param vt ->
                let t = Type.uv gen in
                unify constr span vt t;
                t)

        | Fn (_, (_, name, (* FIXME: use ann: *) _), body) ->
            let domain = Type.uv gen in
            let codomain = Type.uv gen in
            let t = Type.arrow gen domain codomain in

            let bgen = Type.level gen in
            let env = Env.add name (Param domain) env in
            let bt = typeof bgen env body in
            instantiate constr (Expr.span body) bgen bt codomain;

            t

        | App (_, callee, arg) ->
            let domain = Type.uv gen in
            let codomain = Type.uv gen in
            let t = Type.arrow gen domain codomain in

            let cgen = Type.level gen in
            let ct = typeof cgen env callee in
            instantiate constr (Expr.span callee) cgen ct t;

            let agen = Type.level gen in
            let at = typeof agen env arg in
            instantiate constr (Expr.span arg) agen at domain;

            codomain

        | Const (_, c) ->
            let t = Type.prim gen (const_type c) in
            t

    and constrain_stmt (ts, gen, env) = function
        | Val (_, (_, name, (* FIXME: use ann: *) _), expr) ->
            let gen = Type.level gen in
            let t = typeof gen env expr in
            ((Some name, t) :: ts, gen, Env.add name (Let (gen, t)) env)

        | Do (_, expr) ->
            let gen = Type.level gen in
            let t =  typeof gen env expr in
            ((None, t) :: ts, gen, env) in

    let gen : Type.gen = Top in
    let (nts, _, _) = Vec.fold constrain_stmt ([], gen, Env.empty) stmts in
    (constr, List.rev nts)

