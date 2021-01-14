module Vec = CCImmutArray

open Ast
module C = Constraint

type binding =
    | Let of Type.gen * Type.t
    | Param of Type.t

module Env = struct
    include Name.HashMap

    type env = binding t
    type t = env
end

let constrain stmts =
    let constr = C.create () in

    let const_type : Const.t -> Prim.t = function
        | String _ -> String
        | Int _ -> Int in

    let rec typeof gen env : Expr.t -> Type.t = function
        | Let (_, stmts, body) ->
            let (gen, env) = Vec.fold constrain_stmt (gen, env) stmts in
            typeof gen env body

        | Var (_, name) -> (match Env.get_exn name env with (* FIXME: can raise *)
            | Let (vgen, vt) ->
                let t = Type.uv gen in
                CCVector.push gen.typs t;

                Constraint.instantiate constr vgen vt t;

                t

            | Param vt ->
                let t = Type.uv gen in
                CCVector.push gen.typs t;

                Constraint.unify constr vt t;

                t)

        | Fn (_, (_, name, (* FIXME: use ann: *) _), body) ->
            let domain = Type.uv gen in
            let codomain = Type.uv gen in
            let t = Type.arrow gen domain codomain in
            CCVector.push gen.typs t;

            let bgen = Type.level gen in
            let env = Env.add name (Param domain) env in
            let bt = typeof bgen env body in
            Constraint.instantiate constr bgen bt codomain;

            t

        | App (_, callee, arg) ->
            let domain = Type.uv gen in
            let codomain = Type.uv gen in
            let t = Type.arrow gen domain codomain in
            CCVector.push gen.typs codomain;

            let cgen = Type.level gen in
            let ct = typeof cgen env callee in
            Constraint.instantiate constr cgen ct t;

            let agen = Type.level gen in
            let at = typeof agen env arg in
            Constraint.instantiate constr agen at domain;

            codomain

        | Const (_, c) ->
            let t = Type.prim (const_type c) in
            CCVector.push gen.typs t;

            t

    and constrain_stmt (gen, env) : Stmt.t -> Type.gen * Env.t = function
        | Val (_, (_, name, (* FIXME: use ann: *) _), expr) ->
            let gen = Type.level gen in
            let t = typeof gen env expr in
            (gen, Env.add name (Let (gen, t)) env)

        | Do (_, expr) ->
            let gen = Type.level gen in
            ignore (typeof gen env expr);
            (gen, env) in

    let gen : Type.gen = {binder = None; typs = CCVector.create ()} in
    let _ = Vec.fold constrain_stmt (gen, Env.empty) stmts in
    constr

