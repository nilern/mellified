open Streaming
let (%>) = CCFun.(%>)

type flag = IRUtil.flag

type gen =
    | Local of gen
    | Top

and t =
    | Arrow of {mutable binder : binder; domain : t; codomain : t}
    | Uv of {mutable binder : binder; mutable v : t option}
    | Prim of {mutable binder : binder; p : Prim.t}

and binder =
    | Gen of flag * gen
    | Typ of flag * t

let level parent = Local parent

let uv gen = Uv {binder = Gen (Flex, gen); v = None}
let arrow gen domain codomain = Arrow {binder = Gen (Flex, gen); domain; codomain}
let prim gen p = Prim {binder = Gen (Flex, gen); p}

let binder = function
    | Arrow {binder; _} | Uv {binder; _} | Prim {binder; _} -> binder

let bind (t : t) binder = match t with
    | Arrow arrow -> arrow.binder <- binder
    | Uv uv -> uv.binder <- binder
    | Prim prim -> prim.binder <- binder

let flag = function
    | Gen (flag, _) | Typ (flag, _) -> flag

let binder_eq b b' = match (b, b') with
    | (Gen (flag, gen), Gen (flag', gen')) -> flag = flag' && gen == gen'
    | (Typ (flag, t), Typ (flag', t')) -> flag = flag' && t == t'
    | (Gen _, Typ _) | (Typ _, Gen _) -> false

type typ = t

module Hashtbl = CCHashtbl.Make (struct
    type t = typ
    let hash = Hashtbl.hash
    let equal = (==)
end)

module HashSet = CCHashSet.Make (struct
    type t = typ
    let hash = Stdlib.Hashtbl.hash
    let equal = (==)
end)

let of_syn gen syn =
    let module Env = Name.HashMap in

    let rec of' env binder (syn : Ast.Type.t) =
        let t = match syn with
            | ForAll (_, param, flag, bound, body) ->
                let bound = of' env binder bound in
                let env = Env.add param bound env in
                let body = of' env binder body in
                if bound != body then bind bound (Typ (flag, body));
                body
            | Var (_, name) -> Env.get_exn name env (* FIXME: can raise *)
            | Arrow {span = _; domain; codomain} ->
                let domain = of' env binder domain in
                let codomain = of' env binder codomain in
                let t = Arrow {binder; domain; codomain} in
                bind domain (Typ (Rigid, t));
                bind codomain (Typ (Flex, t));
                t
            | Wild _ -> Uv {binder; v = None}
            | Prim (_, p) -> Prim {binder; p} in
        t in

    let t = of' Env.empty (Gen (Flex, gen)) syn in
    t

let analyze t =
    let bindees = Hashtbl.create 0 in
    let add_bindee (t : t) = match binder t with
        | Typ (_, binder) ->
            Hashtbl.update bindees ~k: binder ~f: (fun _ -> function
                | Some ts -> Some (t :: ts)
                | None -> Some [t])
        | Gen _ -> () in

    let inlineabilities = Hashtbl.create 0 in
    let add_inlineability t b' =
        Hashtbl.update inlineabilities ~k: t ~f: (fun _ -> function
            | Some b -> Some (b && b')
            | None -> Some b') in

    let visited = HashSet.create 0 in

    let rec analyze parent t =
        if HashSet.mem visited t
        then add_inlineability t false
        else begin
            HashSet.insert visited t;
            (match t with
            | Arrow {binder = _; domain; codomain} ->
                analyze (Typ (Rigid, t)) domain;
                analyze (Typ (Flex, t)) codomain;
                add_bindee t;
            | Uv {binder = _; v} -> (match v with
                | Some term -> analyze parent term
                | None -> add_bindee t);
            | Prim _ -> add_bindee t);
            binder_eq (binder t) parent
            |> add_inlineability t
        end in

    let tmp = Top in
    analyze (Gen (Flex, tmp)) t;
    (bindees, inlineabilities)

let to_syn span t = 
    let (bindees, inlineabilities) = analyze t in

    let vne = Hashtbl.create 0 in
    let fresh_qname t =
        let name = Name.fresh () in
        Hashtbl.add vne t (Ast.Type.Var (span, name));
        name in

    let rec to_syn t =
        let bindees = Hashtbl.get bindees t |> Option.value ~default: [] in
        let bindees = List.fold_right (fun bindee acc ->
            if Hashtbl.find inlineabilities bindee
            then acc
            else begin
                let bound = to_syn bindee in
                let flag = bindee |> binder |> flag in
                let name = fresh_qname bindee in
                (name, flag, bound) :: acc
            end
        ) bindees [] in
        let body : Ast.Type.t = match t with
            | Arrow {binder = _; domain; codomain} ->
                Arrow {span; domain = child_to_syn domain; codomain = child_to_syn codomain}
            | Uv {binder = _; v} -> (match v with
                | Some t -> to_syn t
                | None -> Hashtbl.get vne t |> Option.value ~default: (Ast.Type.Wild span))
            | Prim {binder = _; p} -> Prim (span, p) in
        List.fold_left (fun body (name, flag, bound) -> (* fold_left reverses, intentionally *)
            Ast.Type.ForAll (span, name, flag, bound, body)
        ) body bindees

    and child_to_syn t =
        if Hashtbl.find inlineabilities t
        then to_syn t
        else Hashtbl.find vne t in

    to_syn t

let to_doc =
    let shim_pos = Util.start_pos "" in
    let shim_span = (shim_pos, shim_pos) in
    fun t -> Ast.Type.to_doc (to_syn shim_span t)

let expand unify gen t dest =
    let dest =
        let rec loop t = match binder t with
            | Gen (_, dest) -> dest
            | Typ (_, t) -> loop t in
        loop dest in

    let copies = Hashtbl.create 0 in

    let rec locally_bound t = match binder t with
        | Gen (_, gen') -> gen == gen'
        | Typ (_, t') -> locally_bound t' in

    let rec expand_term t = match t with

        | Uv {binder; v} -> (match v with
            | Some t -> expand_term t
            | None when locally_bound t ->
                let t' = Uv {binder; v = None} in
                Hashtbl.add copies t t';
                t'
            | None ->
                let t' = Uv {binder = Gen (Flex, dest); v = None} in
                unify t t';
                t')

        | Arrow {binder; domain; codomain} when locally_bound t ->
            let t' = Arrow {binder
                ; domain = expand_term domain; codomain = expand_term codomain} in
            Hashtbl.add copies t t';
            t'

        | Prim {binder; p} when locally_bound t ->
            let t' = Prim {binder; p} in
            Hashtbl.add copies t t';
            t'

        | Arrow _ | Prim _ ->
            let t' = Uv {binder = Gen (Flex, dest); v = None} in
            unify t t';
            t' in

    let root = expand_term t in

    let rec rebind_expansion t =
        if t == root
        then bind t (Gen (Flex, dest))
        else (match binder t with
            | Typ (flag, t) -> bind t (Typ (flag, Hashtbl.find copies t))
            | Gen (flag, gen') when gen' == gen -> bind t (Typ (flag, root))
            | Gen _ -> ());

        match t with
        | Arrow {binder = _; domain; codomain} ->
            rebind_expansion domain;
            rebind_expansion codomain
        | Uv {binder = _; v = None} | Prim _ -> ()
        | Uv {binder = _; v = Some _} -> failwith "unreachable" in

    rebind_expansion root;
    root

let unify_terms span t t' =
    let mergeds = Hashtbl.create 0 in
    let pg_parents = Hashtbl.create 0 in

    let occurs uvt = (* NOTE: also adds to `pgrafteds` if relevant *)
        let rec occ parent t =
            (match parent with
            | Some parent -> Hashtbl.add pg_parents t parent
            | None -> ());

            match t with
            | Uv {binder = _; v} as t -> (match v with
                | Some t' -> occ (Some t) t'
                | None -> t == uvt)
            | Arrow {binder = _; domain; codomain} ->
                let parent = Some t in
                occ parent domain || occ parent codomain
            | Prim _ -> false in
        occ None in

    let rec uni t t' =
        Hashtbl.update mergeds ~k: t ~f: (fun _ -> function
            | Some ts -> Some (t' :: ts)
            | None -> Some [t']);

        match (t, t') with
        | (Uv ({binder = _; v = None} as uv) as uvt, t) | (t, (Uv ({binder = _; v = None} as uv) as uvt)) ->
            if occurs uvt t
            then failwith ("occurs check at " ^ Util.span_to_string span)
            else uv.v <- Some t

        | (Arrow {binder = _; domain; codomain}, _) -> (match t' with
            | Arrow {binder = _; domain = domain'; codomain = codomain'} ->
                uni domain domain';
                uni codomain codomain'

            | _ -> failwith ("incompatible type shapes at " ^ Util.span_to_string span))

        | (Prim {binder = _; p}, _) -> (match t' with
            | Prim {binder = _; p = p'} ->
                if Prim.eq p p'
                then ()
                else failwith ("incompatible type shapes at " ^ Util.span_to_string span)

            | _ -> failwith ("incompatible type shapes at " ^ Util.span_to_string span))

        | _ -> failwith ("incompatible type shapes at " ^ Util.span_to_string span) in
    uni t t';

    (pg_parents, mergeds)

(* Yakobowski thesis fig. 7.3.2:
 *
 * A node is *partially grafted* when some ancestor of it was assigned to a uv
 * by unify_terms.
 *
 * 1. Building the binding tree:
 *     Visit nodes in pre-order (w.r.t. children of course).
 *     For each node:
 *
 *         a. let mergeds = the set of nodes merged into `node` by unify_terms.
 *         b. let flag = Rigid if it exists in `mergeds`, else Flex
 *         c. let binders = the `binder`s of `mergeds`
 *            let pg_parents =
 *                 {the structural parents of `node`} if `node` is partially grafted
 *                 {} otherwise
 *         d. node.binder <- Gen/Typ (flag, LCA (binders U pg_parents))
 * 2. Checking permissions:
 *     Fail if:
 *     
 *         a. A non-green uv was grafted to
 *         b. A red node was raised
 *         c. A red node was weakened
 *         d. A red node was merged with another node with the same binder
 *)
(* FIXME: Check permissions: *)
let rebind _ t (pg_parents, mergeds) =
    let lca = (* OPTIMIZE *)
        let ancestors t =
            let rec anc bs b =
                let bs = b :: bs in
                match b with
                | Typ (_, t) -> anc bs (binder t)
                | Gen (_, Local parent) -> anc bs (Gen (Flex, parent))
                | Gen (_, Top) -> bs in
            anc [] t in
        fun b b' -> Stream.from
                (Source.zip (Source.list (ancestors b)) (Source.list (ancestors b')))
            |> Stream.filter (fun (b, b') -> binder_eq b b')
            |> Stream.into Sink.last
            |> Option.get |> fst in

    let rec rebind t =
        let mergeds = Hashtbl.get mergeds t |> Option.value ~default: [] in
        let ts = t :: mergeds in
        let binders = Stream.from (Source.list ts)
            |> Stream.flat_map (binder %> Stream.single)
            |> Stream.into Sink.list in

        let flag = Stream.from (Source.list binders)
            |> Stream.map flag
            |> Stream.into (Sink.fold IRUtil.flag_max Flex) in

        let pg_parents = Stream.from (Source.list ts)
            |> Stream.flat_map (fun t -> match Hashtbl.get pg_parents t with
                | Some parent -> Stream.single (Typ (Flex, parent))
                | None -> Stream.empty)
            |> Stream.into Sink.list in
        (match List.append binders pg_parents with
        | b :: bs ->
            let binder = match List.fold_left lca b bs with
                | Gen (_, gen) -> Gen (flag, gen)
                | Typ (_, t) -> Typ (flag, t) in
            ts |> List.iter (fun t -> bind t binder)
        | [] -> ());

        match t with
        | Arrow {binder = _; domain; codomain} -> rebind domain; rebind codomain
        | Uv {binder = _; v} -> Option.iter rebind v
        | Prim _ -> () in
    rebind t

let unify span t t' = unify_terms span t t' |> rebind span t

