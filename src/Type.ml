type flag = IRUtil.flag

type gen =
    | Local of gen
    | Top

and t =
    | Arrow of {mutable binder : binder; domain : t; codomain : t}
    | Uv of {mutable binder : binder; mutable v : t option}
    | Prim of Prim.t

and binder =
    | Gen of flag * gen
    | Typ of flag * t

let level parent = Local parent

let uv gen = Uv {binder = Gen (Flex, gen); v = None}
let arrow gen domain codomain = Arrow {binder = Gen (Flex, gen); domain; codomain}
let prim p = Prim p

let binder = function
    | Arrow {binder; _} | Uv {binder; _} -> Some binder
    | Prim _ -> None

let bind (t : t) binder = match t with
    | Arrow arrow -> arrow.binder <- binder
    | Uv uv -> uv.binder <- binder
    | Prim _ -> ()

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
            | Prim (_, p) -> Prim p in
        t in

    let t = of' Env.empty (Gen (Flex, gen)) syn in
    t

let analyze t =
    let bindees = Hashtbl.create 0 in
    let add_bindee (t : t) = match binder t with
        | Some (Typ (_, binder)) ->
            Hashtbl.update bindees ~k: binder ~f: (fun _ -> function
                | Some ts -> Some (t :: ts)
                | None -> Some [t])
        | Some (Gen _) | None -> () in

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
            | Prim _ -> ());
            (match binder t with
            | Some binder -> binder_eq binder parent
            | None -> true)
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
                let flag = bindee |> binder |> Option.get |> flag in
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
            | Prim p -> Prim (span, p) in
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

