type flag = Flex | Rigid

let flag_to_string = function
    | Flex -> ">="
    | Rigid -> "="

type gen = {mutable binder : gen option; typs : t CCVector.vector}

and t =
    | Arrow of {mutable binder : binder; domain : t; codomain : t}
    | Uv of {mutable binder : binder; mutable v : t option}
    | Prim of Prim.t

and binder =
    | Gen of flag * gen
    | Typ of flag * t

type syn =
    | SynForAll of Name.t * flag * syn * syn
    | SynVar of Name.t
    | SynArrow of {domain : syn; codomain : syn}
    | SynWild
    | SynPrim of Prim.t

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

    let rec of' env binder syn =
        let t = match syn with
            | SynForAll (param, flag, bound, body) ->
                let bound = of' env binder bound in
                let env = Env.add param bound env in
                let body = of' env binder body in
                if bound != body then bind bound (Typ (flag, body));
                body
            | SynVar name -> Env.get_exn name env (* FIXME: can raise *)
            | SynArrow {domain; codomain} ->
                let domain = of' env binder domain in
                let codomain = of' env binder codomain in
                let t = Arrow {binder; domain; codomain} in
                bind domain (Typ (Rigid, t));
                bind codomain (Typ (Flex, t));
                t
            | SynWild -> Uv {binder; v = None}
            | SynPrim p -> Prim p in
        t in

    let t = of' Env.empty (Gen (Flex, gen)) syn in
    CCVector.push gen.typs t;
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

    let tmp = {binder = None; typs = CCVector.create ()} in
    analyze (Gen (Flex, tmp)) t;
    (bindees, inlineabilities)

let to_doc t =
    let open PPrint in

    let (bindees, inlineabilities) = analyze t in

    let vne = Hashtbl.create 0 in
    let fresh_qname t =
        let doc = Name.to_doc (Name.fresh ()) in
        Hashtbl.add vne t doc;
        doc in

    let rec to_doc t =
        let bindees = Hashtbl.get bindees t |> Option.value ~default: [] in
        let quantification = List.fold_right (fun bindee acc ->
            if Hashtbl.find inlineabilities bindee
            then acc
            else begin
                let bdoc = to_doc bindee in
                let name = fresh_qname bindee in
                let q = string "forall" ^^ blank 1
                    ^^ infix 4 1 (string (flag_to_string
                            (bindee |> binder |> Option.get |> flag)))
                        name bdoc in
                Some (match acc with
                | Some acc -> (prefix 4 1 acc (dot ^^ blank 1 ^^ q))
                | None -> q)
            end
        ) bindees None in
        let body = match t with
            | Arrow {binder = _; domain; codomain} ->
                infix 4 1 (string "->") (child_to_doc domain) (child_to_doc codomain)
            | Uv {binder = _; v} -> (match v with
                | Some t -> to_doc t
                | None -> Hashtbl.get vne t |> Option.value ~default: (string "_"))
            | Prim p -> string "__" ^^ Prim.to_doc p in
        match quantification with
        | Some quantification -> prefix 4 1 quantification (dot ^^ blank 1 ^^ body)
        | None -> body

    and child_to_doc t =
        if Hashtbl.find inlineabilities t
        then to_doc t
        else Hashtbl.find vne t in

    to_doc t

