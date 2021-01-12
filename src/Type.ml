type flag = Flex | Rigid

let flag_to_string = function
    | Flex -> ">="
    | Rigid -> "="

type t' =
    | Arrow of {domain : t; codomain : t}
    | Uv of t option ref
    | Prim of Prim.t

and t = {term : t'; mutable binder : t option; mutable flag : flag option}

type syn =
    | SynForAll of Name.t * flag * syn * syn
    | SynVar of Name.t
    | SynArrow of {domain : syn; codomain : syn}
    | SynWild
    | SynPrim of Prim.t

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

let of_syn =
    let module Env = Name.HashMap in

    let rec of_syn env = function
        | SynForAll (param, flag, bound, body) ->
            let bound = of_syn env bound in
            let env = Env.add param bound env in
            let body = of_syn env body in
            bound.binder <- Some body;
            bound.flag <- Some flag;
            body
        | SynVar name -> Env.get_exn name env (* FIXME: can raise *)
        | SynArrow {domain; codomain} ->
            let domain = of_syn env domain in
            let codomain = of_syn env codomain in
            let t = {term = Arrow {domain; codomain}; binder = None; flag = None} in
            domain.binder <- Some t;
            domain.flag <- Some Rigid;
            codomain.binder <- Some t;
            codomain.flag <- Some Flex;
            t
        | SynWild -> {term = Uv (ref None); binder = None; flag = None}
        | SynPrim p -> {term = Prim p; binder = None; flag = None} in
    of_syn Env.empty

let analyze t =
    let bindees = Hashtbl.create 0 in
    let add_bindee t = match t.binder with
        | Some binder -> 
            Hashtbl.update bindees ~k: binder ~f: (fun _ -> function
                | Some ts -> Some (t :: ts)
                | None -> Some [t])
        | None -> () in

    let inlineabilities = Hashtbl.create 0 in
    let add_inlineability t b' =
        Hashtbl.update inlineabilities ~k: t ~f: (fun _ -> function
            | Some b -> Some (b && b')
            | None -> Some b') in

    let visited = HashSet.create 0 in

    let rec analyze opt_parent expected_flag t =
        if HashSet.mem visited t
        then add_inlineability t false
        else begin
            HashSet.insert visited t;
            (match t.term with
            | Arrow {domain; codomain} ->
                analyze (Some t) (Some Rigid) domain;
                analyze (Some t) (Some Flex) codomain;
                add_bindee t;
            | Uv uv -> (match !uv with
                | Some t' -> analyze opt_parent expected_flag t'
                | None -> add_bindee t);
            | Prim _ -> ());
            (match opt_parent with
            | Some parent ->
                (match t.term with
                | Arrow _ | Uv _ ->
                    (match t.binder with
                    | Some binder -> binder == parent && t.flag = expected_flag
                    | None -> false)
                | Prim _ -> true)
            | None -> false)
            |> add_inlineability t
        end in

    analyze None None t;
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
                    ^^ infix 4 1 (string (flag_to_string (Option.get bindee.flag)))
                        name bdoc in
                Some (match acc with
                | Some acc -> (prefix 4 1 acc (dot ^^ blank 1 ^^ q))
                | None -> q)
            end
        ) bindees None in
        let body = match t.term with
            | Arrow {domain; codomain} ->
                infix 4 1 (string "->") (child_to_doc domain) (child_to_doc codomain)
            | Uv uv -> (match !uv with
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

