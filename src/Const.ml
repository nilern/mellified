type t =
    | String of string
    | Int of int

let to_doc =
    let open PPrint in
    function
    | String s -> dquotes (string s)
    | Int n -> string (Int.to_string n)

