type t = String | Int

let of_string = function
    | "string" -> Some String
    | "int" -> Some Int
    | _ -> None

let to_string = function
    | String -> "string"
    | Int -> "int"

let to_doc p = PPrint.string (to_string p)

let eq = (=)

