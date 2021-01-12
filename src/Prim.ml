type t = Int

let of_string = function
    | "int" -> Some Int
    | _ -> None

let to_string = function
    | Int -> "int"

let to_doc p = PPrint.string (to_string p)

