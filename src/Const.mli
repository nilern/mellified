type t = 
    | String of string
    | Int of int

val to_doc : t -> PPrint.document

