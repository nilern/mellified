type t = String | Int

val of_string : string -> t option
val to_string : t -> string
val to_doc : t -> PPrint.document

val eq : t -> t -> bool

