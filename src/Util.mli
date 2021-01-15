type span = Lexing.position * Lexing.position

val start_pos : string -> Lexing.position

val prec_parens : bool -> PPrint.document -> PPrint.document

val pprint : PPrint.document -> unit

