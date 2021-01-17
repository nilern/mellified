type span = Lexing.position * Lexing.position

let start_pos pos_fname = {Lexing.
    pos_fname;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0
  }

let pos_to_string {Lexing.pos_lnum = line; pos_bol = bol; pos_cnum = offset; pos_fname = _} =
    Int.to_string line ^ ":" ^ Int.to_string (offset - bol)
let span_to_string (pos, pos') = pos_to_string pos ^ "-" ^ pos_to_string pos'

let prec_parens wrap doc = if wrap then PPrint.parens doc else doc

let pprint doc =
    PPrint.ToChannel.pretty 1.0 80 stdout doc;
    print_newline ()

