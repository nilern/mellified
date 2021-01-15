type span = Lexing.position * Lexing.position

let start_pos pos_fname = {Lexing.
    pos_fname;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0
  }

let prec_parens wrap doc = if wrap then PPrint.parens doc else doc

let pprint doc =
    PPrint.ToChannel.pretty 1.0 80 stdout doc;
    print_newline ()

