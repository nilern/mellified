type span = Lexing.position * Lexing.position

let pprint doc =
    PPrint.ToChannel.pretty 1.0 80 stdout doc;
    print_newline ()

