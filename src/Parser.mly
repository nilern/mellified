%token VAL "val" DO "do" EQ "=" EOF
%token <string> ID STRING
%token <int> INT

%start stmts
%type <Ast.Stmt.t CCImmutArray.t> stmts

%%

stmts: stmt* EOF { CCImmutArray.of_list $1 }

(* # Statements *)

stmt :
    | "val" pat "=" expr { Val ($loc, $2, $4) }
    | "do" expr { Do $2 }

pat : ID { ($loc, Name.of_string $1) }

(* # Expressions *)

expr :
    | const { Const ($loc, $1) }

const : INT { Int $1 }

