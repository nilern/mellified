%{
open Ast
%}

%token FUN "fun" DARROW "=>" LET "let" IN "in" END "end" LPAREN "(" RPAREN ")"
    VAL "val" DO "do" EQ "="
    EOF
%token <string> ID STRING
%token <int> INT

%start stmts
%type <Ast.Stmt.t CCImmutArray.t> stmts

%%

stmts: stmt* EOF { CCImmutArray.of_list $1 }

(* # Statements *)

stmt :
    | "val" pat "=" expr { Val ($loc, $2, $4) }
    | "do" expr { Do ($loc, $2) }

pat : ID { ($loc, Name.of_string $1) }

(* # Expressions *)

expr :
    | "fun" pat "=>" expr { Expr.fn $loc $2 $4 }
    | app { $1 }

app :
    | app nestable { Expr.app $loc $1 $2 }
    | nestable { $1 }

nestable :
    | "let" stmt+ "in" expr "end" { Expr.let' $loc (CCImmutArray.of_list $2) $4 }
    | "(" expr ")" { $2 }
    | atom { $1 }

atom :
    | ID { Expr.var $loc (Name.of_string $1) }
    | const { Expr.const $loc $1 }

const :
    | STRING { String $1 }
    | INT { Const.Int $1 }

