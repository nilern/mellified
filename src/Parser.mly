%{
open Ast
%}

%token FUN "fun" DARROW "=>" LPAREN "(" RPAREN ")"
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
    | "fun" pat "=>" expr { Fn ($loc, $2, $4) }
    | app { $1 }

app :
    | app nestable { App ($loc, $1, $2) }
    | nestable { $1 }

nestable :
    | "(" expr ")" { $2 }
    | atom { $1 }

atom :
    | ID { Var ($loc, Name.of_string $1) }
    | const { Expr.Const ($loc, $1) }

const :
    | STRING { String $1 }
    | INT { Const.Int $1 }

