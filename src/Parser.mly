%{
open Ast
%}

%token FUN "fun" DARROW "=>" LET "let" IN "in" END "end" LPAREN "(" RPAREN ")"
    VAL "val" DO "do" EQ "="
    COLON ":" FORALL "forall" DOT "." GT ">=" ARROW "->"
    WILD "_"
    EOF
%token <string> ID PRIM STRING
%token <int> INT

%start stmts
%type <Ast.Stmt.t CCImmutArray.t> stmts

%%

stmts: stmt* EOF { CCImmutArray.of_list $1 }

(* # Statements *)

stmt :
    | "val" pat "=" expr { Val ($loc, $2, $4) }
    | "do" expr { Do ($loc, $2) }

pat : ID ann? { ($loc, Name.of_string $1, $2) }

ann : ":" typ { $2 }

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

(* # Types *)

typ : 
    | "forall" ID bound "." typ { Type.ForAll ($loc, Name.of_string $2, fst $3, snd $3, $5) }
    | tbody { $1 }

tbody :
    | tnestable "->" tbody { Arrow {span = $loc; domain = $1; codomain = $3} }
    | tnestable { $1 }

tnestable :
    | "(" typ ")" { $2 }
    | tatom { $1 }

tatom :
    | ID { Var ($loc, Name.of_string $1) }
    | "_" { Wild $loc }
    | PRIM { Prim ($loc, Prim.of_string $1 |> Option.get) } (* FIXME *)

bound :
    | flag typ { ($1, $2) }
    | { (Flex, Wild $loc) }

flag :
    | EQ { Rigid }
    | GT { IRUtil.Flex }

