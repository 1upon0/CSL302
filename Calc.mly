%{

open Printf
open Lexing

type ast = Empty| F of float | Node of string*ast*ast
let var_table = Hashtbl.create 16
let bvar_table = Hashtbl.create 16
type binop =
| IAdd
| ISub
| IMul
| IDiv
| FAdd
| FSub
| FMul
| FDiv
| Or
| And
| Greater
| Less
| Eq
| GreaterE
| LessE
| Mod
| Comma
| App
type unop = Neg
type expr =
| Var of string
| Binop of expr * binop * expr
| Unop of unop*expr

(* print_op : Ast.binop - > unit *)
let print_op op = match op with
	| IAdd -> print_string "+"
	| ISub -> print_string "-"
	| IMul -> print_string "*"
	| IDiv -> print_string "/"
	| FAdd -> print_string ".+"
	| FSub -> print_string ".-"
	| FMul -> print_string ".*"
	| FDiv -> print_string "./" 
	| Or -> print_string "||"
	| And -> print_string "&&"
	| Greater -> print_string ">"
	| Less -> print_string "<"
	| Eq -> print_string "="
	| GreaterE -> print_string ">="
	| LessE -> print_string "<="
	| Mod -> print_string "mod"
	| Comma ->print_string ","
	| App -> print_string " applied to -> "
let print_op1 op = match op with	
	| Neg -> print_string "~"
(* print_exp : Ast.exp -> unit *)
let rec print_exp expr = match expr with
	| Var v -> print_string v
	| Unop (op ,exp0) -> begin
		print_string "(";
		print_op1 op;
		print_exp exp0;
		print_string ")";
		end
	| Binop (exp0, op, exp1) -> begin
		print_string "(";
		print_exp exp0;
		print_op op;
		print_exp exp1;
		print_string ")";
		end
		
%}


/* Ocamlyacc Declarations */
%token NEWLINE  OR AND NOT 
%token LPAREN RPAREN EQ  LESSEQ GREATEREQ LESS GREATER COMMA
%token <bool> TRUE FALSE
%token <string> FNUM
%token <string> INUM
%token IPLUS IMINUS IMULTIPLY IDIVIDE CARET FPLUS FMINUS FMULTIPLY FDIVIDE IMOD FMOD IPOWER FPOWER	
%token <string> VAR


%left AND OR EQ LESSEQ GREATEREQ LESS GREATER
%left IPLUS IMINUS FMINUS FPLUS    
%left IMULTIPLY IDIVIDE FMULTIPLY FDIVIDE
%left NEG	/* negation -- unary minus */
%right IPOWER FPOWER IMOD FMOD	/* exponentiation */
%left NOT

%start input

 %type <unit> input 

/* Grammar follows */
%%
input:	/* empty */	{ }
	| input line	{ }
;
line:	NEWLINE		{ }
	| exp NEWLINE	{ print_exp $1; printf "\t\n";flush stdout }
	| bexp NEWLINE { print_exp $1;printf "\t  \n" ; flush stdout }
	| fexp NEWLINE { print_exp $1;printf "\t  \n" ; flush stdout }
	| error NEWLINE	{printf "\t error \n" ; flush stdout }
;
exp: INUM { Var  $1 }	
	| VAR			{ Var $1 }
	| VAR EQ exp		{  Binop(Var $1 ,Eq , $3 )
				}

	| exp IPLUS exp		{  Binop( $1 ,IAdd , $3 )  }
	| exp IMINUS exp		{ Binop( $1 ,ISub , $3 )  }
	| exp IMULTIPLY exp	{ Binop( $1 ,IMul , $3 )  }
	| exp IDIVIDE exp	{ Binop( $1 ,IDiv , $3 )  }

	| IMINUS exp %prec NEG	{ Unop(Neg,$2) }

	| exp IMOD exp  		{Binop($1,Mod,$3) }
	| LPAREN exp RPAREN	{ $2 }
	| exp COMMA exp {Binop($1,Comma,$3) }
	| VAR LPAREN exp RPAREN { Binop( Var $1, App ,$3) }
;
fexp: 
	| VAR			{ Var $1 }
	| VAR EQ fexp		{  Binop(Var $1 ,Eq , $3 )
				}
	|FNUM { Var $1 }
	
	| fexp FPLUS fexp		{ Binop( $1 ,FAdd , $3 )  }
	| fexp FMINUS fexp		{ Binop( $1 ,FSub , $3 )  }
	| fexp FMULTIPLY fexp	{ Binop( $1 ,FMul , $3 )  }
	| fexp FDIVIDE fexp	{ Binop( $1 , FDiv, $3 )  }

	| FMINUS fexp %prec NEG	{ Unop(Neg,$2) }

	| LPAREN fexp RPAREN	{ $2 }
	| fexp COMMA fexp {Binop($1,Comma,$3) }
	| VAR LPAREN fexp RPAREN { Binop( Var $1, App ,$3) }
;
bexp: TRUE { Var  "True" }
	|	FALSE { Var "False" }
	| bexp OR bexp { Binop ($1,Or,$3)}
	
	| bexp AND bexp { Binop ($1,And,$3) }
	| NOT bexp { Unop (Neg,$2) } 
	| exp LESS exp { Binop ($1,Less,$3)}
	| VAR			{Var $1
				}	
	| VAR EQ bexp		{ Binop(Var $1 ,Eq , $3 )
				}	
	| exp GREATER exp { Binop ($1,Greater,$3)}
	| exp EQ exp { Binop ($1,Eq,$3) }
	| exp LESSEQ exp { Binop ($1,LessE,$3) }
	| exp GREATEREQ exp { Binop ($1,GreaterE,$3) }
	| bexp COMMA bexp {Binop($1,Comma,$3) }
	| VAR LPAREN bexp RPAREN { Binop( Var $1, App ,$3) }

;
%%