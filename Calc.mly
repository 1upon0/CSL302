%{

open Printf
open Lexing

type ast = Empty| F of float | Node of string*ast*ast
let var_table = Hashtbl.create 16
let bvar_table = Hashtbl.create 16
type binop =
| Add
| Sub
| Mul
| Div
type expr =
| Var of string
| Binop of exp * binop * exp

(* print_op : Ast.binop - > unit *)
let print_op op = match op with
	| Add -> print_string "+"
	| Sub -> print_string "-"
	| Mul -> print_string "*"
	| Div -> print_string "/"
(* print_exp : Ast.exp -> unit *)
let rec print_exp expr = match expr with
	| Var v -> print_string v
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
%token LPAREN RPAREN EQ  LESSEQ GREATEREQ LESS GREATER 
%token <bool> TRUE FALSE
%token <float> FNUM
%token <int> INUM
%token IPLUS IMINUS IMULTIPLY IDIVIDE CARET FPLUS FMINUS FMULTIPLY FDIVIDE IMOD FMOD IPOWER FPOWER	
%token <string> VAR
%token <float->float> FNCT


%left AND OR EQ LESSEQ GREATEREQ LESS GREATER
%left IPLUS IMINUS FMINUS FPLUS    
%left IMULTIPLY IDIVIDE FMULTIPLY FDIVIDE
%left NEG	/* negation -- unary minus */
%right IPOWER FPOWER IMOD FMOD	/* exponentiation */
%left NOT

%start input

 %type <expr	> input 

/* Grammar follows */
%%
input:	/* empty */	{ }
	| input line	{ }
;
line:	NEWLINE		{ }
	| exp NEWLINE	{ printf "\t % f	 \n" $1;print_exp $1 ;flush stdout }
	| bexp NEWLINE { printf "\t% b \n" $1; flush stdout }
	| error NEWLINE	{printf "\t error \n" ; flush stdout }
;
exp:	FNUM { $1 }
	| INUM			{	float $1 }
	| VAR			{ try Hashtbl.find var_table $1
				  with Not_found ->
				    printf "no such variable '%s'\n" $1;
				    0.0
				}
	| VAR EQ exp		{ Hashtbl.replace var_table $1 $3;
				  printf "%s -> %f " $1 $3;
					$3
				}
	| FNCT LPAREN exp RPAREN	{ $1 $3 }
	| exp IPLUS exp		{Binop ($1,Add,$3);
											$1 +. $3  }
	| exp IMINUS exp		{ Binop ($1,Sub,$3);
												$1 -. $3}
	| exp IMULTIPLY exp	{ Binop ($1,Mul,$3);
												$1 *. $3 }
	| exp IDIVIDE exp	{ Binop ($1,Div,$3);
											$1 /. $3}
	| exp FPLUS exp		{ $1 +. $3 }
	| exp FMINUS exp		{ $1 -. $3 }
	| exp FMULTIPLY exp	{ $1 *. $3 }
	| exp FDIVIDE exp	{ $1 /. $3 }
	| IMINUS exp %prec NEG	{ -. $2 }
	| FMINUS exp %prec NEG	{ -. $2 }
	| exp IPOWER exp		{ $1 ** $3 }
	| exp FPOWER exp		{ $1 ** $3 }
	| exp IMOD exp  		{float (int_of_float $1 mod int_of_float $3) }
	| LPAREN exp RPAREN	{ $2 }
;
bexp: TRUE { $1 }
	|	FALSE { $1 }
	| bexp OR bexp { $1 || $3 }
	| VAR			{ try Hashtbl.find bvar_table $1
				  with Not_found ->
				    printf "no such variable '%s'\n" $1;
				    false
				}	
	| VAR EQ bexp		{ Hashtbl.replace bvar_table $1 $3;
				  $3
				}
	| bexp AND bexp { $1 & $3 }
	| NOT bexp { not $2 } 
	| exp LESS exp { $1 < $3 }
	| exp GREATER exp { $1 > $3 }
	| exp EQ exp { $1 = $3 }
	| exp LESSEQ exp { $1 <= $3 }
	| exp GREATEREQ exp { $1 >= $3 }
	

;
%%