%{

open Printf
open Lexing

let var_table = Hashtbl.create 16
 
%}


/* Ocamlyacc Declarations */
%token NEWLINE  OR AND NOT 
%token <bool> TRUE FALSE
%token LPAREN RPAREN EQ  LESSEQ GREATEREQ LESS GREATER 
%token <float> FNUM
%token <int> INUM
%token IPLUS IMINUS IMULTIPLY IDIVIDE CARET FPLUS FMINUS FMULTIPLY FDIVIDE IMOD FMOD 	
%token <string> VAR
%token <float->float> FNCT


%left AND OR
%left IPLUS IMINUS FMINUS FPLUS EQ  LESSEQ GREATEREQ LESS GREATER 
%left IMULTIPLY IDIVIDE FMULTIPLY FDIVIDE
%left NEG	/* negation -- unary minus */
%right CARET IMOD FMOD	/* exponentiation */
%left NOT


%start input
%type <unit> input

/* Grammar follows */
%%
input:	/* empty */	{ }
	| input line	{ }
;
line:	NEWLINE		{ }
	| exp NEWLINE	{ printf "\t%.10g\n" $1; flush stdout }
	| error NEWLINE	{ }
;
exp:	FNUM			{ $1 }
	| INUM { $1 }
	| TRUE { $1 }
	| FALSE { $1 } 
	| VAR			{ try Hashtbl.find var_table $1
				  with Not_found ->
				    printf "no such variable '%s'\n" $1;
				    0.0
				}
	| VAR EQ exp		{ Hashtbl.replace var_table $1 $3;
				  $3
				}
	| FNCT LPAREN exp RPAREN	{ $1 $3 }
	| exp LESS exp    { $1 < $3 }
	| exp PLUS exp		{ $1 +. $3 }
	| exp MINUS exp		{ $1 -. $3 }
	| exp MULTIPLY exp	{ $1 *. $3 }
	| exp DIVIDE exp	{ $1 /. $3 }
	| MINUS exp %prec NEG	{ -. $2 }
	| exp CARET exp		{ $1 ** $3 }
	| LPAREN exp RPAREN	{ $2 }
;
%%