{
  open Calc
}


  type token =
    | CASE of char
    | LIST of char (*define all the different cases we want to use *)
    | FIELD of char
    | SEPARATOR of char
    | BRACKETS of char
    | PARENTHESIS of char
    | ID of string
    | ARITHOP of string
    | INT of int
    | FLOAT of float
    | CHAR of char
		| VAR of string
		| BOOLOP of string
		| CONTROLOP of string
		| CMPOP of string
		| WHITE
		| TAB
		| NEWLINE


let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
let cap =['A'-'Z']
let letter=['a'-'z' 'A'-'Z']
let pdigit =['1'-'9']

rule scaner = parse   (*all the rules as specified*)
  |'0'  
	| '-'? pdigit digit* as inum
  	{ let num = int_of_string inum in
	  printf "integer: %s (%d)\n" inum num;
	  INT num
	}

  | '-'? pdigit digit* '.' digit* pdigit as fnum
  	{ let num = float_of_string fnum in
	  printf "float: %s (%f)\n" fnum num;
	  FLOAT num
	}
 	| cap id as var
	  {
		 printf "Variable: %s\n " var;
			VAR var
	}	
	| "+"	
  | "-" 
  | "*"
  | "/"
	| "div"
	| "mod"  as aop
  	{ printf "operator: %s\n" aop;
	  ARITHOP aop
	}
	| "/\\"	
  | "\\/"
  | "~"  as bop
  	{ printf "operator: %s\n" bop;
	  BOOLOP bop
	}
	| ";"	
  | "if" 
  | "then"
  | "else"  as cmop
  	{ printf "operator: %s\n" cmop;
	  CONTROLOP cmop
	}
	| "<"	
  | ">" 
  | "="
  | ">="
	| "<="
	| "//="  as cop
  	{ printf "operator: %s\n" cop;
	  CMPOP cop
	}
	 | letter id as word
  	{ 
	    printf "identifier: %s\n" word;
	    ID word
	}
	| '('
 	|	')' as par
	  {
			printf "parenthesis : %c \n" par;
			PARENTHESIS par
		}
	| '['
	|	']' as brac
		{
			printf "brackets : %c \n" brac;
			BRACKETS brac
		}	
	| '.'
	|	'#' as field
		{
			printf "feild quantifier : %c \n" field;
			FIELD field
		}
	| '@' as list
		{
			printf "list : %c \n" list;
			LIST list
		}
	| '|' as case
		{
			printf "case operator : %c \n" case;
			CASE case
		}
	| ',' as sep
		{
			printf "seperator : %c \n" sep;
			SEPARATOR sep
		}				

  | ' '  	
  	{ printf "white spce \n";
			WHITE 
			 }
 	| '\t' {printf "tab \n"; TAB}
	| '\n'   { printf "newline \n" ; NEWLINE}
	
	 | _ as c
  	{ printf "Unrecognized character: %c\n" c;
	  CHAR c
	}
  | eof
  	{ raise End_of_file }

