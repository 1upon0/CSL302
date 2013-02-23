
{
  open Calc
  open Lexing
	let create_hashtable size init =
	let tbl = Hashtbl.create size in
	List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
	tbl

  
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | '\n'	{ NEWLINE }
  | digit+ as inum
		{ INUM (int_of_string inum) }
  | "." digit+ 
  | digit+ "." digit* as num
		{ FNUM (float_of_string num) }
  | '+'		{ IPLUS }
  | '-'		{ IMINUS }
  | '*'		{ IMULTIPLY }
  | '/'		{ IDIVIDE }
	| "mod"	{ IMOD }
	| ".+"		{ FPLUS }
  | ".-"		{ FMINUS }
  | ".*"		{ FMULTIPLY }
  | "./"		{ FDIVIDE }
	| ".mod"	{ FMOD }
	| ".^"  { FPOWER }
  | '^'		{ IPOWER }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | '='		{ EQ }
	| 'T'   { TRUE (true) }
	| 'F'   { FALSE (false) }
	| '<'   { LESS }
	| '>'   { GREATER }
	| '='		{ EQ }
	| "<="
	| "=<"	{ LESSEQ }
	| "=>"
	| ">="	{ GREATEREQ }
	| "/\\" { OR }
	| "\\/" { AND }
	| "/="  { NOT }
  | ident ident_num* as word
  		{ VAR  ( word)
  		}
  | _		{ token lexbuf }
  | eof		{ raise End_of_file }