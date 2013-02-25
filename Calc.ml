type token =
  | NEWLINE
  | OR
  | AND
  | NOT
  | LPAREN
  | RPAREN
  | EQ
  | LESSEQ
  | GREATEREQ
  | LESS
  | GREATER
  | COMMA
  | TRUE of (bool)
  | FALSE of (bool)
  | FNUM of (string)
  | INUM of (string)
  | IPLUS
  | IMINUS
  | IMULTIPLY
  | IDIVIDE
  | CARET
  | FPLUS
  | FMINUS
  | FMULTIPLY
  | FDIVIDE
  | IMOD
  | FMOD
  | IPOWER
  | FPOWER
  | VAR of (string)

open Parsing;;
# 2 "Calc.mly"

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
		
# 107 "Calc.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
  258 (* OR *);
  259 (* AND *);
  260 (* NOT *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* EQ *);
  264 (* LESSEQ *);
  265 (* GREATEREQ *);
  266 (* LESS *);
  267 (* GREATER *);
  268 (* COMMA *);
  273 (* IPLUS *);
  274 (* IMINUS *);
  275 (* IMULTIPLY *);
  276 (* IDIVIDE *);
  277 (* CARET *);
  278 (* FPLUS *);
  279 (* FMINUS *);
  280 (* FMULTIPLY *);
  281 (* FDIVIDE *);
  282 (* IMOD *);
  283 (* FMOD *);
  284 (* IPOWER *);
  285 (* FPOWER *);
    0|]

let yytransl_block = [|
  269 (* TRUE *);
  270 (* FALSE *);
  271 (* FNUM *);
  272 (* INUM *);
  286 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\002\000\002\000\002\000\002\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\003\000\003\000\004\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\002\000\003\000\003\000\004\000\001\000\001\000\
\003\000\003\000\002\000\003\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\003\000\000\000\000\000\031\000\
\032\000\022\000\008\000\000\000\000\000\000\000\002\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\044\000\030\000"

let yydgoto = "\002\000\
\003\000\015\000\022\000\067\000\068\000"

let yysindex = "\011\000\
\000\000\000\000\002\255\000\255\000\000\082\255\061\255\000\000\
\000\000\000\000\000\000\003\255\170\255\006\255\000\000\172\255\
\164\255\185\255\000\000\003\255\050\255\114\000\017\255\068\255\
\009\000\123\000\234\255\041\255\170\255\248\255\051\255\067\255\
\067\255\000\000\003\255\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\003\255\003\255\003\255\000\000\082\255\082\255\
\082\255\000\000\170\255\170\255\170\255\170\255\170\255\082\255\
\082\255\061\255\061\255\000\000\000\000\003\255\003\255\170\255\
\170\255\240\255\223\000\130\000\170\000\017\255\208\255\170\000\
\170\000\170\000\170\000\170\000\170\000\126\255\126\255\041\255\
\041\255\041\255\017\255\017\255\201\255\208\255\077\255\077\255\
\051\255\051\255\019\000\170\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\074\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\105\255\000\000\000\000\
\000\000\000\000\000\000\000\000\131\255\000\000\230\000\250\255\
\000\000\000\000\152\255\040\000\000\000\193\255\200\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\213\000\236\000\022\255\242\000\
\248\000\254\000\004\001\010\001\191\000\155\000\168\000\060\000\
\080\000\100\000\016\001\022\001\030\255\228\255\087\255\098\255\
\213\255\220\255\000\000\202\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\002\000\021\000\253\255"

let yytablesize = 540
let yytable = "\018\000\
\019\000\004\000\005\000\026\000\016\000\006\000\007\000\020\000\
\025\000\031\000\032\000\001\000\033\000\028\000\008\000\009\000\
\010\000\011\000\011\000\012\000\012\000\025\000\021\000\017\000\
\013\000\026\000\023\000\021\000\049\000\071\000\043\000\014\000\
\027\000\066\000\069\000\043\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\086\000\
\087\000\088\000\089\000\090\000\040\000\070\000\056\000\071\000\
\057\000\066\000\069\000\091\000\092\000\071\000\051\000\091\000\
\092\000\007\000\045\000\083\000\084\000\085\000\006\000\007\000\
\058\000\045\000\059\000\010\000\011\000\070\000\012\000\008\000\
\009\000\010\000\011\000\013\000\012\000\006\000\020\000\023\000\
\051\000\013\000\024\000\000\000\023\000\000\000\008\000\009\000\
\014\000\011\000\024\000\012\000\054\000\055\000\000\000\024\000\
\000\000\009\000\037\000\037\000\023\000\023\000\009\000\021\000\
\009\000\009\000\009\000\009\000\009\000\000\000\000\000\024\000\
\024\000\009\000\009\000\009\000\009\000\000\000\020\000\020\000\
\020\000\020\000\009\000\037\000\037\000\037\000\000\000\000\000\
\009\000\040\000\009\000\009\000\009\000\009\000\009\000\000\000\
\043\000\044\000\000\000\009\000\009\000\009\000\009\000\045\000\
\009\000\009\000\009\000\000\000\009\000\009\000\000\000\009\000\
\009\000\009\000\009\000\009\000\046\000\047\000\048\000\000\000\
\009\000\009\000\009\000\009\000\034\000\000\000\029\000\049\000\
\000\000\009\000\035\000\036\000\037\000\038\000\039\000\040\000\
\010\000\050\000\000\000\000\000\041\000\042\000\043\000\044\000\
\013\000\020\000\000\000\000\000\051\000\045\000\020\000\030\000\
\027\000\000\000\047\000\048\000\020\000\027\000\052\000\053\000\
\054\000\055\000\000\000\000\000\049\000\025\000\020\000\020\000\
\020\000\020\000\025\000\051\000\026\000\027\000\027\000\027\000\
\027\000\026\000\000\000\000\000\029\000\052\000\053\000\054\000\
\055\000\029\000\025\000\025\000\025\000\025\000\062\000\000\000\
\063\000\026\000\026\000\026\000\026\000\093\000\035\000\036\000\
\037\000\038\000\039\000\040\000\064\000\000\000\065\000\009\000\
\041\000\042\000\043\000\044\000\000\000\009\000\000\000\000\000\
\000\000\045\000\009\000\009\000\009\000\009\000\060\000\020\000\
\020\000\020\000\020\000\009\000\040\000\000\000\000\000\000\000\
\093\000\041\000\042\000\043\000\044\000\000\000\040\000\000\000\
\000\000\000\000\045\000\041\000\042\000\043\000\044\000\000\000\
\015\000\015\000\015\000\000\000\045\000\015\000\015\000\015\000\
\015\000\015\000\015\000\000\000\000\000\000\000\000\000\000\000\
\015\000\015\000\015\000\015\000\013\000\013\000\013\000\000\000\
\000\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
\000\000\000\000\000\000\000\000\013\000\013\000\013\000\013\000\
\014\000\014\000\014\000\000\000\000\000\014\000\014\000\014\000\
\014\000\014\000\014\000\000\000\000\000\000\000\000\000\000\000\
\014\000\014\000\014\000\014\000\016\000\016\000\016\000\000\000\
\000\000\016\000\016\000\016\000\016\000\016\000\016\000\000\000\
\000\000\000\000\000\000\000\000\016\000\016\000\016\000\016\000\
\035\000\036\000\037\000\038\000\039\000\040\000\000\000\000\000\
\061\000\000\000\041\000\042\000\043\000\044\000\051\000\095\000\
\000\000\000\000\000\000\045\000\000\000\051\000\000\000\000\000\
\052\000\053\000\054\000\055\000\000\000\000\000\000\000\052\000\
\053\000\054\000\055\000\011\000\011\000\011\000\000\000\000\000\
\011\000\011\000\011\000\011\000\011\000\011\000\000\000\000\000\
\012\000\012\000\012\000\011\000\011\000\012\000\012\000\012\000\
\012\000\012\000\012\000\000\000\000\000\040\000\000\000\000\000\
\012\000\012\000\041\000\042\000\043\000\044\000\000\000\018\000\
\018\000\018\000\000\000\045\000\018\000\018\000\018\000\018\000\
\018\000\018\000\010\000\010\000\010\000\000\000\000\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\000\000\000\000\
\000\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
\047\000\048\000\000\000\000\000\094\000\000\000\035\000\035\000\
\035\000\000\000\049\000\035\000\038\000\038\000\038\000\000\000\
\000\000\038\000\040\000\040\000\040\000\000\000\000\000\040\000\
\041\000\041\000\041\000\000\000\000\000\041\000\042\000\042\000\
\042\000\000\000\000\000\042\000\036\000\036\000\036\000\000\000\
\000\000\036\000\039\000\039\000\039\000\000\000\000\000\039\000\
\033\000\033\000\033\000\000\000\000\000\033\000\034\000\034\000\
\034\000\000\000\000\000\034\000"

let yycheck = "\003\000\
\001\001\000\001\001\001\007\000\003\000\004\001\005\001\005\001\
\007\000\013\000\005\001\001\000\007\001\012\000\013\001\014\001\
\015\001\016\001\016\001\018\001\018\001\020\000\001\001\003\000\
\023\001\029\000\006\000\006\001\012\001\033\000\001\001\030\001\
\030\001\032\000\033\000\006\001\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\051\000\
\052\000\053\000\054\000\055\000\012\001\033\000\005\001\059\000\
\007\001\056\000\057\000\058\000\059\000\065\000\012\001\062\000\
\063\000\005\001\026\001\047\000\048\000\049\000\004\001\005\001\
\005\001\000\000\007\001\015\001\016\001\057\000\018\001\013\001\
\014\001\015\001\016\001\023\001\018\001\004\001\005\001\001\001\
\012\001\023\001\030\001\255\255\006\001\255\255\013\001\014\001\
\030\001\016\001\001\001\018\001\024\001\025\001\255\255\006\001\
\255\255\001\001\002\001\003\001\022\001\023\001\006\001\030\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\022\001\
\023\001\017\001\018\001\019\001\020\001\255\255\022\001\023\001\
\024\001\025\001\026\001\001\001\002\001\003\001\255\255\255\255\
\006\001\012\001\008\001\009\001\010\001\011\001\012\001\255\255\
\019\001\020\001\255\255\017\001\018\001\019\001\020\001\026\001\
\001\001\002\001\003\001\255\255\026\001\006\001\255\255\008\001\
\009\001\010\001\011\001\012\001\001\001\002\001\003\001\255\255\
\017\001\018\001\019\001\020\001\001\001\255\255\005\001\012\001\
\255\255\026\001\007\001\008\001\009\001\010\001\011\001\012\001\
\015\001\001\001\255\255\255\255\017\001\018\001\019\001\020\001\
\023\001\001\001\255\255\255\255\012\001\026\001\006\001\030\001\
\001\001\255\255\002\001\003\001\012\001\006\001\022\001\023\001\
\024\001\025\001\255\255\255\255\012\001\001\001\022\001\023\001\
\024\001\025\001\006\001\012\001\001\001\022\001\023\001\024\001\
\025\001\006\001\255\255\255\255\001\001\022\001\023\001\024\001\
\025\001\006\001\022\001\023\001\024\001\025\001\005\001\255\255\
\007\001\022\001\023\001\024\001\025\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\005\001\255\255\007\001\006\001\
\017\001\018\001\019\001\020\001\255\255\012\001\255\255\255\255\
\255\255\026\001\017\001\018\001\019\001\020\001\006\001\022\001\
\023\001\024\001\025\001\026\001\012\001\255\255\255\255\255\255\
\006\001\017\001\018\001\019\001\020\001\255\255\012\001\255\255\
\255\255\255\255\026\001\017\001\018\001\019\001\020\001\255\255\
\001\001\002\001\003\001\255\255\026\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\001\001\002\001\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\001\001\002\001\003\001\255\255\255\255\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\001\001\002\001\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\006\001\255\255\017\001\018\001\019\001\020\001\012\001\006\001\
\255\255\255\255\255\255\026\001\255\255\012\001\255\255\255\255\
\022\001\023\001\024\001\025\001\255\255\255\255\255\255\022\001\
\023\001\024\001\025\001\001\001\002\001\003\001\255\255\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\001\001\002\001\003\001\017\001\018\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\012\001\255\255\255\255\
\017\001\018\001\017\001\018\001\019\001\020\001\255\255\001\001\
\002\001\003\001\255\255\026\001\006\001\007\001\008\001\009\001\
\010\001\011\001\001\001\002\001\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\001\001\255\255\255\255\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\002\001\003\001\255\255\255\255\006\001\255\255\001\001\002\001\
\003\001\255\255\012\001\006\001\001\001\002\001\003\001\255\255\
\255\255\006\001\001\001\002\001\003\001\255\255\255\255\006\001\
\001\001\002\001\003\001\255\255\255\255\006\001\001\001\002\001\
\003\001\255\255\255\255\006\001\001\001\002\001\003\001\255\255\
\255\255\006\001\001\001\002\001\003\001\255\255\255\255\006\001\
\001\001\002\001\003\001\255\255\255\255\006\001\001\001\002\001\
\003\001\255\255\255\255\006\001"

let yynames_const = "\
  NEWLINE\000\
  OR\000\
  AND\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  LESSEQ\000\
  GREATEREQ\000\
  LESS\000\
  GREATER\000\
  COMMA\000\
  IPLUS\000\
  IMINUS\000\
  IMULTIPLY\000\
  IDIVIDE\000\
  CARET\000\
  FPLUS\000\
  FMINUS\000\
  FMULTIPLY\000\
  FDIVIDE\000\
  IMOD\000\
  FMOD\000\
  IPOWER\000\
  FPOWER\000\
  "

let yynames_block = "\
  TRUE\000\
  FALSE\000\
  FNUM\000\
  INUM\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "Calc.mly"
                   ( )
# 391 "Calc.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 100 "Calc.mly"
              ( )
# 399 "Calc.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "Calc.mly"
               ( )
# 405 "Calc.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 103 "Calc.mly"
               ( print_exp _1; printf "\t\n";flush stdout )
# 412 "Calc.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 104 "Calc.mly"
                ( print_exp _1;printf "\t  \n" ; flush stdout )
# 419 "Calc.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fexp) in
    Obj.repr(
# 105 "Calc.mly"
                ( print_exp _1;printf "\t  \n" ; flush stdout )
# 426 "Calc.ml"
               : 'line))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "Calc.mly"
                 (printf "\t error \n" ; flush stdout )
# 432 "Calc.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "Calc.mly"
          ( Var  _1 )
# 439 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "Calc.mly"
         ( Var _1 )
# 446 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 110 "Calc.mly"
               (  Binop(Var _1 ,Eq , _3 )
				)
# 455 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 113 "Calc.mly"
                  (  Binop( _1 ,IAdd , _3 )  )
# 463 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 114 "Calc.mly"
                   ( Binop( _1 ,ISub , _3 )  )
# 471 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 115 "Calc.mly"
                     ( Binop( _1 ,IMul , _3 )  )
# 479 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 116 "Calc.mly"
                   ( Binop( _1 ,IDiv , _3 )  )
# 487 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 118 "Calc.mly"
                        ( Unop(Neg,_2) )
# 494 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 120 "Calc.mly"
                   (Binop(_1,Mod,_3) )
# 502 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 121 "Calc.mly"
                     ( _2 )
# 509 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 122 "Calc.mly"
                 (Binop(_1,Comma,_3) )
# 517 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 123 "Calc.mly"
                         ( Binop( Var _1, App ,_3) )
# 525 "Calc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "Calc.mly"
         ( Var _1 )
# 532 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 127 "Calc.mly"
                (  Binop(Var _1 ,Eq , _3 )
				)
# 541 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "Calc.mly"
       ( Var _1 )
# 548 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 131 "Calc.mly"
                    ( Binop( _1 ,FAdd , _3 )  )
# 556 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 132 "Calc.mly"
                     ( Binop( _1 ,FSub , _3 )  )
# 564 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 133 "Calc.mly"
                       ( Binop( _1 ,FMul , _3 )  )
# 572 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 134 "Calc.mly"
                     ( Binop( _1 , FDiv, _3 )  )
# 580 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 136 "Calc.mly"
                         ( Unop(Neg,_2) )
# 587 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fexp) in
    Obj.repr(
# 138 "Calc.mly"
                      ( _2 )
# 594 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fexp) in
    Obj.repr(
# 139 "Calc.mly"
                   (Binop(_1,Comma,_3) )
# 602 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'fexp) in
    Obj.repr(
# 140 "Calc.mly"
                          ( Binop( Var _1, App ,_3) )
# 610 "Calc.ml"
               : 'fexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 142 "Calc.mly"
           ( Var  "True" )
# 617 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 143 "Calc.mly"
         ( Var "False" )
# 624 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 144 "Calc.mly"
                ( Binop (_1,Or,_3))
# 632 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 146 "Calc.mly"
                 ( Binop (_1,And,_3) )
# 640 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 147 "Calc.mly"
            ( Unop (Neg,_2) )
# 647 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 148 "Calc.mly"
                ( Binop (_1,Less,_3))
# 655 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "Calc.mly"
         (Var _1
				)
# 663 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 151 "Calc.mly"
                ( Binop(Var _1 ,Eq , _3 )
				)
# 672 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 153 "Calc.mly"
                   ( Binop (_1,Greater,_3))
# 680 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 154 "Calc.mly"
              ( Binop (_1,Eq,_3) )
# 688 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 155 "Calc.mly"
                  ( Binop (_1,LessE,_3) )
# 696 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 156 "Calc.mly"
                     ( Binop (_1,GreaterE,_3) )
# 704 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 157 "Calc.mly"
                   (Binop(_1,Comma,_3) )
# 712 "Calc.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 158 "Calc.mly"
                          ( Binop( Var _1, App ,_3) )
# 720 "Calc.ml"
               : 'bexp))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
