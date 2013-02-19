type token =
  | NUM of (float)
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | CARET
  | UMINUS
  | NEWLINE

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
