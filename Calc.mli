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

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
