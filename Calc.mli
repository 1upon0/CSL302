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
  | TRUE of (bool)
  | FALSE of (bool)
  | FNUM of (float)
  | INUM of (int)
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
  | FNCT of (float->float)

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> expr	
