type token =
  | LPAREN
  | RPAREN
  | MINUS
  | PLUS
  | MINUSDOT
  | PLUSDOT
  | TIMES
  | DIVIDE
  | TIMESDOT
  | DIVIDEDOT
  | MOD
  | EQUAL
  | NEQUAL
  | NMORE
  | NLESS
  | LESS
  | MORE
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | REC
  | NUMBER of (int)
  | REAL of (float)
  | VAR of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
