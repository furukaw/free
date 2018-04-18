type token =
  | LPAREN
  | RPAREN
  | ADD
  | REDUCE
  | PARENT
  | FREE
  | VARIABLES
  | IS
  | CLOSED
  | RENAME
  | RESET
  | LAMBDA
  | DOT
  | AND
  | EQUAL
  | ALPHA
  | EQ
  | CONVERSION
  | VAR of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
