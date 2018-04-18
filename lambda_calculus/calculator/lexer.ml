# 1 "lexer.mll"
 
(* 補助的な変数、関数、型などの定義 *)
open Parser

# 7 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\233\255\234\255\235\255\236\255\237\255\010\000\000\000\
    \242\255\016\000\010\000\029\000\014\000\028\000\033\000\031\000\
    \252\255\000\000\002\000\004\000\254\255\020\000\033\000\034\000\
    \251\255\250\255\031\000\039\000\241\255\023\000\037\000\030\000\
    \026\000\039\000\043\000\030\000\041\000\032\000\249\255\048\000\
    \048\000\053\000\034\000\053\000\052\000\248\255\045\000\054\000\
    \243\255\040\000\238\255\056\000\058\000\247\255\046\000\056\000\
    \065\000\066\000\057\000\065\000\052\000\246\255\245\255\058\000\
    \058\000\055\000\070\000\072\000\244\255\055\000\073\000\061\000\
    \061\000\073\000\068\000\070\000\239\255\064\000\077\000\065\000\
    \087\000\077\000\085\000\077\000\089\000\088\000\240\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\020\000\022\000\
    \255\255\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \255\255\002\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\006\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\009\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\015\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\019\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\018\000\018\000\018\000\018\000\018\000\020\000\018\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \018\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\
    \017\000\016\000\019\000\000\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\015\000\003\000\009\000\003\000\006\000\012\000\003\000\
    \003\000\010\000\003\000\003\000\005\000\003\000\003\000\003\000\
    \014\000\003\000\013\000\003\000\003\000\003\000\011\000\003\000\
    \003\000\003\000\003\000\077\000\064\000\062\000\054\000\063\000\
    \051\000\039\000\029\000\023\000\026\000\025\000\024\000\027\000\
    \028\000\030\000\031\000\021\000\032\000\022\000\033\000\034\000\
    \035\000\036\000\037\000\038\000\042\000\049\000\046\000\043\000\
    \044\000\045\000\047\000\048\000\050\000\052\000\041\000\053\000\
    \055\000\056\000\057\000\040\000\058\000\059\000\060\000\061\000\
    \069\000\065\000\066\000\067\000\068\000\070\000\071\000\072\000\
    \073\000\028\000\074\000\075\000\076\000\078\000\079\000\080\000\
    \081\000\082\000\083\000\084\000\085\000\086\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\018\000\018\000\000\000\019\000\018\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\018\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\017\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\009\000\010\000\011\000\009\000\
    \012\000\013\000\014\000\015\000\021\000\022\000\023\000\026\000\
    \027\000\029\000\030\000\015\000\031\000\015\000\032\000\033\000\
    \034\000\035\000\036\000\037\000\039\000\040\000\041\000\042\000\
    \043\000\044\000\046\000\047\000\049\000\051\000\039\000\052\000\
    \054\000\055\000\056\000\039\000\057\000\058\000\059\000\060\000\
    \063\000\064\000\065\000\066\000\067\000\069\000\070\000\071\000\
    \072\000\007\000\073\000\074\000\075\000\077\000\078\000\079\000\
    \080\000\081\000\082\000\083\000\084\000\085\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\019\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 15 "lexer.mll"
         ( token lexbuf )
# 156 "lexer.ml"

  | 1 ->
# 17 "lexer.mll"
  ( token lexbuf )
# 161 "lexer.ml"

  | 2 ->
# 18 "lexer.mll"
       ( LPAREN )
# 166 "lexer.ml"

  | 3 ->
# 19 "lexer.mll"
       ( RPAREN )
# 171 "lexer.ml"

  | 4 ->
# 20 "lexer.mll"
         ( ADD )
# 176 "lexer.ml"

  | 5 ->
# 21 "lexer.mll"
         ( AND )
# 181 "lexer.ml"

  | 6 ->
# 22 "lexer.mll"
                    ( PARENT )
# 186 "lexer.ml"

  | 7 ->
# 23 "lexer.mll"
           ( REDUCE )
# 191 "lexer.ml"

  | 8 ->
# 24 "lexer.mll"
         ( FREE )
# 196 "lexer.ml"

  | 9 ->
# 25 "lexer.mll"
                  ( VARIABLES )
# 201 "lexer.ml"

  | 10 ->
# 26 "lexer.mll"
         ( IS )
# 206 "lexer.ml"

  | 11 ->
# 27 "lexer.mll"
           ( CLOSED )
# 211 "lexer.ml"

  | 12 ->
# 28 "lexer.mll"
           ( RENAME )
# 216 "lexer.ml"

  | 13 ->
# 29 "lexer.mll"
         ( EQUAL )
# 221 "lexer.ml"

  | 14 ->
# 30 "lexer.mll"
                 (ALPHA)
# 226 "lexer.ml"

  | 15 ->
# 31 "lexer.mll"
                    ( EQ )
# 231 "lexer.ml"

  | 16 ->
# 32 "lexer.mll"
               ( CONVERSION )
# 236 "lexer.ml"

  | 17 ->
# 33 "lexer.mll"
          ( RESET )
# 241 "lexer.ml"

  | 18 ->
# 34 "lexer.mll"
       ( LAMBDA )
# 246 "lexer.ml"

  | 19 ->
# 35 "lexer.mll"
       ( DOT )
# 251 "lexer.ml"

  | 20 ->
# 37 "lexer.mll"
    ( VAR (Lexing.lexeme lexbuf) )
# 256 "lexer.ml"

  | 21 ->
# 38 "lexer.mll"
       ( EOF )
# 261 "lexer.ml"

  | 22 ->
# 39 "lexer.mll"
     ( failwith ("unknown token: " ^ Lexing.lexeme lexbuf) )
# 266 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;
