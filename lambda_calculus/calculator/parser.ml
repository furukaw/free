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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* 補助的な変数、関数、型などの定義 *)
# 28 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* ADD *);
  260 (* REDUCE *);
  261 (* PARENT *);
  262 (* FREE *);
  263 (* VARIABLES *);
  264 (* IS *);
  265 (* CLOSED *);
  266 (* RENAME *);
  267 (* RESET *);
  268 (* LAMBDA *);
  269 (* DOT *);
  270 (* AND *);
  271 (* EQUAL *);
  272 (* ALPHA *);
  273 (* EQ *);
  274 (* CONVERSION *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  275 (* VAR *);
    0|]

let yylhs = "\255\255\
\003\000\003\000\004\000\004\000\005\000\005\000\005\000\005\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\003\000\003\000\002\000\002\000\003\000\002\000\003\000\
\001\000\002\000\001\000\001\000\002\000\002\000\002\000\002\000\
\003\000\002\000\002\000\002\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\001\000\023\000\012\000\
\000\000\000\000\000\000\013\000\014\000\015\000\016\000\000\000\
\000\000\010\000\018\000\019\000\020\000\021\000\000\000\005\000\
\000\000\007\000\002\000\017\000\000\000\004\000\006\000\008\000\
\003\000"

let yydgoto = "\002\000\
\015\000\016\000\017\000\026\000\018\000"

let yysindex = "\011\000\
\003\255\000\000\004\255\254\254\024\255\030\255\031\255\020\255\
\000\000\022\255\004\255\026\255\008\255\000\000\000\000\000\000\
\009\255\019\255\041\255\000\000\000\000\000\000\000\000\025\255\
\014\255\000\000\000\000\000\000\000\000\000\000\022\255\000\000\
\022\255\000\000\000\000\000\000\004\255\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\253\255\018\000\255\255\000\000"

let yytablesize = 260
let yytable = "\019\000\
\009\000\011\000\020\000\003\000\003\000\004\000\005\000\027\000\
\006\000\003\000\007\000\001\000\008\000\009\000\010\000\010\000\
\011\000\012\000\013\000\003\000\031\000\014\000\014\000\038\000\
\029\000\030\000\037\000\014\000\021\000\039\000\033\000\040\000\
\025\000\041\000\032\000\034\000\022\000\014\000\024\000\023\000\
\025\000\028\000\035\000\036\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\011\000"

let yycheck = "\003\000\
\000\000\000\000\005\001\001\001\001\001\003\001\004\001\011\000\
\006\001\001\001\008\001\001\000\010\001\011\001\012\001\012\001\
\014\001\015\001\016\001\001\001\012\001\019\001\019\001\025\000\
\017\001\018\001\013\001\019\001\005\001\031\000\012\001\033\000\
\019\001\037\000\017\000\018\000\007\001\019\001\019\001\009\001\
\019\001\016\001\002\001\019\001\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\002\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  ADD\000\
  REDUCE\000\
  PARENT\000\
  FREE\000\
  VARIABLES\000\
  IS\000\
  CLOSED\000\
  RENAME\000\
  RESET\000\
  LAMBDA\000\
  DOT\000\
  AND\000\
  EQUAL\000\
  ALPHA\000\
  EQ\000\
  CONVERSION\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
 ( Syntax.Var (_1) )
# 199 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.lam) in
    Obj.repr(
# 36 "parser.mly"
 ( _2 )
# 206 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.lam) in
    Obj.repr(
# 40 "parser.mly"
       ( Syntax.Abs (_1, [], _3, true) )
# 214 "parser.ml"
               : 'abs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 42 "parser.mly"
 ( Syntax.Abs (_1, [], _2, true) )
# 222 "parser.ml"
               : 'abs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 46 "parser.mly"
 ( Syntax.App (_1, _2, true) )
# 230 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 48 "parser.mly"
   ( Syntax.App (_1, _3, true) )
# 238 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 50 "parser.mly"
   ( Syntax.App (_1, _2, true) )
# 246 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'app) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 52 "parser.mly"
       ( Syntax.App (_1, _3, true) )
# 254 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 56 "parser.mly"
 ( _1 )
# 261 "parser.ml"
               : Syntax.lam))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 58 "parser.mly"
   ( _2 )
# 268 "parser.ml"
               : Syntax.lam))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app) in
    Obj.repr(
# 60 "parser.mly"
 ( _1 )
# 275 "parser.ml"
               : Syntax.lam))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.lam) in
    Obj.repr(
# 64 "parser.mly"
 ( Syntax.Lam (_1) )
# 282 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
        ( Syntax.Max )
# 288 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
        ( Syntax.Min )
# 294 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
        ( Syntax.FV )
# 300 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
 ( Syntax.IsClosed )
# 306 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
   ( Syntax.Rename (_2, _3) )
# 314 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.lam) in
    Obj.repr(
# 76 "parser.mly"
 ( Syntax.And (_2) )
# 321 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
   ( Syntax.AlphaEq )
# 327 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
        ( Syntax.AlphaEq )
# 333 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
        ( Syntax.AlphaEq )
# 339 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
 ( Syntax.Reset )
# 345 "parser.ml"
               : Syntax.t))
(* Entry expr *)
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
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t)
