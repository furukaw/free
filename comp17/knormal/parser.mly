%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN
%token RPAREN
%token MINUS PLUS MINUSDOT PLUSDOT
%token TIMES DIVIDE TIMESDOT DIVIDEDOT MOD
%token EQUAL NEQUAL NMORE NLESS LESS MORE
%token IF THEN ELSE LET IN REC
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token <float> REAL
%token <string> VAR
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 開始記号の定義 */
%start start

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%right ELSE IN
%left MINUS PLUS MINUSDOT PLUSDOT
%left TIMES DIVIDE TIMESDOT DIVIDEDOT MOD
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
	{ Syntax.Number ($1) }
| REAL
	{ Syntax.Real ($1) }
| VAR
	{ Syntax.Variable ($1) }
| LPAREN start RPAREN
	{ $2 }

vars:
| VAR
	{ [($1, Type.gen_type ())] }
| VAR vars
	{ ($1, Type.gen_type ()) :: $2 }

app:
| simple_expr
	{ [$1] }
| simple_expr app
  	{ $1 :: $2 }

start:
| simple_expr
	{ $1 }
| start PLUS start
        { Syntax.Op ($1, Operator.Plus, $3) }
| start MINUS start
	{ Syntax.Op ($1, Operator.Minus, $3) }
| start PLUSDOT start
        { Syntax.Op ($1, Operator.PlusDot, $3) }
| start MINUSDOT start
	{ Syntax.Op ($1, Operator.MinusDot, $3) }
| start TIMES start
	{ Syntax.Op ($1, Operator.Times, $3) }
| start DIVIDE start
	{ Syntax.Op ($1, Operator.Divide, $3) }
| start MOD start
        { Syntax.Op ($1, Operator.Mod, $3) }
| start TIMESDOT start
	{ Syntax.Op ($1, Operator.TimesDot, $3) }
| start DIVIDEDOT start
	{ Syntax.Op ($1, Operator.DivideDot, $3) }	
| IF start EQUAL start THEN start ELSE start
     	{ Syntax.IfEqual ($2, $4, $6, $8) }
| IF start NEQUAL start THEN start ELSE start
     	{ Syntax.IfEqual ($2, $4, $8, $6) }
| IF start NMORE start THEN start ELSE start
     	{ Syntax.IfLess ($4, $2, $8, $6) }
| IF start NLESS start THEN start ELSE start
     	{ Syntax.IfLess ($2, $4, $8, $6) }
| IF start LESS start THEN start ELSE start
     	{ Syntax.IfLess ($2, $4, $6, $8) }
| IF start MORE start THEN start ELSE start
     	{ Syntax.IfLess ($4, $2, $6, $8) }
| LET VAR EQUAL start IN start
      	{ Syntax.Let (($2, Type.gen_type ()), $4, $6) }
| LET REC VAR vars EQUAL start IN start
      	{ Syntax.LetRec (($3, Type.gen_type ()), $4, $6, $8) }
| simple_expr app
  	{ Syntax.Application ($1, $2) }