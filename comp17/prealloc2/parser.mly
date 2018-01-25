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
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

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
| LPAREN expr RPAREN
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

expr:
| simple_expr
	{ $1 }
| expr PLUS expr
        { Syntax.Op ($1, Operator.Plus, $3) }
| expr MINUS expr
	{ Syntax.Op ($1, Operator.Minus, $3) }
| expr PLUSDOT expr
        { Syntax.Op ($1, Operator.PlusDot, $3) }
| expr MINUSDOT expr
	{ Syntax.Op ($1, Operator.MinusDot, $3) }
| expr TIMES expr
	{ Syntax.Op ($1, Operator.Times, $3) }
| expr DIVIDE expr
	{ Syntax.Op ($1, Operator.Divide, $3) }
| expr MOD expr
        { Syntax.Op ($1, Operator.Mod, $3) }
| expr TIMESDOT expr
	{ Syntax.Op ($1, Operator.TimesDot, $3) }
| expr DIVIDEDOT expr
	{ Syntax.Op ($1, Operator.DivideDot, $3) }	
| IF expr EQUAL expr THEN expr ELSE expr
     	{ Syntax.IfEqual ($2, $4, $6, $8) }
| IF expr NEQUAL expr THEN expr ELSE expr
     	{ Syntax.IfEqual ($2, $4, $8, $6) }
| IF expr NMORE expr THEN expr ELSE expr
     	{ Syntax.IfLess ($4, $2, $8, $6) }
| IF expr NLESS expr THEN expr ELSE expr
     	{ Syntax.IfLess ($2, $4, $8, $6) }
| IF expr LESS expr THEN expr ELSE expr
     	{ Syntax.IfLess ($2, $4, $6, $8) }
| IF expr MORE expr THEN expr ELSE expr
     	{ Syntax.IfLess ($4, $2, $6, $8) }
| LET VAR EQUAL expr IN expr
      	{ Syntax.Let (($2, Type.gen_type ()), $4, $6) }
| LET REC VAR vars EQUAL expr IN expr
      	{ Syntax.LetRec (($3, Type.gen_type ()), $4, $6, $8) }
| simple_expr app
  	{ Syntax.Application ($1, $2) }