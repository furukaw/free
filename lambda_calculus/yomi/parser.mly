%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token LAMBDA DOT
%token <string> VAR
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%left DOT
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| VAR
	{ Syntax.Var ($1) }
| LPAREN expr RPAREN
	{ $2 }

abs:
| VAR DOT expr
      	{ Syntax.Abs ($1, $3) }
| VAR abs
	{ Syntax.Abs ($1, $2) }

app:
| simple_expr simple_expr
	{ Syntax.App ($1, $2) }
| app simple_expr
  	{ Syntax.App ($1, $2) }
| app LAMBDA abs
      	{ Syntax.App ($1, $3) }

expr:
| simple_expr
	{ $1 }
| LAMBDA abs
  	{ $2 }
| app
	{ $1 }
