{
(* 補助的な変数、関数、型などの定義 *)
open Parser
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "+."	 { PLUSDOT }
| "-."	 { MINUSDOT }
| "*."	 { TIMESDOT }
| "/."	 { DIVIDEDOT }
| "+"	 { PLUS }
| "-"	 { MINUS }
| "*"	 { TIMES }
| "/"	 { DIVIDE }
| "mod"	 { MOD }
| "="	 { EQUAL }
| "<>"	 { NEQUAL }
| "<="	 { NMORE }
| ">="	 { NLESS }
| "<"	 { LESS }
| ">"	 { MORE }
| "if"	 { IF }
| "then" { THEN }
| "else" { ELSE }
| "let"	 { LET }
| "in"	 { IN }
| "rec"	 { REC }
| digit+'.'digit*
	 { REAL (float_of_string (Lexing.lexeme lexbuf)) }
| digit+                        (* 数字が１個以上 *)
	 { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| lower+(alpha | digit)*
  	 { VAR (Lexing.lexeme lexbuf) }
| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
