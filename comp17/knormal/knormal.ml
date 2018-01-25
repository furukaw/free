(* Knormal.t: k-正規形の抽象構文木の型（k-正規形変換の出力） *)

type t = Number of int
       | Real of float
       | Variable of string
       | Op of string * Operator.t * string
       | IfEqual of string * string * t * t
       | IfLess of string * string * t * t
       | Let of (string * Type.t) * t * t
       | LetRec of (string * Type.t) * (string * Type.t) list * t * t
       | Application of string * string list

(* Knormal.print: k-正規形の抽象構文木をプリントする関数（デバッグ用）  *)

let type_on = ref false (* 型推論を実装したら true にする *)

let indent i = String.make i ' '

let rec string_of_expr expr i = match expr with
    Number (num) -> string_of_int num
  | Real (f) -> string_of_float f
  | Variable (name) -> name
  | Op (arg1, op, arg2) ->
	"(" ^ arg1
	    ^ (match op with
		  Operator.Plus -> "+"
		| Operator.Minus -> "-"
		| Operator.Times -> "*"
		| Operator.Divide -> "/"
		| Operator.Mod -> " mod "
		| Operator.PlusDot -> "+."
		| Operator.MinusDot -> "-."
		| Operator.TimesDot -> "*."
		| Operator.DivideDot -> "/.")
	    ^ arg2 ^ ")"
  | IfEqual (arg1, arg2, arg3, arg4) ->
	"if " ^ arg1 ^ "=" ^ arg2 ^ "\n"
	^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
	^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | IfLess (arg1, arg2, arg3, arg4) ->
	"if " ^ arg1 ^ "<" ^ arg2 ^ "\n"
	^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
	^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | Let ((name, t), arg1, arg2) ->
	(if !type_on
	 then "let (" ^ name ^ ":" ^ Type.to_string t ^ ")="
	 else "let " ^ name ^ "=")
	^ string_of_expr arg1 (i+5+String.length name)
	^ " in\n"
	^ indent i ^ string_of_expr arg2 i
  | LetRec ((name, _), args, arg1, arg2) ->
	"let rec " ^ name
	^ List.fold_left
	    (fun str (arg, t) ->
	      if !type_on
	      then str ^ " (" ^ arg ^ ":" ^ Type.to_string t ^ ")"
	      else str ^ " " ^ arg)
	    "" args
	^ " =\n  "
	^ string_of_expr arg1 (i+2) ^ "\n"
	^ indent i ^ "in\n\n" ^ string_of_expr arg2 i
  | Application (name, args) ->
	"(" ^ name ^ " " ^
	(match args with
	    [] -> ""
	  | arg :: args ->
		arg ^
		List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
	^ ")"

let print expr =
  let str = string_of_expr expr 0
  in (print_string str;
      print_newline ())

(* Knormal.size: k-正規形の抽象構文木をの大きさを返す関数 *)

let rec size expr = match expr with
    Number (num) -> 1
  | Real (f) -> 1
  | Variable (name) -> 1
  | Op (arg1, op, arg2) -> 1
  | IfEqual (arg1, arg2, arg3, arg4) -> 1 + size arg3 + size arg4
  | IfLess (arg1, arg2, arg3, arg4) -> 1 + size arg3 + size arg4
  | Let ((name, _), arg1, arg2) -> 1 + size arg1 + size arg2
  | LetRec ((name, _), args, arg1, arg2) -> 1 + size arg1 + size arg2
  | Application (name, args) -> 1

(* k-正規形変換プログラムのメイン *)

exception NotSupported

let rec g expr = match expr with
    Syntax.Number (num) -> Number (num)
  | Syntax.Real (num) -> Real(num)
  | Syntax.Variable (var) -> Variable(var)
  | Syntax.Op (e1, op, e2) ->
    let v1 = Gensym.f "op1" in
    let v2 = Gensym.f "op2" in
    Let ((v1, Type.gen_type ()), g e1,
         Let ((v2, Type.gen_type ()), g e2,
              Op (v1, op, v2)))
  | Syntax.IfEqual (c1, c2, te, ee) ->
    let v1 = Gensym.f "if" in
    let v2 = Gensym.f "if" in
    Let ((v1, Type.gen_type ()), g c1,
         Let ((v2, Type.gen_type ()), g c2,
              IfEqual (v1, v2, g te, g ee)))
  | Syntax.IfLess (c1, c2, te, ee) ->
    let v1 = Gensym.f "if" in
    let v2 = Gensym.f "if" in
    Let ((v1, Type.gen_type ()), g c1,
         Let ((v2, Type.gen_type ()), g c2,
              IfLess (v1, v2, g te, g ee)))
  | Syntax.Let ((n, t), e1, e2) -> Let ((n, t), g e1, g e2)
  | Syntax.LetRec (p1, l2, e3, e4) -> LetRec (p1, l2, g e3, g e4)
  | Syntax.Application (Syntax.Variable (name), args) ->
    let rec gapp tlist slist = match tlist with
        [] -> Application (name, List.rev slist)
      | tfirst :: trest ->
        let v = Gensym.f "app" in
        Let ((v, Type.gen_type ()), g tfirst,
             gapp trest (v :: slist))
    in gapp args []
  | Syntax.Application (name, args) ->
    raise NotSupported


(* Knormal.f: k-正規形変換プログラムの入口 *)

let f program = g program
    
