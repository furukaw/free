(* Syntax.t : プログラムを表す型 *)
type t = Var of string
       | App of t * t
       | Abs of string * t

(* 文字列を括弧で囲む関数 *)
let pq s = "(" ^ s ^")"

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Var (x) -> x
  | App (t1, t2) ->
    let s1 = match t1 with
        Abs _ -> pq (to_string t1)
      | _ -> to_string t1 in
    let s2 = match t2 with
        Var x -> x
      | _ -> pq (to_string t2) in
    s1 ^ " " ^ s2
  | Abs (x, t) ->
    let rec abss_string e = match e with
        Var (x) -> "." ^ x
      | App (t1, t2) ->
        "." ^ to_string e
      | Abs (z, ttt) ->
        z ^ abss_string ttt
    in
    "λ" ^ x ^ abss_string t

(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
