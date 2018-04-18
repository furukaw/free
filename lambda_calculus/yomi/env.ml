(* キーが 'a 型、値が 'b 型の環境を表す型 *)
type ('a, 'b) t = ('a * 'b) list

(* 空の環境 *)
(* empty : ('a, 'b) *)
let empty = []

(* 環境 env の中で変数 var の値を返す *)
(* get : ('a, 'b) t -> 'a -> 'b *)
(* 変数 var が見つからなかったら例外 Not_found を起こす *)
let rec get env var =
  match env with
    [] -> raise Not_found
  | (key, value) :: rest ->
    if var = key then value else get rest var

(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
(* extend : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
let extend env var value =
  (var, value) :: env
  
