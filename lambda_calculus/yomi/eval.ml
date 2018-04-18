open Env
open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Syntax.t *)
let rec beta_reduction expr env we = match expr with
    Var x -> x
  | App (t1, t2) -> 
