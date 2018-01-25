(* 末尾呼び出し最適化 tail call optimization *)

let rec gcd2 m n =
  if n = 0 then m
  else let amari = m mod n in
    gcd2 n amari
      
(* C では *)
let rec gcd2C m n =
  let m = ref m in
  let n = ref n in
  let amari = ref 0 in
  while !n > 0 do
    amari := (!m) mod (!n);
    m := !n;
    n := !amari;
  done;
  !m

(* call f : 次の番地をスタックにつむ
   f に飛ぶ *)

(* ret : 帰り番地をスタックから pop してそこに飛ぶ *)

(* 末尾呼び出しでない関数 *)
let rec fac n =
  if n = 0 then 1 else n * fac (n - 1)

(* 末尾呼び出しにされた関数 *)
let rec fac' n acc =
  if n = 0 then acc
  else fac' (n - 1) (acc * n)
      
