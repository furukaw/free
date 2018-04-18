(* λ式の型 *)
type term = Var of string
| App of term * term
| Abs of string * string list * term
| Par of term

(* 置換の型 *)
type subst_t = term * string * term

(* 等しくないときに使うエラー *)
exception Not_equal of term * term

(* やりすぎな時に使うエラー *)
exception Limited

(* 項を形成する関数たち *)
(* var : string -> term *)
let var x = Var x
(* app : term -> term -> term *)
let app m n = Par (App (m, n))
(* abs : string -> term -> term *)
let abs x m = Par (Abs (x, [], m))
(* par : term -> term *)
let par t = Par t

(* 項を右の外側から削る関数 *)
(* reducer : term -> int -> term *)
let rec reducer t k = if k < 1 then t else
match t with
  Var x -> Var x
| App (m, n) -> reducer m (k-1)
| Abs (x, [], m) -> reducer m (k-1)
| Abs (x, f :: r, m) -> reducer (Abs (f, r, m)) (k-1)
| Par (t') -> reducer t' (k)

(* 項を左の外側から削る関数 *)
(* reducel : term -> int -> term *)
let rec reducel t k = if k < 1 then t else
match t with
Var x -> Var x
| App (m, n) -> reducer n (k-1)
| Abs (x, [], m) -> reducel m (k-1)
| Abs (x, f :: r, m) -> reducel (Abs (f, r, m)) (k-1)
| Par (t') -> reducer t' (k)

(* 項を文字列にする関数 *)
(* output : term -> string *)
let rec output t =
(* output_binding : string list -> string *)
  let rec output_binding lst = match lst with
[] -> ". "
| first :: rest -> " " ^ first ^ output_binding rest
in
match t with
Var x -> x
| App (m, n) -> output m ^ " " ^ output n
| Abs (x, l, m) -> "λ" ^ x ^ output_binding l ^ output m
| Par (t') -> "(" ^ output t' ^ ")"

(* 括弧を最低限にされたterm型の項の構造の等しさを返す *)
(* tequ : term -> term -> bool *)
let teq term1 term2 =
(* eq : term -> term -> unit *)
let rec eq t1 t2 = match (t1, t2) with
 (Var (x1), Var (x2)) -> if x1 = x2 then ()
else raise (Not_equal (Var (x1), Var(x2)))
| (App (m1, n1), App (m2, n2)) -> eq m1 m2; eq n1 n2
| (Abs (x1, l1, m1), Abs (x2, l2, m2)) -> if x1 = x2
then begin
match (l1, l2) with ([], []) -> eq m1 m2
| (f1 :: r1, f2 :: r2) -> eq (Abs (f1, r1, m1)) (Abs (f2, r2, m2))
| _ -> raise (Not_equal (t1, t2)) end
else raise (Not_equal (t1, t2))
| (Par (t1'), Par(t2')) -> eq t1' t2'
| (_, _) -> raise (Not_equal (t1, t2))

in try (eq term1 term2; true) with Not_equal (t1, t2) -> 
print_string(output t1 ^ "\n\n" ^ output t2); false

(* リストで表された集合に要素を足した集合を表すリストを返す *)
(* set_add : 'a list -> 'a -> 'a list *)
let rec set_add lst n = match lst with
[] -> [n]
| first :: rest -> if first = n then lst else first :: (set_add rest n)

(* リストで表された集合二つの和集合を表すリストを返す *)
(* set_union : 'a list -> 'a list -> 'a list *)
let rec set_union l1 l2 = match l1 with
[] -> l2
| first :: rest -> set_add (set_union rest l2) first

(* リストで表された重複のない集合から要素を引いた集合を表すリストを返す *)
(* set_off : 'a list -> 'a -> 'a list *)
let rec set_off lst n = match lst with
[] -> []
| first :: rest -> if first = n then rest else first :: (set_off rest n)

(* リストと要素一つを受け取って、リストに要素が含まれているかを返す *)
(* set_has : 'a list -> 'a -> bool *)
let rec set_has lst n = match lst with
[] -> false
| first :: rest -> if first = n then true else set_has rest n

(* λ式を受け取って部分項の集合を返す *)
(* sub_terms : term -> term list *)
let rec sub_terms term =
(* sub : term -> string list -> term list *)
let sub t l = match t with
Var x -> t :: l
| App (m, n) -> t :: (sub_terms m) @ (sub_terms n) @ l
| Abs (x, [], m) -> t :: (sub_terms m) @ l
| Abs (x, f :: r, m) -> t :: (sub_terms (Abs (f, r, m))) @ l
| Par (t') -> (sub_terms t') @ l

in sub term []

(* λ式を受け取って自由変数の集合を返す *)
(* fv : term -> term list *)
let rec fv t = match t with
Var x -> [x]
| App (m, n) -> set_union (fv m) (fv n)
| Abs (x, [], m) -> set_off (fv m) x
| Abs (x, f :: r, m) -> set_off (fv (Abs (f, r, m))) x
| Par t' -> fv t'

(* λ式を受け取って束縛変数の集合を返す *)
(* bv : term -> term list *)
let rec bv t = match t with
Var x -> []
| App (m, n) -> set_union (bv m) (bv n)
| Abs (x, [], m) -> set_add (bv m) x
| Abs (x, f :: r, m) -> set_add (bv (Abs (f, r, m))) x
| Par t' -> bv t'

(* リストの式を全部文字列にする *)
(* output_list : term list -> string *)
let rec output_list lst = match lst with
[] -> ""
| f :: r -> output f ^ "\n" ^ output_list r

(* 同じ場所に括弧が無駄に重なっているのを一つにする *)
(* depar : term -> term *)
let rec depar t = match t with
Var x -> t
| App (m, n) -> App (depar m, depar n)
| Abs (m, l, n) -> Abs (m, l, depar n)
| Par (t') -> begin match t' with Par (t'') -> depar t'
| _ -> Par (depar t')
end

(* 式の一番外側の括弧をはずす *)
(* shell : term -> term *)
let shell t = match t with
Par (t') -> t'
| _ -> t

(* 式の一番右の関数適用に付いている、無くてもいい括弧をはずす *)
(* fumei : term -> term *)
let rec fumei t = match t with
Var x -> t
| App (m, n) -> begin match n with
Par (Abs (x, l, nr)) -> App (m, Abs (x, l, fumei nr))
| _ -> App (m, fumei n)
end
| Abs (x, [], m) -> Abs (x, [],fumei m)
| Abs (x, f :: r, m) -> Abs (x, [], Abs (f, r, fumei m))
| Par (t') -> Par (fumei t')

(* 無くてもいい括弧をはずす *)
(* readable : term -> term *)
let readable ter =
let rec rea t = match t with
Var x -> t
| App (mn, l) -> begin
match mn with Par (App (m, n)) -> App (rea (App (m, n)), rea l)
  | _ -> App (rea mn, rea l) end
| Abs (x, l, mn) -> begin
match mn with
Par (App (m, n)) -> Abs (x, l, rea (App (m, n)))
| Par (Abs (y, l', m)) -> rea (Abs (x, l @ (y :: l'), m))
| Abs (y, l', m) -> rea (Abs (x, l @ (y :: l'), m))
| _ -> Abs (x, l, rea mn)
end
| Par (t') -> Par (rea t')
in
let t = depar ter in
let t = shell t in
let t = fumei t in
rea t

(* 名前変更 *)
(* rename : term -> string -> string -> term *)
let rec rename t x y = match t with
Var v -> if v = x then Var y else Var v
| App (m, n) -> App (rename m x y, rename n x y)
| Abs (v, l, m) -> let newv = if v = x then y else v in
(match l with [] -> Abs (newv, [], rename m x y)
       | f :: r -> Abs (newv, [], rename (Abs (f, r, m)) x y)
)
| Par t' -> Par (rename t' x y)

(* term型の項の構造の等しさを返す *)
(* tequal : term -> term -> bool *)
let tequal t1 t2 =
let r1 = readable t1 in
let r2 = readable t2 in
teq r1 r2

(* 文字列と整数のリストと文字列を受け取って、文字列がリストにあれば対応する整数を返す *)
(* search_list : (string, int) list -> string -> int *)
(* 無ければ0を返す *)
let rec search_list lst x = match lst with
[] -> 0
| (name, num) :: rest -> if name = x then num else search_list rest x

(* 変数名が固有の整数であるα同値なλ式に変換する *)
(* number : term -> term *)
let number t =
(* al : term -> int -> (string, int) list -> alpha_t *)
let rec al t k lst = match t with
Var x -> let n = search_list lst x in
let n = if n = 0 then k else n in
Var (string_of_int n)
| App (m, n) -> App (al m (k) lst, al n (k) lst)
| Abs (x, [], m) -> Abs (string_of_int k, [], al m (k+1) ((x, k) :: lst))
| Abs (x, f :: r, m) -> Abs (string_of_int k, [], al (Abs (f, r, m)) (k+1) ((x, k) :: lst))
| Par (t') -> Par (al t' k lst)

in al t 1 []

(* α同値 *)
(* alpha : term -> term -> bool *)
let alpha t1 t2 =
let nt1 = number t1 in
let nt2 = number t2 in
let st1 = readable nt1 in
let st2 = readable nt2 in
tequal st1 st2

(* 項への代入（置換）を生成 *)
(* subst : term -> string -> term -> subst *)
let subst t x m = (t, x, m)

(* 置換を文字列にする *)
(* output_sub : subst -> string *)
let output_sub s = match s with (t, x, m) ->
"(" ^ output t ^ ")[" ^ x ^ " := " ^ output m ^ "]"

(* 置換操作において、新しい変数名をつける *)
(* newvar : term -> string -> string *)
let newvar t x =
(* newv : term -> string -> int -> string *)
let rec newv t x n = if n > 100000000 then raise Limited else begin
let newstr = x ^ string_of_int n in
if set_has t newstr then newv t x (n+1) else newstr
end
in newv (fv t) x 1

(* 置換を実行 *)
(* elaborate : subst -> term *)
let elaborate (t, x, m) =
(* ela : subst -> term *)
let rec ela (t, x, m) = begin
match t with
Var v -> if v = x then (print_string ("wawawa"); m) else t
| App (p, q) -> App (ela (p, x, m), ela (q, x, m))
| Abs (y, l, p) -> let newp = begin
match l with [] -> p
| f :: r -> Abs (f, r, p)
end in
let newy = if (set_has (fv m) y) || (set_has (fv p) x) then
newvar newp y else y in
Abs (newy, [], ela (rename newp y newy, x, m))
| Par (t') -> Par (ela (t', x, m))
end in
try (ela (t, x, m)) with Limited -> print_string ("Error: too much variables");
t

(* テスト用の式（章末問題1.1より） *)
let x = var "x"
let y = var "y"
let z = var "z"

let t1 = abs "x" (app (app (app x z) y)
		      (app x x))
let t1 = readable t1
let t2 = app (
abs "x" (abs "y" (abs "z" (app z (app (app x y) z))))
) (abs "u" (var "u"))
let t2 = readable t2

let ex1a = 
