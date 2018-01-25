open First

let gensymR count = (count := !count + 1;
                   "_R_" ^ string_of_int (!count))

(* １階の言語の式にレジスタ割り当ての前処理をする *)
(* pe : First.t -> First.t *)
let rec pe program = match program with
    IfEqual (s1, s2, t3, t4) -> IfEqual (s1, s2, pe t3, pe t4)
  | IfLess (s1, s2, t3, t4) -> IfLess (s1, s2, pe t3, pe t4)
  | Let (v1, t2, t3) -> Let (v1, pe t2, pe t3)
  | Application (f, slst) ->
    let counter = ref 0 in
    let rec fapp slist rlist = match slist with
        [] -> Application (f, rlist)
      | sfirst :: srest ->
        let v = gensymR counter in
        Let ((v, Type.gen_type ()), Variable sfirst,
             fapp srest (v :: rlist))
    in fapp slst []
  | _ -> program

(* １階の言語の変数定義文にレジスタ割り当ての前処理をする *)
(* pd : First.def_t -> First.def_t *)
let pd hikisu = match hikisu with
    FunDef (f, vlst, e) ->
    let counter = ref 0 in
    (* loop : (string * Type.t) list -> (string * Type.t) list * First.t *)
    let rec loop vlist = match vlist with
        [] -> ([], pe e)
      | (vn, vt) as first :: rest ->
        let rn = gensymR counter in
        let (args, lest) = loop rest in
        ((rn, vt) :: args, Let (first, Variable rn, lest))
    in let (args, lets) = loop vlst in
    FunDef (f, args, lets)

(* １階の言語のプログラムにレジスタ割り当ての前処理をする *)
(* Prealloc.f : First.prog_t -> First.prog_t *)
let f programs = match programs with
    Program (deflist, e) ->
    Program (List.map pd deflist, pe e)
