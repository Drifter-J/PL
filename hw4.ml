(* fold function for lists *)
let rec foldl op result lst =
  match lst with
   [] -> result
  |hd::tl -> foldl op (op result hd) tl

let rec foldr op initial lst = 
  match lst with
   [] -> initial
  |hd::tl -> op hd (foldr op initial tl)

(* membership checking for lists *)
let mem a l = List.mem a l

(* true if for pred holds for all element in l,
   false otherwise *)
let for_all pred l = List.for_all pred l

(* 
 *  Problem 1 
 * *)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match a,b with
  | [],[] -> []
  | _,[] -> a
  | [],_ -> b
  | hd::tl, hd'::tl' -> hd::hd'::zipper (tl,tl')

let z1 = zipper ([1;3;5], [2;4;6]) 
let z2 = zipper ([1;3], [2;4;6;8])
let z3 = zipper ([1;3;5;7], [2;4])
let z4 = zipper ([1;2;3], [4])

(* 
 *  Problem 2 
 * *)
type formula = 
    True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
  
and exp = 
    Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
=fun f ->
  match f with
  | True -> true
  | False -> false
  | Not f -> not (eval f)
  | AndAlso (f1,f2) -> (eval f1) && (eval f2)
  | OrElse (f1,f2) -> (eval f1) || (eval f2)
  | Imply (f1,f2) -> not (eval f1) || (eval f2)
  | Equal (e1,e2) -> (value e1) = (value e2)

and value : exp -> int
=fun e ->
  match e with
  | Num n -> n
  | Plus (e1,e2) -> (value e1) + (value e2)
  | Minus (e1,e2) -> (value e1) - (value e2)

let f1 = eval (Imply (Imply (True,False), True)) (* (True => False) => True *)
let f2 = eval (Equal (Num 1, Plus (Num 0, Num 1))) (* 1 = (0 + 1) *)

(* 
 *  Problem 3 
 * *)
type aexp = 
  | Const of int
  | Var of string 
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
  match aexp with
  | Const _ -> Const 0
  | Var y -> if x = y then Const 1 else Const 0
  | Power (y,n) -> 
      if not (x = y) then Const 0
      else Times [Const n; Power (y, n-1)]
  | Times [] -> Const 0
  | Times (hd::tl) -> Sum [Times (diff (hd,x)::tl); Times [hd; diff(Times tl, x)]]
  | Sum aexps -> Sum (List.map (fun ae -> diff (ae,x)) aexps)

(* x^2 + 2x + 1 *)
let ae1 = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]
let _ = diff (ae1, "x")


(*
 *  Problem 4: "Optimization"
 * *)
type cmd = 
  | Skip
  | Assign of string * exp
  | Seq of cmd * cmd
  | If of exp * cmd * cmd
  | While of exp * cmd

and exp =
  | N of int
  | Var of string
  | Plus of exp * exp 
  | Mult of exp * exp 

let rec optimize : cmd -> cmd
=fun cmd ->
  match cmd with
  | Skip -> Skip
  | Assign (x,e) -> Assign (x, reduce e)
  | Seq (c1,c2) -> Seq (optimize c1, optimize c2)
  | If (e,c1,c2) -> If (reduce e, optimize c1, optimize c2)
  | While (e,c) -> While (reduce e, optimize c)

and reduce : exp -> exp
=fun e ->
  if is_cexp e then N (compute_exp e)
  else 
    match e with
    | N n -> N n
    | Var x -> Var x
    | Plus (e1,e2) -> Plus (reduce e1, reduce e2)
    | Mult (e1,e2) -> Mult (reduce e1, reduce e2)

and is_cexp e = 
  match e with
  | N _ -> true
  | Var _ -> false
  | Plus (e1,e2) -> is_cexp e1 && is_cexp e2
  | Mult (e1,e2) -> is_cexp e1 && is_cexp e2

and compute_exp e = 
  match e with
  | N n -> n
  | Var x -> raise (Failure "Never happen")
  | Plus (e1,e2) -> compute_exp e1 + compute_exp e2
  | Mult (e1,e2) -> compute_exp e1 * compute_exp e2

let p1 = Assign ("x", Mult (N 2, Plus (Var "x", Plus (N 1, N 2))))
let p2 = Seq (Assign ("i", Plus (N 1, N 2)),
              While (Plus (Var "x", Mult (N 2, N 3)), 
                     Seq (Assign ("i", Plus (Var "i", N 1)),
                          If (Mult (Var "i", N 10), 
                              Skip, 
                              Skip)
                        )))

(*
 *  Problem 5: reachability 
 * *)
type graph = (vertex * vertex) list
and vertex = int

let g1 = [ (1,2); (1,3); (2,4); (3,4) ]
let g2 = [ (1,2); (2,3); (3,4); (4,2); (2,5) ]

let succ : graph -> vertex -> vertex list
=fun g v -> foldr (fun (s,d) succs -> if v = s then d::succs else succs) [] g

let apply : graph -> vertex list -> vertex list
=fun g vs -> foldr (fun v l -> (succ g v) @ l) [] vs

let equal : vertex list -> vertex list -> bool
=fun s1 s2 -> for_all (fun v1 -> mem v1 s2) s1 && for_all (fun v2 -> mem v2 s1) s2

let remove_dups : vertex list -> vertex list
=fun vs -> foldr (fun v l -> if mem v l then l else v::l) [] vs

let reach : graph * vertex -> vertex list
=fun (g,v) ->
  let rec iter reaches =
    let next = reaches @ (apply g reaches) in
      if equal reaches next then reaches
      else iter next 
  in List.sort (fun x y -> x - y) (remove_dups (iter [v]))
 
let r1 = reach (g1, 1)
let r2 = reach (g2, 3)
