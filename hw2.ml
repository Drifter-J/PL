(* library functions *)
let rec map proc a = 
  match a with
   [] -> []
  |hd::tl -> (proc hd)::(map proc tl)

let rec foldr op initial lst =
  match lst with
 |[] -> initial
 |hd::tl -> op hd (foldr op initial tl)

let rec foldl op result lst =
  match lst with
   [] -> result
  |hd::tl -> foldl op (op result hd) tl

let rec enumerate_interval a b = 
  if a > b then []
  else a::(enumerate_interval (a+1) b)

let hd l = match l with [] -> raise (Failure "error") | hd::tl -> hd

let mult x y = x * y
let plus x y = x + y

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (row,col) ->
  if col = 0 || col = row then 1
  else pascal (row - 1, col - 1) + pascal (row - 1, col)

(* Problem 2 *)
let rec product term a b =
  if a > b then 1
  else (term a) * product term (a+1) b

(* cf., product and sum can be simply defined in terms of map and fold *)
let product term a b = foldr mult 1 (map term (enumerate_interval a b))
let sigma   term a b = foldr plus 0 (map term (enumerate_interval a b))

(* Problem 3 *)
let sigma_tr term a next b = 
  let rec iter a result = 
    if a > b then result
    else iter (next a) (term a + result)
  in iter a 0

(* Problem 4 *)
let rec reverse l = 
  match l with
  | [] -> []
  | hd::tl -> (reverse tl) @ [hd]

(* Problem 5 *)
let max lst = foldr (fun x y -> if x > y then x else y) (hd lst) lst
let min lst = foldr (fun x y -> if x > y then y else x) (hd lst) lst

let rec max lst = 
  match lst with
  | [] -> raise (Failure "error")
  | [hd] -> hd
  | hd::tl -> if hd > (max tl) then hd else (max tl)
