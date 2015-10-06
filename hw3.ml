(*1.*)
type pgm = cmd
and cmd = Skip
| Assign of string * exp
| Composite of cmd * cmd
| IF of exp * cmd * cmd
| While of exp * cmd
| For of cmd * exp * cmd
| Read of string
| Write of exp
and exp = Int of int 
| True
| False
| Var of string
| Plus of exp * exp
| Minus of exp * exp 
| Mul of exp * exp
| Div of exp * exp
| Not of exp
| Eq of exp * exp
| Lesst of exp * exp
| Neg of exp

(*2.*)
Read ("n");;
For (Assign ("i", Int 1), Var "n", Assign ("r", Mul(Var "r", Var "i")));;
Write (Var "r");;
