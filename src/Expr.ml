(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

(* Available binary operators:
    !!                   --- disjunction
    &&                   --- conjunction
    ==, !=, <=, <, >=, > --- comparisons
    +, -                 --- addition, subtraction
    *, /, %              --- multiplication, division, reminder
*)

(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x 
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

let (@$) f g = fun x y -> f @@ g x y;;

let (@^) f g = fun x y -> f (g x) (g y);;

let b2i b = if b then 1 else 0;;

let i2b x = x <> 0;;

(* Expression evaluator

     val eval : state -> expr -> int
 
   Takes a state and an expression, and returns the value of the expression in 
   the given state.
*)
let rec eval state expr = match expr with
  | Const x -> x
  | Var z   -> state z
  | Binop (op, left, right) -> 
    let leftR = eval state left in
    let rightR = eval state right in
    let opR = match op with
      | "+"  -> ( + )
      | "-"  -> ( - )
      | "*"  -> ( * )
      | "/"  -> ( / )
      | "%"  -> (mod)
      | "<"  -> b2i @$ (<)
      | ">"  -> b2i @$ (>)
      | "<=" -> b2i @$ (<=)
      | ">=" -> b2i @$ (>=)
      | "==" -> b2i @$ (=)
      | "!=" -> b2i @$ (<>)
      | "&&" -> b2i @$ (&&) @^ i2b
      | "!!" -> b2i @$ (||) @^ i2b
      | _ -> failwith @@ Printf.sprintf "Unknown operator %s" op in
      opR leftR rightR
;;