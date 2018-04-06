open Language

module MonotoneFramework = 
  struct
    
    (* Language statement annotated with analyse result *)
    type 'a t =
        | ARead   of string * ('a * 'a)
        | AWrite  of Expr.t * ('a * 'a)
        | AAssign of string * Expr.t * ('a * 'a)
        | ASeq    of 'a t * 'a t * ('a * 'a)
        | ASkip   of ('a * 'a)
        | AIf     of Expr.t * 'a t * 'a t * ('a * 'a)
        | AWhile  of Expr.t * 'a t * ('a * 'a)
        | ARepeat of 'a t * Expr.t * ('a * 'a)
        | ACall   of string * Expr.t list * ('a * 'a)
    
    let rec annotate (prg : Stmt.t) (init : 'a) : 'a t = match prg with
        | Stmt.Read    x          -> ARead (x, (init, init))
        | Stmt.Write   e          -> AWrite (e, (init, init))
        | Stmt.Assign (x, e)      -> AAssign (x, e, (init, init))
        | Stmt.Seq    (s1, s2)    -> ASeq (annotate s1 init, annotate s2 init, (init, init))
        | Stmt.Skip               -> ASkip (init, init)
        | Stmt.If     (e, s1, s2) -> AIf (e, annotate s1 init, annotate s2 init, (init, init))
        | Stmt.While  (e, s)      -> AWhile (e, annotate s init, (init, init))
        | Stmt.Repeat (s, e)      -> ARepeat (annotate s init, e, (init, init))
        | Stmt.Call   (f, params) -> ACall (f, params, (init, init))
    
    let rec print_result elem_printer = function
        | ARead   (_, (a1, a2))       -> print_string @@ "ARead "   ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | AWrite  (_, (a1, a2))       -> print_string @@ "AWrite "  ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | AAssign (_, _, (a1, a2))    -> print_string @@ "AAssign " ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | ASeq    (l, r, (a1, a2))    -> print_result elem_printer l;
                                         print_result elem_printer r
        | ASkip   (a1, a2)            -> print_string @@ "ASkip "   ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | AIf     (_, l, r, (a1, a2)) -> print_string "// if\n";
                                         print_result elem_printer l;
                                         print_string "// else\n";
                                         print_result elem_printer r;
                                         print_string @@ "// if-end " ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | AWhile  (_, b, (a1, a2))    -> print_string "// while\n";
                                         print_result elem_printer b;
                                         print_string @@ "// while-end " ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | ARepeat (b, _, (a1, a2))    -> print_string "// repeat\n";
                                         print_result elem_printer b;
                                         print_string @@ "// repeat-end " ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"
        | ACall   (_, _, (a1, a2))    -> print_string @@ "ACall "   ^ elem_printer a1 ^ " -> " ^ elem_printer a2; print_string "\n"

    type 'a monotone_framework = {
        init : 'a;                    (* Initial analysis for entry (or exit) point *)
        zero : 'a;                    (* Identity with respect to `combine`: forall a. combine a zero = combine zero a = a *)
        combine : 'a -> 'a -> 'a;     (* Combine function (usually set union or intersection) *)
        transfer : 'a t -> 'a -> 'a;  (* Transfer function (usually `transfer prg a = gen prg (kill prg a)` *)
        change : 'a -> 'a -> bool     (* Inequality predicate, `change a b == true` means that `a is not equal to b` *)
    }

    let rec forward_analyse' (aprg : 'a t) (input : 'a) (combine : 'a -> 'a -> 'a) (transfer : 'a t -> 'a -> 'a) (change : 'a -> 'a -> bool) : 'a * 'a t * bool =
        let exit_analysis = transfer aprg input in
        match aprg with
        | ARead   (x, (_, a))         -> (exit_analysis, ARead (x, (input, exit_analysis)), change a exit_analysis)
        | AWrite  (e, (_, a))         -> (exit_analysis, AWrite (e, (input, exit_analysis)), change a exit_analysis)
        | AAssign (x, e, (_, a))      -> (exit_analysis, AAssign (x, e, (input, exit_analysis)), change a exit_analysis)
        | ASkip (_, a)                -> (exit_analysis, ASkip (input, exit_analysis), change a exit_analysis)
        | ACall   (f, params, (_, a)) -> (exit_analysis, ACall (f, params, (input, exit_analysis)), change a exit_analysis)
        | ASeq    (s1, s2, (_, a))    -> let (exit1, as1, change1) = forward_analyse' s1 input combine transfer change in
                                         let (exit2, as2, change2) = forward_analyse' s2 exit1 combine transfer change in
                                         (exit2, ASeq (as1, as2, (input, exit2)), change1 || change2 || change a exit2)
        | AIf     (e, s1, s2, (_, a)) -> let (exit1, as1, change1) = forward_analyse' s1 input combine transfer change in
                                         let (exit2, as2, change2) = forward_analyse' s2 input combine transfer change in
                                         let exit = combine exit1 exit2 in
                                         (exit, AIf (e, as1, as2, (input, exit)), change1 || change2 || change a exit)
        | AWhile  (e, s, (_, a))      -> let global_change = ref true in
                                         let annotated_body = ref s in
                                         let current_input = ref input in
                                         let ever_change = ref false in
                                         let last_exit = ref input in
                                         while !global_change do
                                             let (exit, new_body, changed) = forward_analyse' !annotated_body !current_input combine transfer change in
                                             current_input := combine input exit;
                                             annotated_body := new_body;
                                             global_change := changed;
                                             if changed then ever_change := true;
                                             last_exit := exit
                                         done;
                                         let exit = combine !last_exit input in
                                         (exit, AWhile (e, !annotated_body, (!current_input, exit)), !ever_change)
        | ARepeat (s, e, (_, a))      -> failwith "Not supported: Repeat"

    let forward_analyse (prg : Stmt.t) (framework : 'a monotone_framework) : 'a t =
            let (_, tree, _) = forward_analyse' (annotate prg framework.zero) framework.init framework.combine framework.transfer framework.change in tree

  end
  
module ConstantPropagation =
  struct
    module M = MonotoneFramework
  
    let rec const_value (list (*: [string * int option]*)) (e : Expr.t) : int option = match e with
        | Expr.Const z            -> Some z
        | Expr.Var x              -> (match List.fold_left (
                                         fun a (y, v) -> if x <> y then a else match a with
                                             | [] -> [v]
                                             | [Some z] -> (match v with
                                                 | Some t -> if z = t then [Some z] else [None]
                                                 | None -> [None]) 
                                             | [None] -> [None]
                                     ) [] list with
                                         | [v] -> v
                                         | [] -> None)
        | Expr.Binop (op, e1, e2) -> let c1 = const_value list e1 in
                                     let c2 = const_value list e2 in
                                     match (c1, c2) with
                                         | (Some x, Some y) -> Some (Expr.to_func op x y)
                                         | _ -> None
    
    let rec unique = function
        | []      -> []
        | x::rest -> x :: unique (List.filter (fun y -> x <> y) rest)
    
    let (@@@) (list1 : 'a list) (list2 : 'a list) : 'a list = unique @@ list1 @ list2

    let constant_propagation (prg : Stmt.t) =
        let kill s list = match s with
            | M.AAssign (x, _, _) -> List.filter (fun (y, _) -> x <> y) list
            | _ -> list
        in
        let gen s list = match s with
            | M.AAssign (x, e, _) -> (x, const_value list e) :: list
            | _ -> list
        in
        let (<*>) f g = fun a b -> g a (f a b) in
        MonotoneFramework.forward_analyse prg {
            init = [];
            zero = [];
            combine = (@@@);
            transfer = kill <*> gen;
            change = (<>)
        }

    let fold_const state e =
        match const_value state e with
            | Some z -> Expr.Const z
            | None   -> e

    let propagate_constants (prg : Stmt.t) : Stmt.t =
        let analyse_result = constant_propagation prg in
        let rec transform = function
            | M.ARead   (x, (input, _))         -> Stmt.Read x
            | M.AWrite  (e, (input, _))         -> let value = fold_const input e in
                                                   Stmt.Write value
            | M.AAssign (x, e, (input, _))      -> let value = fold_const input e in
                                                   Stmt.Assign (x, value)
            | M.ASkip (input, _)                -> Stmt.Skip
            | M.ACall   (f, params, (input, _)) -> let value_params = List.map (fold_const input) params in
                                                   Stmt.Call (f, value_params)
            | M.ASeq    (s1, s2, (input, _))    -> let ss1 = transform s1 in
                                                   let ss2 = transform s2 in
                                                   Stmt.Seq (ss1, ss2)
            | M.AIf     (e, s1, s2, (input, _)) -> let ss1 = transform s1 in
                                                   let ss2 = transform s2 in
                                                   let value = fold_const input e in
                                                   Stmt.If (value, ss1, ss2)
            | M.AWhile  (e, s, (input, _))      -> let ss = transform s in
                                                   let value = fold_const input e in
                                                   Stmt.While (value, ss)
            | M.ARepeat (s, e, (input, _))      -> failwith "Not supported: Repeat"
        in
        transform analyse_result
  
  end
  
module EliminateUselessStatements = 
  struct
  
    let rec eliminate_useless_statements' (prg : Stmt.t) : bool * Stmt.t = match prg with
        | Stmt.Seq    (s1, s2)    -> let (cb1, ss1) = eliminate_useless_statements' s1 in
                                     let (cb2, ss2) = eliminate_useless_statements' s2 in
                                     if cb1 then (cb1, ss1) else
                                     (match (ss1, ss2) with
                                        | _, Stmt.Skip -> (cb1, ss1)
                                        | Stmt.Skip, _ -> (cb2, ss2)
                                        | _            -> (cb1 || cb2, Stmt.Seq (ss1, ss2)))
        | Stmt.If     (e, s1, s2) -> let (cb1, ss1) = eliminate_useless_statements' s1 in
                                     let (cb2, ss2) = eliminate_useless_statements' s2 in
                                     (match e with
                                        | Expr.Const 0 -> (cb2, ss2)
                                        | Expr.Const _ -> (cb1, ss1)
                                        | _            -> (cb1 && cb2, Stmt.If (e, s1, s2)))
        | Stmt.While  (e, s)      -> let (cb, ss) = eliminate_useless_statements' s in
                                     if cb then eliminate_useless_statements' (Stmt.If (e, ss, Stmt.Skip)) else
                                     (match e with
                                        | Expr.Const 0 -> (false, Stmt.Skip)
                                        | Expr.Const _ -> (true, Stmt.While (e, ss))
                                        | _            -> (false, Stmt.While (e, ss)))
        | Stmt.Repeat (s, e)      -> failwith "Not supported: Repeat"
        | _                       -> (false, prg)

    let eliminate_useless_statements (prg : Stmt.t) : Stmt.t =
        let (_, result) = eliminate_useless_statements' prg in result
        
  end
  
module Optimizations =
  struct
  
    let optimize (prg : Stmt.t) : Stmt.t =
        let change = ref true in
        let old_prg = ref prg in
        while !change do
            let new_prg = EliminateUselessStatements.eliminate_useless_statements @@ ConstantPropagation.propagate_constants !old_prg in
            change := new_prg <> !old_prg;
            old_prg := new_prg
        done;
        !old_prg
        
  end