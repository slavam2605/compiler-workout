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

    let rec transform (aprg : 'a t) (transformer : 'a t -> Stmt.t option) : Stmt.t =
        match transformer aprg with | Some r -> r | None -> match aprg with
            | ARead   (x, _)         -> Stmt.Read x
            | AWrite  (e, _)         -> Stmt.Write e
            | AAssign (x, e, _)      -> Stmt.Assign (x, e)
            | ASeq    (s1, s2, _)    -> Stmt.Seq (transform s1 transformer, transform s2 transformer)
            | ASkip   _              -> Stmt.Skip
            | AIf     (e, s1, s2, _) -> Stmt.If (e, transform s1 transformer, transform s2 transformer)
            | AWhile  (e, s, _)      -> Stmt.While (e, transform s transformer)
            | ARepeat (s, e, _)      -> Stmt.Repeat (transform s transformer, e)
            | ACall   (f, params, _) -> Stmt.Call (f, params)
    
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
        init : 'a;                                (* Initial analysis for entry (or exit) point *)
        zero : 'a;                                (* Identity with respect to `combine`: forall a. combine a zero = combine zero a = a *)
        combine : 'a -> 'a -> 'a;                 (* Combine function (usually set union or intersection) *)
        transfer : 'a t -> 'a -> 'a;              (* Transfer function (usually `transfer prg a = gen prg (kill prg a)` *)
        cond_transfer : Expr.t -> 'a -> 'a * 'a;  (* Transfer function for conditions, returns (if_true, if_false) results *)
        change : 'a -> 'a -> bool                 (* Inequality predicate, `change a b == true` means that `a is not equal to b` *)
    }

    let rec forward_analyse' (aprg : 'a t) (input : 'a) (combine : 'a -> 'a -> 'a) (transfer : 'a t -> 'a -> 'a) (cond_transfer : Expr.t -> 'a -> 'a * 'a) (change : 'a -> 'a -> bool) : 'a * 'a t * bool =
        let exit_analysis = transfer aprg input in
        match aprg with
        | ARead   (x, (_, a))         -> (exit_analysis, ARead (x, (input, exit_analysis)), change a exit_analysis)
        | AWrite  (e, (_, a))         -> (exit_analysis, AWrite (e, (input, exit_analysis)), change a exit_analysis)
        | AAssign (x, e, (_, a))      -> (exit_analysis, AAssign (x, e, (input, exit_analysis)), change a exit_analysis)
        | ASkip (_, a)                -> (exit_analysis, ASkip (input, exit_analysis), change a exit_analysis)
        | ACall   (f, params, (_, a)) -> (exit_analysis, ACall (f, params, (input, exit_analysis)), change a exit_analysis)
        | ASeq    (s1, s2, (_, a))    -> let (exit1, as1, change1) = forward_analyse' s1 input combine transfer cond_transfer change in
                                         let (exit2, as2, change2) = forward_analyse' s2 exit1 combine transfer cond_transfer change in
                                         (exit2, ASeq (as1, as2, (input, exit2)), change1 || change2 || change a exit2)
        | AIf     (e, s1, s2, (_, a)) -> let (true_input, false_input) = cond_transfer e input in
                                         let (exit1, as1, change1) = forward_analyse' s1 true_input combine transfer cond_transfer change in
                                         let (exit2, as2, change2) = forward_analyse' s2 false_input combine transfer cond_transfer change in
                                         let exit = combine exit1 exit2 in
                                         (exit, AIf (e, as1, as2, (input, exit)), change1 || change2 || change a exit)
        | AWhile  (e, s, (_, a))      -> let (true_input, false_input) = cond_transfer e input in 
                                         let global_change = ref true in
                                         let ever_change = ref false in
                                         let current_body = ref s in
                                         let current_input = ref input in
                                         let current_cond_true = ref true_input in
                                         let current_cond_false = ref false_input in
                                         while !global_change do
                                             let (body_exit, new_body, changed) = forward_analyse' !current_body !current_cond_true combine transfer cond_transfer change in
                                             current_body := new_body;
                                             current_input := combine !current_input body_exit;
                                             let (new_cond_true, new_cond_false) = cond_transfer e !current_input in
                                             let changed = changed || change !current_cond_true new_cond_true || change !current_cond_false new_cond_false in
                                             current_cond_true := new_cond_true;
                                             current_cond_false := new_cond_false;
                                             global_change := changed;
                                             if changed then ever_change := true
                                         done;
                                         (!current_cond_false, AWhile (e, !current_body, (input, !current_cond_false)), !ever_change)
        | ARepeat (s, e, (_, a))      -> failwith "Not supported: Repeat"

    let forward_analyse (prg : Stmt.t) (framework : 'a monotone_framework) : 'a t =
        let (_, tree, _) = forward_analyse' (annotate prg framework.zero) framework.init framework.combine framework.transfer framework.cond_transfer framework.change in tree

    let rec backward_analyse' (aprg: 'a t) (output : 'a) (combine : 'a -> 'a -> 'a) (transfer : 'a t -> 'a -> 'a) (change : 'a -> 'a -> bool) : 'a * 'a t * bool =
        let input_analysis = transfer aprg output in
        match aprg with
        | ARead (x, (a, _))         -> (input_analysis, ARead (x, (input_analysis, output)), change a input_analysis)
        | AWrite (e, (a, _))        -> (input_analysis, AWrite (e, (input_analysis, output)), change a input_analysis)
        | AAssign (x, e, (a, _))    -> (input_analysis, AAssign (x, e, (input_analysis, output)), change a input_analysis)
        | ASkip (a, _)              -> (input_analysis, ASkip (input_analysis, output), change a input_analysis)
        | ACall (f, params, (a, _)) -> (input_analysis, ACall (f, params, (input_analysis, output)), change a input_analysis)
        | ASeq (s1, s2, (a, _))     -> let (input2, as2, change2) = backward_analyse' s2 output combine transfer change in
                                       let (input1, as1, change1) = backward_analyse' s1 input2 combine transfer change in
                                       (input1, ASeq (as1, as2, (input1, output)), change1 || change2 || change a input2)
        | AIf (e, s1, s2, (a, _))   -> let (input1, as1, change1) = backward_analyse' s1 output combine transfer change in
                                       let (input2, as2, change2) = backward_analyse' s2 output combine transfer change in
                                       let input = combine input1 input2 in
                                       let input_if = transfer aprg input in
                                       (input_if, AIf (e, as1, as2, (input_if, output)), change1 || change2 || change a input_if)
        | AWhile (e, s, (a, _))     -> let global_change = ref true in
                                       let annotated_body = ref s in
                                       let current_output = ref output in
                                       let ever_change = ref false in
                                       let last_input = ref output in
                                       while !global_change do
                                           let (input, new_body, changed) = backward_analyse' !annotated_body !current_output combine transfer change in
                                           current_output := combine output input;
                                           annotated_body := new_body;
                                           global_change := changed;
                                           if changed then ever_change := true;
                                           last_input := input
                                       done;
                                       let input = combine !last_input output in
                                       let input_while = transfer aprg input in
                                       (input_while, AWhile (e, !annotated_body, (input_while, !current_output)), !ever_change || change a input_while)
        | ARepeat (s, e, (a, _))    -> failwith "Not supported: Repeat"

    let backward_analyse (prg : Stmt.t) (framework : 'a monotone_framework) : 'a t =
        let (_, tree, _) = backward_analyse' (annotate prg framework.zero) framework.init framework.combine framework.transfer framework.change in tree

  end
  
module ConstantPropagation =
  struct
  
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
        let module M = MonotoneFramework in
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
            cond_transfer = (fun _ x -> x, x);
            change = (<>)
        }

    let fold_const state e =
        match const_value state e with
            | Some z -> Expr.Const z
            | None   -> e

    let propagate_constants (prg : Stmt.t) : Stmt.t =
        let module M = MonotoneFramework in
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

module LiveVariables =
  struct

    let rec unique = function
        | []      -> []
        | x::rest -> x :: unique (List.filter (fun y -> x <> y) rest)

    let (@@@) (list1 : 'a list) (list2 : 'a list) : 'a list = unique @@ list1 @ list2

    let rec used_variables' = function
        | Expr.Const _         -> []
        | Expr.Var x           -> [x]
        | Expr.Binop (_, l, r) -> used_variables' l @ used_variables' r

    let used_variables (e : Expr.t) = unique @@ used_variables' e

    let live_variables (prg : Stmt.t) =
        let module M = MonotoneFramework in
        let kill s list = match s with
            | M.AAssign (x, _, _) -> List.filter ((<>) x) list
            | _ -> list
        in
        let gen s list = match s with
            | M.AWrite (e, _)     -> used_variables e @@@ list
            | M.AAssign (_, e, _) -> used_variables e @@@ list
            | M.AIf (e, _, _, _)  -> used_variables e @@@ list
            | M.AWhile (e, _, _)  -> used_variables e @@@ list
            | M.ARepeat (_, e, _) -> used_variables e @@@ list
            | M.ACall (_, p, _)   -> List.concat (List.map used_variables p) @@@ list
            | _ -> list
        in
        let (<*>) f g = fun a b -> g a (f a b) in
        let change x y = (List.sort compare x) <> (List.sort compare y) in
        MonotoneFramework.backward_analyse prg {
            init = [];
            zero = [];
            combine = (@@@);
            transfer = kill <*> gen;
            cond_transfer = (fun _ x -> x, x);
            change = change
        }

     let eliminate_dead_variables (prg : Stmt.t) : Stmt.t =
         let module M = MonotoneFramework in
         let analyse_result = live_variables prg in
         let transformer = function
             | M.AAssign (x, e, (_, output)) -> Some (if List.mem x output then Stmt.Assign (x, e) else Skip)
             | _ -> None
         in
         M.transform analyse_result transformer

  end

module TrueExpressions =
  struct
  
    let (&&&) list1 list2 = List.filter (fun x -> List.mem x list2) list1
        
    let rec unique = function
        | []      -> []
        | x::rest -> x :: unique (List.filter (fun y -> x <> y) rest) 
        
    let (@@@) list1 list2 = unique @@ list1 @ list2
    
    let rec use_var x = function
        | Expr.Const _ -> false
        | Expr.Var y -> x = y
        | Expr.Binop (_, l, r) -> use_var x l || use_var x r
    
    let rec negate = function
        | Expr.Const 0 -> Expr.Const 1
        | Expr.Const _ -> Expr.Const 0
        | Expr.Var z -> Expr.Binop ("==", Expr.Var z, Expr.Const 0)
        | Expr.Binop (op, l, r) -> match op with
            | "+" | "-" | "*" | "/" | "%" -> Expr.Binop ("==", Expr.Binop (op, l, r), Expr.Const 0)
            | "<"  -> Expr.Binop (">=", l, r)
            | ">"  -> Expr.Binop ("<=", l, r)
            | "<=" -> Expr.Binop (">", l, r)
            | ">=" -> Expr.Binop ("<", l, r)
            | "==" -> Expr.Binop ("!=", l, r)
            | "!=" -> Expr.Binop ("==", l, r)
            | "&&" -> Expr.Binop ("!!", negate l, negate r)
            | "!!" -> Expr.Binop ("&&", negate l, negate r)
    
    let true_expressions (prg : Stmt.t) =
        let module M = MonotoneFramework in
        let (<*>) f g = fun a b -> g a (f a b) in
        let change x y = (List.sort compare x) <> (List.sort compare y) in
        let gen _ list = list in
        let kill s list = match s with 
            | M.AAssign (x, _, _) -> List.filter (fun e -> not @@ use_var x e) list 
            | _ -> list
        in
        let cond_transfer e list = (list @@@ [e], list @@@ [negate e]) in
        MonotoneFramework.forward_analyse prg {
            init = [];
            zero = [];
            combine = (&&&);
            transfer = kill <*> gen;
            cond_transfer = cond_transfer;
            change = change
        } 
  
  end

module Optimizations =
  struct
  
    let optimize (prg : Stmt.t) : Stmt.t =
        let change = ref true in
        let old_prg = ref prg in
        while !change do
            let new_prg =
                LiveVariables.eliminate_dead_variables @@
                EliminateUselessStatements.eliminate_useless_statements @@
                ConstantPropagation.propagate_constants !old_prg
            in
            change := new_prg <> !old_prg;
            old_prg := new_prg
        done;
        !old_prg
        
  end