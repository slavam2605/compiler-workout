open Language

module Analysis = 
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
    
    let rec forward_analyse' (aprg : 'a t) (input : 'a) (combine : 'a -> 'a -> 'a) (kill : 'a t -> 'a -> 'a) (gen : 'a t -> 'a -> 'a) (change : 'a -> 'a -> bool) : 'a * 'a t * bool =
        let exit_analysis = gen aprg (kill aprg input) in
        match aprg with
        | ARead   (x, (_, a))         -> (exit_analysis, ARead (x, (input, exit_analysis)), change a exit_analysis)
        | AWrite  (e, (_, a))         -> (exit_analysis, AWrite (e, (input, exit_analysis)), change a exit_analysis)
        | AAssign (x, e, (_, a))      -> (exit_analysis, AAssign (x, e, (input, exit_analysis)), change a exit_analysis)
        | ASkip (_, a)                -> (exit_analysis, ASkip (input, exit_analysis), change a exit_analysis)
        | ACall   (f, params, (_, a)) -> (exit_analysis, ACall (f, params, (input, exit_analysis)), change a exit_analysis)
        | ASeq    (s1, s2, (_, a))    -> let (exit1, as1, change1) = forward_analyse' s1 input combine kill gen change in
                                         let (exit2, as2, change2) = forward_analyse' s2 exit1 combine kill gen change in
                                         (exit2, ASeq (as1, as2, (input, exit2)), change1 || change2 || change a exit2)
        | AIf     (e, s1, s2, (_, a)) -> let (exit1, as1, change1) = forward_analyse' s1 input combine kill gen change in
                                         let (exit2, as2, change2) = forward_analyse' s2 input combine kill gen change in
                                         let exit = combine exit1 exit2 in
                                         (exit, AIf (e, as1, as2, (input, exit)), change1 || change2 || change a exit)
        | AWhile  (e, s, (_, a))      -> let global_change = ref true in
                                         let annotated_body = ref s in
                                         let current_input = ref input in
                                         let ever_change = ref false in
                                         let last_exit = ref input in
                                         while !global_change do
                                             let (exit, new_body, changed) = forward_analyse' !annotated_body !current_input combine kill gen change in
                                             current_input := combine input exit;
                                             annotated_body := new_body;
                                             global_change := changed;
                                             if changed then ever_change := true;
                                             last_exit := exit
                                         done;
                                         let exit = combine !last_exit input in
                                         (exit, AWhile (e, !annotated_body, (input, exit)), !ever_change)
        | ARepeat (s, e, (_, a))      -> failwith "Not supported: Repeat"
    
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
    
    let rec forward_analyse (prg : Stmt.t) (init : 'a) (combine : 'a -> 'a -> 'a) (kill : 'a t -> 'a -> 'a) (gen : 'a t -> 'a -> 'a) (change : 'a -> 'a -> bool) : 'a t =
        let (_, tree, _) = forward_analyse' (annotate prg init) init combine kill gen change in tree
    
    let rec unique = function
        | []      -> []
        | x::rest -> x :: unique (List.filter (fun y -> x <> y) rest)
    
    let (@@@) (list1 : 'a list) (list2 : 'a list) : 'a list = unique @@ list1 @ list2
    
    let constant_propagation (prg : Stmt.t) (*: [string * int option]*) =
        forward_analyse prg [] (@@@) (
            fun s list -> match s with
                | AAssign (x, _, _) -> List.filter (fun (y, _) -> x <> y) list
                | _ -> list
        ) (
            fun s list -> match s with
                | AAssign (x, e, _) -> (x, const_value list e) :: list
                | _ -> list
        ) (
            fun x y -> x <> y
        )

    let fold_const state e =
        match const_value state e with
            | Some z -> Expr.Const z
            | None   -> e

    let propagate_constants (prg : Stmt.t) : Stmt.t =
        let analyse_result = constant_propagation prg in
        let rec transform = function
            | ARead   (x, (input, _))         -> Stmt.Read x
            | AWrite  (e, (input, _))         -> let value = fold_const input e in
                                                 Stmt.Write value
            | AAssign (x, e, (input, _))      -> let value = fold_const input e in
                                                 Stmt.Assign (x, value)
            | ASkip (input, _)                -> Stmt.Skip
            | ACall   (f, params, (input, _)) -> let value_params = List.map (fold_const input) params in
                                                 Stmt.Call (f, value_params)
            | ASeq    (s1, s2, (input, _))    -> let ss1 = transform s1 in
                                                 let ss2 = transform s2 in
                                                 Stmt.Seq (ss1, ss2)
            | AIf     (e, s1, s2, (input, _)) -> let ss1 = transform s1 in
                                                 let ss2 = transform s2 in
                                                 let value = fold_const input e in
                                                 Stmt.If (value, ss1, ss2)
            | AWhile  (e, s, (input, _))      -> let ss = transform s in
                                                 let value = fold_const input e in
                                                 Stmt.While (value, ss)
            | ARepeat (s, e, (input, _))      -> failwith "Not supported: Repeat"
        in
        transform analyse_result
    
  end