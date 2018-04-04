open Language

module Analysis = 
  struct
    
    (* Language statement annotated with analyse result *)
    type 'a t =
        | ARead   of string * 'a
        | AWrite  of Expr.t * 'a
        | AAssign of string * Expr.t * 'a
        | ASeq    of 'a t * 'a t * 'a
        | ASkip   of 'a
        | AIf     of Expr.t * 'a t * 'a t * 'a
        | AWhile  of Expr.t * 'a t * 'a
        | ARepeat of 'a t * Expr.t * 'a
        | ACall   of string * Expr.t list * 'a
    
    let rec annotate (prg : Stmt.t) (init : 'a) : 'a t = match prg with
        | Stmt.Read    x          -> ARead (x, init)
        | Stmt.Write   e          -> AWrite (e, init)
        | Stmt.Assign (x, e)      -> AAssign (x, e, init)
        | Stmt.Seq    (s1, s2)    -> ASeq (annotate s1 init, annotate s2 init, init)
        | Stmt.Skip               -> ASkip init
        | Stmt.If     (e, s1, s2) -> AIf (e, annotate s1 init, annotate s2 init, init)
        | Stmt.While  (e, s)      -> AWhile (e, annotate s init, init)
        | Stmt.Repeat (s, e)      -> ARepeat (annotate s init, e, init)
        | Stmt.Call   (f, params) -> ACall (f, params, init)
    
    let rec print_result elem_printer = function
        | ARead   (_, a)       -> print_string @@ "ARead "   ^ elem_printer a; print_string "\n"
        | AWrite  (_, a)       -> print_string @@ "AWrite "  ^ elem_printer a; print_string "\n"
        | AAssign (_, _, a)    -> print_string @@ "AAssign " ^ elem_printer a; print_string "\n"
        | ASeq    (l, r, a)    -> print_result elem_printer l;
                                  print_result elem_printer r
        | ASkip   a            -> print_string @@ "ASkip "   ^ elem_printer a; print_string "\n"
        | AIf     (_, l, r, a) -> print_string "// if\n";
                                  print_result elem_printer l;
                                  print_string "// else\n";
                                  print_result elem_printer r;
                                  print_string @@ "// if-end " ^ elem_printer a; print_string "\n"
        | AWhile  (_, b, a)    -> print_string "// while\n";
                                  print_result elem_printer b;
                                  print_string @@ "// while-end " ^ elem_printer a; print_string "\n"
        | ARepeat (b, _, a)    -> print_string "// repeat\n";
                                  print_result elem_printer b;
                                  print_string @@ "// repeat-end " ^ elem_printer a; print_string "\n"
        | ACall   (_, _, a)    -> print_string @@ "ACall "   ^ elem_printer a; print_string "\n"
    
    let rec forward_analyse' (aprg : 'a t) (init : 'a) (combine : 'a -> 'a -> 'a) (kill : 'a t -> 'a -> 'a) (gen : 'a t -> 'a -> 'a) (change : 'a -> 'a -> bool) : 'a * 'a t * bool = 
        let exit_analysis = gen aprg (kill aprg init) in
        match aprg with
        | ARead   (x, a)         -> (exit_analysis, ARead (x, exit_analysis), change a exit_analysis) 
        | AWrite  (e, a)         -> (exit_analysis, AWrite (e, exit_analysis), change a exit_analysis)
        | AAssign (x, e, a)      -> (exit_analysis, AAssign (x, e, exit_analysis), change a exit_analysis)
        | ASkip a                -> (exit_analysis, ASkip exit_analysis, change a exit_analysis)
        | ACall   (f, params, a) -> (exit_analysis, ACall (f, params, exit_analysis), change a exit_analysis)
        | ASeq    (s1, s2, a)    -> let (exit1, as1, change1) = forward_analyse' s1 init combine kill gen change in
                                    let (exit2, as2, change2) = forward_analyse' s2 exit1 combine kill gen change in
                                    (exit2, ASeq (as1, as2, exit2), change1 || change2 || change a exit2)
        | AIf     (e, s1, s2, a) -> let (exit1, as1, change1) = forward_analyse' s1 init combine kill gen change in
                                    let (exit2, as2, change2) = forward_analyse' s2 init combine kill gen change in
                                    let exit = combine exit1 exit2 in
                                    (exit, AIf (e, as1, as2, exit), change1 || change2 || change a exit)
        | AWhile  (e, s, a)      -> let global_change = ref true in
                                    let annotated_body = ref s in
                                    let current_init = ref init in
                                    let ever_change = ref false in
                                    let last_exit = ref init in 
                                    while !global_change do
                                        let (exit, new_body, changed) = forward_analyse' !annotated_body !current_init combine kill gen change in
                                        current_init := combine init exit;
                                        annotated_body := new_body;
                                        global_change := changed;
                                        if changed then ever_change := true;
                                        last_exit := exit
                                    done;
                                    let exit = combine !last_exit init in
                                    (exit, AWhile (e, !annotated_body, exit), !ever_change)
        | ARepeat (s, e, a)      -> failwith "Not supported: Repeat"
    
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
    
  end