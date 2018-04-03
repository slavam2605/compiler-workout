open GT
open Language

module Analysis = 
  struct
    
    (* Language statement annotated with analyse result *)
    @type 'a t =
        | ARead   of string * 'a
        | AWrite  of Expr.t * 'a
        | AAssign of string * Expr.t * 'a
        | ASeq    of 'a t * 'a t * 'a
        | ASkip   of 'a
        | AIf     of Expr.t * 'a t * 'a t * 'a
        | AWhile  of Expr.t * 'a t * 'a
        | ARepeat of 'a t * Expr.t * 'a
        | ACall   of string * Expr.t list * 'a
    with show
    
    let rec print_result elem_printer = function
        | ARead   (_, a)       -> print_string @@ "ARead "   ^ elem_printer a; print_string "\n"
        | AWrite  (_, a)       -> print_string @@ "AWrite "  ^ elem_printer a; print_string "\n"
        | AAssign (_, _, a)    -> print_string @@ "AAssign " ^ elem_printer a; print_string "\n"
        | ASeq    (l, r, a)    -> print_string @@ "ASeq "    ^ elem_printer a; print_string "\n";
                                  print_result elem_printer l;
                                  print_result elem_printer r
        | ASkip   a            -> print_string @@ "ASkip "   ^ elem_printer a; print_string "\n"
        | AIf     (_, l, r, a) -> print_string @@ "AIf "     ^ elem_printer a; print_string "\n";
                                  print_result elem_printer l;
                                  print_string "// else\n";
                                  print_result elem_printer r;
                                  print_string "// if-end\n"
        | AWhile  (_, b, a)    -> print_string @@ "AWhile "  ^ elem_printer a; print_string "\n";
                                  print_result elem_printer b;
                                  print_string "// while-end\n"
        | ARepeat (b, _, a)    -> print_string @@ "ARepeat " ^ elem_printer a; print_string "\n";
                                  print_result elem_printer b;
                                  print_string "// repeat-end\n"
        | ACall   (_, _, a)    -> print_string @@ "ACall "   ^ elem_printer a; print_string "\n"
    
    let rec forward_analyse (prg : Stmt.t) (init : 'a) (combine : 'a -> 'a -> 'a) (kill : Stmt.t -> 'a -> 'a) (gen : Stmt.t -> 'a -> 'a) : 'a * 'a t = 
        let exit_analysis = gen prg (kill prg init) in
        match prg with
        | Read    x          -> (exit_analysis, ARead (x, exit_analysis)) 
        | Write   e          -> (exit_analysis, AWrite (e, exit_analysis))
        | Assign (x, e)      -> (exit_analysis, AAssign (x, e, exit_analysis))
        | Skip               -> (exit_analysis, ASkip exit_analysis)
        | Call   (f, params) -> (exit_analysis, ACall (f, params, exit_analysis))
        | Seq    (s1, s2)    -> let (exit1, as1) = forward_analyse s1 init combine kill gen in
                                let (exit2, as2) = forward_analyse s2 exit1 combine kill gen in
                                (exit2, ASeq (as1, as2, exit2))
        | If     (e, s1, s2) -> let (exit1, as1) = forward_analyse s1 init combine kill gen in
                                let (exit2, as2) = forward_analyse s2 init combine kill gen in
                                let exit = combine exit1 exit2 in
                                (exit, AIf (e, as1, as2, exit))
        | While  (e, s)      -> failwith "Not supported: While"
        | Repeat (s, e)      -> failwith "Not supported: Repeat"
    
  end