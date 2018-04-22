open Ostap
open Analysis
open Language

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.ident ["read"; "write"; "skip"; "if"; "then"; "else"; "elif"; "fi"; "while"; "do"; "od"; "repeat"; "until"; "for"; "fun"; "local"] s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip.nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.parse) -EOF))

let string_of_pair ((a, b) : string * int option) = match b with
    | Some x -> Printf.sprintf "%s = %d" a x
    | None   -> Printf.sprintf "%s = None" a

let string_of_pair_range ((a, (b, c)) : string * (int * int)) = Printf.sprintf "(%s, [%d, %d])" a b c

let main =
  try
    let interpret  = Sys.argv.(1) = "-i"  in
    let stack      = Sys.argv.(1) = "-s"  in
    let analyse    = Sys.argv.(1) = "-a"  in
    let to_compile = not (interpret || stack || analyse) in
    let infile     = Sys.argv.(if not to_compile then 2 else 1) in
    match parse infile with
    | `Ok prog ->
      if analyse then
        let (_, s) = prog in
        let analyse_tree = ConstantPropagation.constant_propagation s in
        print_string "Propagate constants:\n";
        MonotoneFramework.print_result (fun x -> "[" ^ (String.concat ", " (List.map string_of_pair x)) ^ "]") analyse_tree;
        let analyse_tree = LiveVariables.live_variables s in
        print_string "\nLive variables:\n";
        MonotoneFramework.print_result (fun x -> "[" ^ (String.concat ", " x) ^ "]") analyse_tree;
        let analyse_tree = TrueExpressions.true_expressions s in
        print_string "\nTrue expressions:\n";
        MonotoneFramework.print_result (fun x -> "[" ^ (String.concat ", " @@ List.map Expr.pretty_print x) ^ "]") analyse_tree;
        let analyse_tree = IntervalAnalysis.interval_analysis s in
        print_string "\nInterval analysis:\n";
        MonotoneFramework.print_result (fun x -> "[" ^ (String.concat ", " @@ List.map string_of_pair_range x) ^ "]") analyse_tree;
        print_string "\nResult:\n";
        let result = Optimizations.optimize s in
        print_string @@ Stmt.pretty_print result
      else 
      if to_compile
      then failwith "Not implemented yet (Driver.ml:28)"
        (*            
        let basename = Filename.chop_suffix infile ".expr" in
        ignore @@ X86.build prog basename
        *)
      else 
	let rec read acc =
	  try
	    let r = read_int () in
	    Printf.printf "> ";
	    read (acc @ [r]) 
          with End_of_file -> acc
	in
	let input = read [] in	
	let output = 
	  if interpret 
	  then Language.eval prog input 
	  else SM.run (SM.compile prog) input
	in
	List.iter (fun i -> Printf.printf "%d\n" i) output
    | `Fail er -> Printf.eprintf "Syntax error: %s\n" er
  with Invalid_argument _ ->
    Printf.printf "Usage: rc [-i | -s] <input file.expr>\n"
