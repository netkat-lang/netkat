open Nkcmd

open Nkego
open Sexplib0

(*

f:=1;(f=1;f:=2 + f=2;f:=3)*;f=3
f:=1;(skip + (f=1;f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3
(f:=1;skip + f:=1;(f=1;f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3
(f:=1;skip + (f:=1;f=1;f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3
(f:=1;skip + (f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3

*)

(* create an egraph *)
let graph = EGraph.init ()
(* add expressions *)
(*let expr1 = [%s (seq (eq a 2) (eq a 2))]*)
(* let expr1 = [%s (seq (set a 1) (star drop))] *)
let expr1 = [%s (seq (set a 1) (seq (star (union (seq (eq a 1) (set a 2)) (seq (eq a 2) (set a 3)))) (eq a 3)))]
(*let expr1 = Nkexp.to_sexp (Nkexp.Seq([Nkexp.Mod(Field.get_or_assign_fid "a",Value.of_int 2);Nkexp.Mod(Field.get_or_assign_fid "a",Value.of_int 3)]))*)
(*let expr1 = Nkexp.to_sexp (Nkexp.filter true (Field.get_or_assign_fid "a") (Value.of_int 2))*)
let expr2 = expr1 (* Sexplib0.Sexp.List [Sexp.Atom "="; Sexp.Atom "f"; Sexp.Atom "1"] *)
let e1 = EGraph.add_node graph (L.of_sexp expr1)
let e2 = EGraph.add_node graph (L.of_sexp expr2)
let rules =
  (* Kleene Algebra Axioms *)
  make_rules ~bidir:true  [%s (union "?a" (union "?b" "?c"))] [%s (union (union "?a" "?b") "?c")] @
  make_rules ~bidir:true  [%s (union "?a" "?b")] [%s (union "?b" "?a")] @
  make_rules ~bidir:true  [%s (union "?a" drop)] [%s "?a"] @
  make_rules ~bidir:true  [%s (union "?a" "?a")] [%s "?a"] @
  make_rules ~bidir:true  [%s (seq "?a" (seq "?b" "?c"))] [%s (seq (seq "?a" "?b") "?c")] @
  make_rules ~bidir:true  [%s (seq skip "?a")] [%s "?a"] @
  make_rules ~bidir:true  [%s (seq "?a" skip)] [%s "?a"] @
  make_rules ~bidir:true  [%s (seq "?a" (union "?b" "?c"))] [%s (union (seq "?a" "?b") (seq "?a" "?c"))] @
  make_rules ~bidir:true  [%s (seq (union "?a" "?b") "?c")] [%s (union (seq "?a" "?c") (seq "?b" "?c"))] @
  make_rules ~bidir:false [%s (seq drop "?a")] [%s drop] @
  make_rules ~bidir:false [%s (seq "?a" drop)] [%s drop] @
  make_rules ~bidir:true  [%s (union skip (seq "?a" (star "?a")))] [%s (star "?a")] @
  make_rules ~bidir:true  [%s (union skip (seq (star "?a") "?a"))] [%s (star "?a")] @
  (* Additional Boolean Algebra Axioms *)
  (*make_rules ~bidir:false [%s (union "?a" skip)] [%s skip] @
  make_rules ~bidir:false [%s (union "?a" (not "?a"))] [%s skip] @
  make_rules ~bidir:false [%s (seq "?a" "?b")] [%s (seq "?b" "?a")] @
  make_rules ~bidir:false [%s (seq "?a" (not "?a"))] [%s drop] @*)
  make_cond_rules ~bidir:true [%s (seq "?a" "?a")] [%s "?a"] (Nkego.is_predicate "a") @
  (* Packet Algebra Axioms *)
  make_rules ~bidir:true  [%s (seq dup (set "?a" "?b"))] [%s (seq (set "?a" "?b") dup)] @
  make_rules ~bidir:true  [%s (seq (set "?a" "?b") (eq "?a" "?b"))] [%s (set "?a" "?b")] @
  make_rules ~bidir:true  [%s (seq (eq "?a" "?b") (set "?a" "?b"))] [%s (eq "?a" "?b")] @
  make_rules ~bidir:false [%s (seq (set "?a" "?b") (set "?a" "?c"))] [%s (set "?a" "?c")] @
  make_cond_rules ~bidir:false [%s (seq (eq "?a" "?b") (eq "?a" "?c"))] [%s drop] (Nkego.is_distinct "b" "c") @
  (* Tentative *)
  (*make_rules ~bidir:false [%s (seq (set "?a" "?b") (eq "?c" "?a"))] [%s (eq "?c" "?b")] @*)
  []
let _ = EGraph.run_until_saturation ~fuel:(`Bounded 20) graph rules
let r = Extractor.extract graph e1
let result = L.to_sexp r
let _ = Printf.printf "%s\n" (Sexp.to_string result)
let _ = Printf.printf "%s\n" (Nkexp.to_string (Nkexp.of_sexp result))
let _ = Printf.printf "%s\n" (Nkexp.to_string (Nkexp.of_sexp [%s (seq (set a 1) (set a 2))]))
(* Convert to graphviz *)
let g : Odot.graph = EGraph.to_dot graph
let _ = let c = open_out "test.dot" in Printf.fprintf c "%s" (Odot.string_of_graph g); close_out c
(* dot -Tpdf test.dot -o test.pdf *)


let rec parse_file_with_env (env: Env.t) (fn: string) : Nkcmd.t list =
  let f = In_channel.open_text fn in
  let lexbuf = Sedlexing.Utf8.from_channel f in
  let lexer  = Sedlexing.with_tokenizer Nkpl_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.nkpl_file in
  (try
    parser lexer
  with
    | Nkpl_parser.Error s ->
      let (x,y) = Sedlexing.lexing_positions lexbuf in
      Printf.printf "Parse error: %s (%d:%d)\n" (Sedlexing.Utf8.lexeme lexbuf) x.pos_lnum (x.pos_cnum - x.pos_bol);
      exit 1)

and parse_string (env: Env.t) (s: string) : Nkcmd.t option =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer  = Sedlexing.with_tokenizer Nkpl_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.single_cmd in
  (try
    parser lexer
  with
    | Nkpl_parser.Error s ->
      let (x,y) = Sedlexing.lexing_positions lexbuf in
      Printf.printf "Parse error: %s (%d:%d)\n" (Sedlexing.Utf8.lexeme lexbuf) x.pos_lnum (x.pos_cnum - x.pos_bol);
      exit 1)

and parse_file (fn: string) : Nkcmd.t list =
  parse_file_with_env Env.empty fn

let rec interp_file_with_env (env: Env.t) (fn: string) : Env.t =
  let cmds = parse_file_with_env env fn in
  let bn = match String.rindex_opt fn '/' with
           | None -> ""
           | Some i -> String.sub fn 0 (i+1) in
  List.fold_left (interp bn) env cmds

and interp_string (env: Env.t) (s: string) =
  let c = parse_string env s in
  match c with
    | None -> env
    | Some cmd -> interp "" env cmd

and interp_file (fn: string) : Env.t =
  interp_file_with_env Env.empty fn

and interp (bn: string) (env: Env.t) (c: t) =
  match c with
  | Import s -> interp_file_with_env env (bn ^ s)
  | Check (b, e1, e2) -> let start = Unix.gettimeofday () in
                         let e1' = Nkexp.eval env e1 in
                         let e2' = Nkexp.eval env e2 in
                         let a1 = Nka.autom e1' in
                         let a2 = Nka.autom e2' in
                         (* let () = Printf.printf "Autom a1:\n%s\n-----\n%!" (Nka.to_string a1) in *)
                         (* let () = Printf.printf "Autom a2:\n%s\n-----\n%!" (Nka.to_string a2) in *)
                         let sgn = if b then "≡" else "≢" in
                         let res = Nka.xor_rep a1 a2 (Field.get_fields ()) in
                         let stop = Unix.gettimeofday () in
                         (*
                         (* Alternative forms of the bisim check: *)
                         let res = Sp.eq (Nka.forward (Nk.xor e1' e2')) Sp.drop in
                         let res = Nka.bisim a1 a2 in
                         *)
                         (*
                         let () = match res,res',res'' with
                         | None,true,true
                         | Some _,false,false -> ()
                         | _ ->
                             let () = match res with
                             | None -> Printf.printf "None "
                             | Some _ -> Printf.printf "Some " in
                             (* let () = Printf.printf "%b\n"  res' in *)
                             let () = Printf.printf "%b %b\n"  res' res'' in
                             let () = Printf.printf "exps: %s ≡ %s\n"
                                (Nk.to_string e1') (Nk.to_string e2') in
                             let () = Printf.printf "forward of e1-e2: %s\n"
                                (Sp.to_string (Nka.forward (Nk.diff e1' e2'))) in
                             let () = Printf.printf "forward of e2-e1: %s\n"
                                (Sp.to_string (Nka.forward (Nk.diff e2' e1'))) in
                             let () = Printf.printf "forward of ⊕: %s\n"
                                (Sp.to_string (Nka.forward (Nk.xor e1' e2'))) in
                             let () = Printf.printf "nka of e1: %s\n"
                                (Nka.to_string (Nka.autom e1')) in
                             let () = Printf.printf "nka of e2: %s\n"
                                (Nka.to_string (Nka.autom e2')) in
                             let () = Printf.printf "nka of e1-e2: %s\n"
                                (Nka.to_string (Nka.autom (Nk.diff e1' e2'))) in
                             let () = Printf.printf "nka of e2-e1: %s\n"
                                (Nka.to_string (Nka.autom (Nk.diff e2' e1'))) in
                             let () = Printf.printf "nka of e1⊕e2: %s\n"
                                (Nka.to_string (Nka.autom (Nk.xor e1' e2'))) in
                             failwith "mismatched bisim results!" in
                         *)
                         begin
                         match b, res with
                         | true, None
                         | false, Some _ ->
                           Printf.printf "*** Check \u{001b}[32mSUCCESS!\u{001b}[0m *** (%s %s %s) time: %fs\n%!"
                            (Nkexp.to_string e1) sgn (Nkexp.to_string e2) (stop -. start)
                         | true, Some cex ->
                            begin
                              Printf.printf "XXX Check \u{001b}[31mFAILED.\u{001b}[0m XXX (expected: %s %s %s)\n%!"
                                (Nkexp.to_string e1) sgn (Nkexp.to_string e2);
                              Printf.printf "Counterexample trace:\n%s\n%!" (Trace.to_string cex)(*; exit 1*)
                            end
                         | false, None ->
                            begin
                            Printf.printf "XXX Check \u{001b}[31mFAILED.\u{001b}[0m XXX (expected: %s %s %s)\n%!"
                              (Nkexp.to_string e1) sgn (Nkexp.to_string e2)(*; exit 1*)
                            end
                          end; env
  | Print e -> Printf.printf "%s\n%!" (Nkexp.eval env e |> Nk.to_string); env
  | Tikz e -> Printf.printf "%s\n%!" (Nkexp.eval env e |> Deriv.e |> Spp.tikz); env
  | Let (s, e) -> Env.bind_exp env s (Nkexp.eval env e)
  | VLet (s, v) -> Env.bind_val env s v
  | Rep e ->
      let a = (Nkexp.eval env e) |> Nka.autom in
      let () = Nka.rep a (Field.get_fields ()) |> Trace.to_string |> Printf.printf "%s\n%!" in
      env
  | For (v, i_0, i_n, cmd) ->
      let indexes = List.init (i_n - i_0 + 1) (fun i -> i_0 + i) in
      List.fold_left (fun env i -> 
        let env' = Env.bind_val env v (Value.of_int i) in
        interp bn env' cmd
      ) env indexes
