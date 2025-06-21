open Nkcmd

let rec interp_file_with_env (env: Env.t) (fn: string) : Env.t =
  let f = In_channel.open_text fn in
  let lexbuf = Sedlexing.Utf8.from_channel f in
  let lexer  = Sedlexing.with_tokenizer Nkpl_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.nkpl_file in
  let cmds = parser lexer in
  let bn = match String.rindex_opt fn '/' with
           | None -> ""
           | Some i -> String.sub fn 0 (i+1) in
  List.fold_left (interp bn) env cmds

and interp_string (env: Env.t) (s: string) =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer  = Sedlexing.with_tokenizer Nkpl_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.single_cmd in
  let c = parser lexer in
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
                              Printf.printf "Counterexample trace:\n%s\n" (Trace.to_string cex);
                              exit 1
                            end
                         | false, None ->
                            begin
                            Printf.printf "XXX Check \u{001b}[31mFAILED.\u{001b}[0m XXX (expected: %s %s %s)\n%!"
                              (Nkexp.to_string e1) sgn (Nkexp.to_string e2);
                            exit 1
                            end
                          end; env
  | Print e -> Printf.printf "%s\n%!" (Nkexp.eval env e |> Nk.to_string); env
  | Tikz e -> Printf.printf "%s\n%!" (Nkexp.eval env e |> Deriv.e |> Spp.tikz); env
  | Let (s, e) -> Env.bind_exp env s (Nkexp.eval env e)
  | VLet (s, v) -> Env.bind_val env s v
  | Rep e -> let a = (Nkexp.eval env e) |> Nka.autom in
             let () = Nka.rep a (Field.get_fields ()) |> Trace.to_string |> Printf.printf "%s\n%!" in
             env

