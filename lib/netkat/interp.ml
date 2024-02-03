open Nkcmd

let rec interp_file (fn: string) : Env.t =
  let f = In_channel.open_text fn in
  let lexbuf = Sedlexing.Utf8.from_channel f in
  let lexer  = Sedlexing.with_tokenizer Nkpl_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.nkpl_file in
  let cmds = parser lexer in
  List.fold_left interp Env.empty cmds

and interp (env: Env.t) (c: t) =
  match c with
  | Import s -> interp_file s
  | Check (b, e1, e2) -> let a1 = Nkexp.eval env e1 |> Nka.autom in
                         (* let () = Printf.printf "Autom a1:\n%s\n-----\n%!" (Nkexpa.to_string a1) in *)
                         let a2 = Nkexp.eval env e2 |> Nka.autom in
                         (* let () = Printf.printf "Autom a2:\n%s\n-----\n%!" (Nkexpa.to_string a2) in *)
                         let sgn = if b then "≡" else "≢" in
                         if b = Nka.bisim a1 a2 then
                           Printf.printf "*** Check \u{001b}[32mSUCCESS!\u{001b}[0m *** (%s %s %s)\n%!"
                            (Nkexp.to_string e1) sgn (Nkexp.to_string e2)
                         else
                            begin
                              Printf.printf "XXX Check \u{001b}[31mFAILED.\u{001b}[0m XXX (expected: %s %s %s)\n%!"
                                (Nkexp.to_string e1) sgn (Nkexp.to_string e2);
                              exit 1
                            end; env
  | Print e -> Printf.printf "%s\n%!" (Nkexp.to_string e); env
  | Let (s, e) -> Env.bind_exp env s (Nkexp.eval env e)
  | VLet (s, v) -> Env.bind_val env s v

