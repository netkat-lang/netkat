open Nkcmd

type result = Success | Fail of Trace.t option

let merge = (fun k v1 v2 -> Some(Value.S.union v1 v2))

let map_union m1 m2 = match (m1,m2) with
| (None,None) -> None
| (Some(m),None) 
| (None,Some(m))-> Some(m)
| (Some(m1),Some(m2)) -> Some(Field.M.union merge m1 m2)

let map_add m f v =
  match m with
  | None -> None
  | Some(m) ->
    let st = (match Field.M.find_opt f m with Some(s) -> s | None -> Value.S.empty) in
    Some(Field.M.add f (Value.S.add v st) m)

let result_list_to_json rl = 
  let item r = match r with
  | Success -> "{\"result\":\"SUCCESS\"}"
  | Fail(t) ->
    Printf.sprintf "{\"result\":\"FAIL\"%s}"
      (Option.fold ~none:"" ~some:(fun t ->
        ", \"trace\":["^(fst (List.fold_left (fun (acc,flag) pk -> (acc^(if flag then "" else ", ")^(Pk.to_json pk), false)) ("",true) t))^"]") t
      )
  in
  "[\n"^(fst (List.fold_left (fun (acc,flag) r -> (acc^(if flag then "" else ",\n")^(item r), false)) ("",true) rl))^"\n]"

let printf out fmt =
  Printf.ksprintf out fmt 

let expect_nk v = match v with
| Env.Expr(e) -> e
| _ -> failwith (Printf.sprintf "unexpected closure: %s" "<closure>") (* TODO XXX - more contextual error message needed *)

let rec eval ((env: Env.t), (m: Value.S.t Field.M.t option)) (e: Nkexp.t) : (Env.nk_val * Value.S.t Field.M.t option) =
  (*printf "EVAL: %s\n" (Nkexp.to_string e);
  printf "   env: %s\n" (Env.to_string env);*)
  flush stdout;
    match e with
    | Nkexp.Num i  -> (Env.Num(Value.of_int i), m)
    | Nkexp.Drop  -> (Env.Expr(Nk.Drop), m)
    | Nkexp.Skip -> (Env.Expr(Nk.Skip), m)
    | Nkexp.Dup -> (Env.Expr(Nk.dup), m)
    | Nkexp.Seq el ->
      let (l1,l2) = List.split (List.map (eval (env,m)) el) in
      let m2 = List.fold_left (fun acc m' -> map_union m' acc) m l2 in
      (Env.Expr(l1 |> List.map expect_nk |> Nk.seq),m2)
    | Nkexp.Union el ->
      let (l1,l2) = List.split (List.map (eval (env,m)) el) in
      let m2 = List.fold_left (fun acc m' -> map_union m' acc) m l2 in
      (Env.Expr(l1 |> List.map expect_nk |> Nk.union),m2)
    | Nkexp.Intersect el ->
      let (l1,l2) = List.split (List.map (eval (env,m)) el) in
      let m2 = List.fold_left (fun acc m' -> map_union m' acc) m l2 in
      (Env.Expr(l1 |> List.map expect_nk |> Nk.intersect),m2)
    | Nkexp.Star e ->
      let l = eval (env,m) e in
      (Env.Expr(fst l |> expect_nk |> Nk.star), snd l)
    | Nkexp.Neg e ->
      let l = eval (env,m) e in
      (Env.Expr(Nk.neg (fst l |> expect_nk)), snd l)
    | Nkexp.Filter (b,f,v) -> (Env.Expr(Nk.filter b f v), map_add m f v)
    | Nkexp.VFilter (b,f,var) ->
      let v = (Env.lookup_val env var |> expect_val) in
      (Env.Expr(Nk.filter b f v), map_add m f v)
    | Nkexp.Mod (f,v) -> (Env.Expr(Nk.modif f v), map_add m f v)
    | Nkexp.VMod (f,var) ->
      let v = (Env.lookup_val env var |> expect_val) in
      (Env.Expr(Nk.modif f v), map_add m f v)
    | Nkexp.Var x -> (Env.lookup_val env x, m (* <- is this right? *))
    | Nkexp.Xor (e1,e2) ->
      let l1 = eval (env,m) e1 in
      let l2 = eval (env,snd l1) e2 in
      (Env.Expr(Nk.xor (fst l1 |> expect_nk) (fst l2 |> expect_nk)), snd l2)
    | Nkexp.Diff (e1,e2) ->
      let l1 = eval (env,m) e1 in
      let l2 = eval (env,snd l1) e2 in
      (Env.Expr(Nk.diff (fst l1 |> expect_nk) (fst l2 |> expect_nk)), snd l2)
    | Nkexp.Fwd (None,e) ->
      let l = eval (env,m) e in
      (Env.Expr(Nka.forward (fst l |> expect_nk) |> Sp.to_exp), snd l)
    | Nkexp.Fwd (Some(p),e) ->
      let l = eval (env,m) e in
      (Env.Expr(Nka.forward_init (fst l |> expect_nk) (Sp.of_pk p) |> Sp.to_exp), snd l)
    | Nkexp.Bwd (None,e) ->
      let l = eval (env,m) e in
      (Env.Expr(Nka.backward (fst l |> expect_nk) |> Sp.to_exp), snd l)
    | Nkexp.Bwd (Some(p),e) ->
      let l = eval (env,m) e in
      (Env.Expr(Nka.backward_final (fst l |> expect_nk) (Sp.of_pk p) |> Sp.to_exp), snd l)
    | Nkexp.Forall (f,e) -> begin
                      match e with
                      | Nkexp.Drop
                      | Nkexp.Skip -> eval (env,m) e
                      | _ -> failwith ("TODO FORALL: " ^ __LOC__)
                      end
    | Nkexp.Exists (f,e) -> failwith ("TODO EXISTS: " ^ __LOC__)
    | Nkexp.Lambda (s,e) -> (Closure(env,s,e), m)
    | Nkexp.App(e1,e2) ->
      let (v1,m1) = eval (env,m) e1 in (
        match v1 with
        | Closure(env',s,body) ->
          let (v2,m2) = eval (env,m1) e2 in
          let env'' = Env.bind_val env' s v2 in
          eval (env'',m2) body
        | _ -> failwith (Printf.sprintf "expected first argument to evaluate to closure: %s" (Nkexp.to_string e))
      )
      (*printf "env: %s\n" (Env.to_string env2);*)
      (*printf "EVAL APP: %s --> %s\n" (Nkexp.to_string e) (Nk.to_string result);*)

let rec parse_file_with_env out (env: Env.t) (fn: string) : Nkcmd.t list =
  In_channel.with_open_text fn (fun f ->
    let lexbuf = Sedlexing.Utf8.from_channel f in
    let lexer  = Sedlexing.with_tokenizer (Nkpl_lexer.token (Nkpl_lexer.fresh_state ())) lexbuf in
    let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.nkpl_file in
    (try
      parser lexer
    with
      | Nkpl_parser.Error s ->
        let (x,y) = Sedlexing.lexing_positions lexbuf in
        printf out "Parse error: %s (%d:%d)\n" (Sedlexing.Utf8.lexeme lexbuf) x.pos_lnum (x.pos_cnum - x.pos_bol);
        exit 1)
  )

and parse_string out (env: Env.t) (s: string) : Nkcmd.t option =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer  = Sedlexing.with_tokenizer (Nkpl_lexer.token (Nkpl_lexer.fresh_state ())) lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.single_cmd in
  (try
    parser lexer
  with
    | Nkpl_parser.Error s ->
      let (x,y) = Sedlexing.lexing_positions lexbuf in
      printf out "Parse error: %s (%d:%d)\n" (Sedlexing.Utf8.lexeme lexbuf) x.pos_lnum (x.pos_cnum - x.pos_bol);
      exit 1)

and parse_file out (fn: string) : Nkcmd.t list =
  parse_file_with_env out Env.empty fn

let rec interp_cmds_with_env out ((env: Env.t), (m: Value.S.t Field.M.t option)) (bn: string) (cmds: Nkcmd.t list) : (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list) =
  let ((env,m), res) = List.fold_left (fun ((env,m),res) e -> let ((env2,m2),res2) = interp out bn (env,m) e in ((env2,m2),(res@res2))) ((env,m),[]) cmds in
  (cmds, (env,m), res)

and interp_file_with_env out ((env: Env.t), (m: Value.S.t Field.M.t option)) (fn: string) : (string * (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list)) =
  let cmds = parse_file_with_env out env fn in
  let bn = match String.rindex_opt fn '/' with
           | None -> ""
           | Some i -> String.sub fn 0 (i+1) in
  (bn, interp_cmds_with_env out (env,m) bn cmds)

and interp_string out ((env: Env.t), (m: Value.S.t Field.M.t option)) (s: string) =
  let c = parse_string out env s in
  match c with
    | None -> ((env,m),[])
    | Some cmd -> interp out "" (env,m) cmd

and interp_file out (fn: string) : (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list) =
  snd (interp_file_with_env out (Env.empty,None (*TODO - is this right?*)) fn)

and parse_program_string out (env: Env.t) (s: string) : Nkcmd.t list =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer  = Sedlexing.with_tokenizer (Nkpl_lexer.token (Nkpl_lexer.fresh_state ())) lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.nkpl_file in
  (try
    parser lexer
  with
    | Nkpl_parser.Error s ->
      let (x,y) = Sedlexing.lexing_positions lexbuf in
      printf out "Parse error: %s (%d:%d)\n" (Sedlexing.Utf8.lexeme lexbuf) x.pos_lnum (x.pos_cnum - x.pos_bol);
      exit 1)

and interp_program_string out ((env: Env.t), (m: Value.S.t Field.M.t option)) (s: string) : (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list) =
  let cmds = parse_program_string out env s in
  interp_cmds_with_env out (env,m) "" cmds

and interp out (bn: string) ((env: Env.t), (m: Value.S.t Field.M.t option)) (c: t) : ((Env.t * Value.S.t Field.M.t option) * result list ) =
  match c with
  | Import s ->
    let (_,(env,m),res) = snd (interp_file_with_env out (env,m) (bn ^ s)) in
    ((env,m),res)
  | Check (b, e1, e2) -> let start = Unix.gettimeofday () in
                         let l1 = eval (env,m) e1 in
                         let l2 = eval (env,snd l1) e2 in
                         let e1' = fst l1 |> expect_nk in
                         let e2' = fst l2 |> expect_nk in
                         (*printf out ">> CHECKING: %s == %s\n" (Nk.to_string e1') (Nk.to_string e2');*)
                         let a1 = Nka.autom e1' in
                         let a2 = Nka.autom e2' in
                         (* let () = printf out "Autom a1:\n%s\n-----\n%!" (Nka.to_string a1) in *)
                         (* let () = printf out "Autom a2:\n%s\n-----\n%!" (Nka.to_string a2) in *)
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
                             | None -> printf out "None "
                             | Some _ -> printf out "Some " in
                             (* let () = printf out "%b\n"  res' in *)
                             let () = printf out "%b %b\n"  res' res'' in
                             let () = printf out "exps: %s ≡ %s\n"
                                (Nk.to_string e1') (Nk.to_string e2') in
                             let () = printf out "forward of e1-e2: %s\n"
                                (Sp.to_string (Nka.forward (Nk.diff e1' e2'))) in
                             let () = printf out "forward of e2-e1: %s\n"
                                (Sp.to_string (Nka.forward (Nk.diff e2' e1'))) in
                             let () = printf out "forward of ⊕: %s\n"
                                (Sp.to_string (Nka.forward (Nk.xor e1' e2'))) in
                             let () = printf out "nka of e1: %s\n"
                                (Nka.to_string (Nka.autom e1')) in
                             let () = printf out "nka of e2: %s\n"
                                (Nka.to_string (Nka.autom e2')) in
                             let () = printf out "nka of e1-e2: %s\n"
                                (Nka.to_string (Nka.autom (Nk.diff e1' e2'))) in
                             let () = printf out "nka of e2-e1: %s\n"
                                (Nka.to_string (Nka.autom (Nk.diff e2' e1'))) in
                             let () = printf out "nka of e1⊕e2: %s\n"
                                (Nka.to_string (Nka.autom (Nk.xor e1' e2'))) in
                             failwith "mismatched bisim results!" in
                         *)
                         ((env,snd l2), 
                         match b, res with
                         | true, None
                         | false, Some _ ->
                           printf out "## *** Check \u{001b}[32mSUCCESS!\u{001b}[0m *** (%s %s %s) time: %fs\n%!"
                            (Nkexp.to_string e1) sgn (Nkexp.to_string e2) (stop -. start);
                           [Success]
                         | true, Some cex ->
                              printf out "## >>> Check \u{001b}[31mFAILED.\u{001b}[0m <<< (expected: %s %s %s)\n%!"
                                (Nkexp.to_string e1) sgn (Nkexp.to_string e2);
                              printf out "Counterexample trace:\n%s\n%!" (Trace.to_string cex);
                              [Fail(Some(cex))]
                         | false, None ->
                            printf out  "## >>> Check \u{001b}[31mFAILED.\u{001b}[0m <<< (expected: %s %s %s)\n%!"
                              (Nkexp.to_string e1) sgn (Nkexp.to_string e2);
                            [Fail(None)]
                         )
  | Prints s -> Printf.printf "%s\n%!" s; ((env,m),[])
  | Print e ->
    (* this overwrites an environment variable *)
    (*let env = Env.bind_val env "foobar" (Env.Expr(Nk.skip)) in*)
    (*let e' = eval env e |> expect_nk in
    let init = Nkpl_parser_utils.exp_of_string "@a=3" in (
      match init with
      | Some(ex) ->*)
        let l = eval (env,m) e in
        printf out "%s\n%!" (fst l |> expect_nk |> Nk.to_string_z3);
        (*let ex2 = eval env ex |> expect_nk in
        let ei = Nka.forward ex2 in
        let x = Nka.forward_init e' ei in
        printf out "forward: %s\n%!" (Sp.to_string x);
        printf out "backward: %s\n%!" (Sp.to_string (Nka.backward_final e' ei));*)
        ((env,snd l), [])
      (*| None ->
        env
    )*)
  | Tikz e ->
    let l = eval (env,m) e in
    printf out "%s\n%!" (fst l |> expect_nk |> Deriv.e |> Spp.tikz); ((env,snd l),[])
  | Let (s, e) ->
    (*printf out ">> LET %s = %s\n" s (Nkexp.to_string e);*)
    let l = eval (env,m) e in
    ((Env.bind_val env s (fst l), snd l), [])
  | VLet (s, v) ->
    ((Env.bind_val env s (Env.Num(v)), m), [])
  | Rep e ->
      let l = (eval (env,m) e) in
      let a = fst l |> expect_nk |> Nka.autom in
      let () = Nka.rep a (Field.get_fields ()) |> Trace.to_string |> printf out "%s\n%!" in
      ((env, snd l), [])
  | For (v, i_0, i_n, cmd) ->
      let indexes = List.init (i_n - i_0 + 1) (fun i -> i_0 + i) in
      List.fold_left (fun ((env,m),res) i ->
        let env' = Env.bind_val env v (Env.Num(Value.of_int i)) in
        let ((env'',m''),res2) = interp out bn (env',m) cmd in
        ((env'',m''), res@res2)
      ) ((env,m),[]) indexes
