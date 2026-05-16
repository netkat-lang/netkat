open Interp
open Z3

(* printf out "%s\n%!" (eval env e |> expect_nk |> Deriv.e |> Spp.to_exp |> Nk.to_string_z3) *)

(*

let x = Arithmetic.Integer.mk_const_s ctx "x"
let y = Arithmetic.Integer.mk_const_s ctx "y"

let solver = Solver.mk_simple_solver ctx

let two = Arithmetic.Integer.mk_numeral_i ctx 2
let sum = Arithmetic.mk_add ctx [x; y]

let eq = Boolean.mk_eq ctx sum two

let () =
  Solver.add solver [eq];
  match Solver.check solver [] with
  | Solver.SATISFIABLE -> print_endline "sat"
  | Solver.UNSATISFIABLE -> print_endline "unsat"
  | Solver.UNKNOWN -> print_endline "unknown"

*)

let chop_suffix suffix s =
  if String.ends_with ~suffix s
  then (String.sub s 0 (String.length s - String.length suffix), true)
  else (s, false)

let interp_file out (fn: string) : (Env.t * result list) =
  (* convert list of results to set of traces *)
  let collect results = (
    List.fold_left (fun acc x -> match x with
    | Fail(Some(t)) -> Trace.S.add t acc
    | _ -> acc
    ) Trace.S.empty results
  ) in
  (* initial run of the input file *)
  let ienv = Env.bind_val Env.empty "hole" (Env.Expr(Nk.skip)) in
  let (env, results) = Interp.interp_file_with_env out ienv fn in
  let cr = collect results in
  if Trace.S.is_empty cr then (
    Printf.printf "SUCCESS\n";
    (env, results)
  ) else (
    match Env.lookup_val_opt env "hop" with
    | Some(Env.Expr(e)) ->
      (* "normalize" the hop expression by converting to an SPP and back to an expression *)
      let hop = Spp.to_exp (Deriv.e e) in
      Printf.printf "Using hop: %s\n" (Nk.to_string hop);
      (* collect all the field names in the hop *)
      let field_ids = Field.S.elements (Nk.get_fields hop) in
      let fields_temp = List.map Field.get_or_fail_fid field_ids in
      let fields = fields_temp@(List.map (fun f -> f^Nk.suffix) fields_temp) in
      List.iter (fun f -> Printf.printf ">> field: %s\n" f) fields;

      let cfg = [("model", "true")] in
      let ctx = mk_context cfg in
      let solver = Solver.mk_simple_solver ctx in

      let int_sort = Arithmetic.Integer.mk_sort ctx in
      let bool_sort = Boolean.mk_sort ctx in
      (* T : Int x ... x Int -> Bool *)
      let t_decl =
        FuncDecl.mk_func_decl
          ctx
          (Symbol.mk_string ctx "T")
          (List.map (fun _ -> int_sort) fields)
          bool_sort
      in

      (* create variables for the params *)
      let var_map = List.fold_left (fun acc name ->
         Nk.StringMap.add name (Expr.mk_const
           ctx
           (Symbol.mk_string ctx name)
           int_sort) acc
      ) Nk.StringMap.empty fields in
      let vars = List.map (fun f -> Nk.StringMap.find f var_map) fields in

      (* dump the hop to Z3 *)
      let body = Nk.to_z3 ctx var_map hop in

      (* build (T v1 v2 ... vn) *)
      let t_app =
        Expr.mk_app ctx t_decl vars
      in

      (* (= (T ...) body) *)
      let eq =
        Boolean.mk_eq ctx t_app body
      in

      (* (forall (...) (= (T ...) body)) *)
      let forall =
        Quantifier.expr_of_quantifier (
          Quantifier.mk_forall_const
            ctx
            vars
            eq
            None
            []
            []
            None
            None
        )
      in

      Solver.add solver [forall];
      Printf.printf "## FORALL:\n%s\n" (Expr.to_string forall);

      let rec loop count env failures num_filters = (
        Printf.printf "## CEGIS iteration %d ##\n" count;
        let h = Env.lookup_val env "hole" in (
        match h with
        | Expr(e) -> Printf.printf "## HOLE: %s\n%!" (Nk.to_string e);
        | _ -> Printf.printf "## NO HOLE\n%!"
        );
        let (env2, results) = Interp.interp_file_with_env out env fn in
        let fails = collect results in
        if Trace.S.is_empty fails then (
          (* TODO - return the synthesized filters *)
          let h = Env.lookup_val env2 "hole" in (
          match h with
          | Expr(e) -> Printf.printf "SYNTH: SUCCESS: %s\n%!" (Nk.to_string e);
          | _ -> Printf.printf "SYNTH: SUCCESS\n%!"
          );
          (env2,[])
        ) else (
          Printf.printf "SYNTH: FAIL\n%!";
          (* add the bad traces to seen *)
          let failures2 = Trace.S.union fails failures in
          (* dump the seen traces to Z3 *)
          Trace.S.iter (fun x ->
            Printf.printf "## Bad trace: %s\n%!" (Trace.to_string x);
            let rec pairs l acc = (match l with
            | p1::p2::more -> 
              Printf.printf "## packet 1: %s\n%!" (Pk.to_string p1);
              Printf.printf "## packet 2: %s\n%!" (Pk.to_string p2);
              if Pk.compare p1 p2 <> 0 then (
                let vs = List.map (fun f ->
                  let (f2,had_suffix) = chop_suffix Nk.suffix f in
                  let p = if had_suffix then p2 else p1 in
                  let fid = Field.get_or_assign_fid f2 in
                  let v = Field.M.find fid p in
                  Printf.printf "  ## field: %s -> %s (fid:%s, val:%s)\n%!" f f2 (Field.to_string fid) (Value.to_string v);
                  Arithmetic.Integer.mk_numeral_i ctx (Value.to_int v)
                ) fields in

                let t_app =
                  Expr.mk_app ctx t_decl vs
                in
                pairs (p2::more) (t_app::acc)
              ) else pairs (p2::more) acc
            | _ -> acc
            ) in
            let ps = pairs x []  in
            let a = Boolean.mk_and ctx ps in
            let n = Boolean.mk_not ctx a in
            Solver.add solver [n];
            Printf.printf "## AND:\n%s\n" (Expr.to_string n);
            ()
          ) failures2;
          (* TODO: dump hole to Z3 *)
          (* check sat Z3 *)
          (match Solver.check solver [] with
          | Solver.SATISFIABLE ->
            Printf.printf "## SAT\n";
            let model = Option.get (Solver.get_model solver) in
            print_endline (Model.to_string model);
            (* TODO: if SAT, need to extract the concrete filters from Z3 model *)
            (* TODO: put these filters in "hole" in the env and recurse *)
            (* TODO *)
            let env3 = Env.bind_val env2 "hole" (Env.Expr(Nk.drop)) in
            loop (count+1) env3 failures2 num_filters
          | _ ->
            (* if UNSAT, we need to increase max num filters *)
            loop (count+1) env2 failures2 (num_filters+1)
          )
        )
      ) in
      loop 1 env (collect results) 1
    | _ ->
      Printf.printf "ERROR: count not find \"hop\" expression in input file\n";
      (env,results)
  )
