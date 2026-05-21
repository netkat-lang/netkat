(* example usage: *)
(* dune exec netkat -- -s test-me.nkpl *)

open Interp
open Z3

module StringSet = Stdlib.Set.Make(String)

(* max number of filters for synthesis *)
let max_filters = 2 (* TODO *)

let omit_fields =
  List.fold_left (fun s x -> StringSet.add x s) StringSet.empty
  ["@dir";"@dev";"@if";"@srcip-0";"srcip-1";"@dstip-0";"@dstip-1"]

(* remove a suffix from a string *)
(* chop_suffix "__" "test__" --> ("test", true) *)
(* chop_suffix "__" "other"  --> ("other", false) *)
let chop_suffix suffix s =
  if String.ends_with ~suffix s
  then (String.sub s 0 (String.length s - String.length suffix), true)
  else (s, false)

let interp_file out (fn: string) : (Nkcmd.t list * Env.t * result list) =
  (* helper function: convert list of results to set of traces *)
  let collect results = (
    List.fold_left (fun (is_fail,acc) x -> match x with
    | Fail(Some(t)) -> (true, Trace.S.add t acc)
    | Fail(_) -> (true, acc)
    | _ -> (is_fail, acc)
    ) (false,Trace.S.empty) results
  ) in
  (* perform initial run of the input file, using hole=skip *)
  let ienv = Env.bind_val_access Env.empty "hole" (Env.Expr(Nk.skip)) Env.ReadOnly Env.Force in
  let (bn, (cmds, env, results)) = Interp.interp_file_with_env out ienv fn in
  let (is_fail,cr) = collect results in
  (* if the initial run was successful, we are done *)
  if not is_fail then (
    (* TODO: return the filter (skip in this case) *)
    Printf.printf "SUCCESS\n";
    (cmds, env, results)
  (* if the initial run was unsuccessful... *)
  ) else (
    (* grab the current "hop" expression -- this represents a single hop of the forwarding behavior *)
    match Env.lookup_val_opt env "hop" with
    | Some(Env.Expr(e)) ->
      (* "normalize" the hop expression by converting to an SPP and back to an expression *)
      let hop = Spp.to_exp (Deriv.e e) in
      Printf.printf "Using hop:\n%s\n" (Nk.to_string hop);

      (* collect all the field names in the hop *)
      (*let field_ids = Field.S.elements (Nk.get_fields hop) in*)
      (* collect all the field names in the entire input file *)
      let field_ids = Field.S.elements (Nkcmd.get_fields_from_cmds cmds) in
      let fields_temp,fields_all = List.fold_left (fun (acc1,acc2) fid ->
        let s = Field.get_or_fail_fid fid in
        if StringSet.mem s omit_fields then (acc1,(s::acc2))
        else ((s::acc1),(s::acc2))
      ) ([],[]) field_ids in
      let fields_temp,fields_all = (List.rev fields_temp, List.rev fields_all) in
      let fields = fields_all@(List.map (fun f -> f^Nk.suffix) fields_all) in
      List.iter (fun f -> Printf.printf "Using field: %s\n" f) fields;

      (* collect all the constant values from the hop *)
      (*let values = Value.S.elements (Nk.get_values hop) in*)
      (* collect all the constant values from the entire input file *)
      let values = Value.S.elements (Nkcmd.get_values_from_cmds cmds) in
      List.iter (fun v -> Printf.printf "Using value: %s\n" (Value.to_string v)) values;

      (* set up a Z3 solver *)
      let cfg = [("model", "true")] in
      let ctx = mk_context cfg in
      let solver = Solver.mk_simple_solver ctx in

      let int_sort = Arithmetic.Integer.mk_sort ctx in
      let bool_sort = Boolean.mk_sort ctx in

      (* this is for the transition relation *)
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

      let mk_filter_vars ctx n =
        let mk_bool name =
          Expr.mk_const
            ctx
            (Symbol.mk_string ctx (Printf.sprintf "filter%d-%s" n name))
            bool_sort
        in

        let mk_int name =
          Expr.mk_const
            ctx
            (Symbol.mk_string ctx (Printf.sprintf "filter%d-%s" n name))
            int_sort
        in

        (
          mk_bool "enable",
          mk_bool "type",
          mk_int "field",
          mk_int "val"
        )
      in

      let rec mk_field_ite i filter_type filter_field filter_val = function
        | [] ->
          Boolean.mk_not ctx filter_type

        | f :: rest ->
          let v = Nk.StringMap.find f var_map in

          Boolean.mk_ite
            ctx
            (Boolean.mk_eq ctx
               filter_field
               (Arithmetic.Integer.mk_numeral_i ctx i))
            (Boolean.mk_eq ctx v filter_val)
            (mk_field_ite (i + 1) filter_type filter_field filter_val rest)
      in

      let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
      let maxf = Arithmetic.Integer.mk_numeral_i ctx (List.length fields_temp) in

      (* these filters represent the "holes" *)
      let filters_data = List.fold_left (fun acc n ->
        let (filter_enable, filter_type, filter_field, filter_val) =
          mk_filter_vars ctx n in

        (* for a disabled filter, all the other components are set to zero *)
        let wf1 =
          Boolean.mk_implies ctx
            (Boolean.mk_not ctx filter_enable)
            (Boolean.mk_and ctx [
              Boolean.mk_not ctx filter_type;
              Boolean.mk_eq ctx filter_field zero;
              Boolean.mk_eq ctx filter_val zero;
            ])
        in

        (* for an enabled filter, it must designate a valid field *)
        let wf2 =
          Boolean.mk_implies ctx
            filter_enable
            (Boolean.mk_and ctx [
              Arithmetic.mk_ge ctx filter_field zero;
              Arithmetic.mk_lt ctx filter_field maxf;
            ])
        in

        (* the value of the filter must be constrained to the set of allowed values *)
        let wf3 = Boolean.mk_or ctx (List.map (fun v ->
          Boolean.mk_implies ctx
            filter_enable
            (Boolean.mk_eq ctx filter_val (Arithmetic.Integer.mk_numeral_i ctx (Value.to_int v)))
          ) values)
        in

        (* assert well-formedness constraints on the filters *)
        Solver.add solver [wf1; wf2; wf3];

        (* build the filter property, according to this basic example

          (ite filter1-enable (xor filter1-type
            (ite (= filter1-field 0) (= dev filter1-val)
            (ite (= filter1-field 1) (= if filter1-val)
            (ite (= filter1-field 2) (= dir filter1-val)
            (ite (= filter1-field 3) (= srcip0 filter1-val)
            (ite (= filter1-field 4) (= srcip1 filter1-val)
            (ite (= filter1-field 5) (= srcip2 filter1-val)
            (ite (= filter1-field 6) (= srcip3 filter1-val)
            (ite (= filter1-field 7) (= dstip0 filter1-val)
            (ite (= filter1-field 8) (= dstip1 filter1-val)
            (ite (= filter1-field 9) (= dstip2 filter1-val)
            (ite (= filter1-field 10) (= dstip3 filter1-val)
            (not filter1-type))))))))))))
          ) true)

        *)
        let filter =
          Boolean.mk_ite
            ctx
            filter_enable
            (Boolean.mk_xor ctx
               filter_type
               (mk_field_ite 0 filter_type filter_field filter_val fields_temp))
            (Boolean.mk_true ctx)
        in
        Printf.printf "Filter formula:\n%s\n" (Expr.to_string filter);
        (filter,filter_enable,filter_type,filter_field,filter_val)::acc
      ) [] (List.init max_filters (fun i -> i + 1)) in
      let filters = List.map (fun (f,_,_,_,_) -> f) filters_data in

      (* dump the hop and filter constraints to Z3 *)
      let hop_body = Nk.to_z3 ctx var_map hop in
      let body = Boolean.mk_and ctx (filters@[hop_body]) in

      (* build (T v1 v2 ... vn) *)
      let t_app =
        Expr.mk_app ctx t_decl vars
      in

      (* (= (T ...) body) *)
      let eq =
        Boolean.mk_eq ctx t_app body
      in

      (* this is the body of the transition relation *)
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

      (* assert the transition relation *)
      Solver.add solver [forall];
      (*Printf.printf "Forall formula:\n%s\n" (Expr.to_string forall);*)

      (* this is the main CEGIS loop *)
      (* TODO - num_filters is not being used correctly *)
      let rec loop count env failures num_filters = (
        Printf.printf "## CEGIS iteration %d ##\n" count;
        (* run the input file *)
        let (cmds, env2, results) = Interp.interp_cmds_with_env out env bn cmds in
        let (is_fail,fails) = collect results in
        (* if all the checks passed, we are done *)
        if not is_fail then (
          (* TODO - return the synthesized filters *)
          let h = Env.lookup_val env2 "hole" in (
          match h with
          | Expr(e) -> Printf.printf "SYNTH: SUCCESS: %s\n%!" (Nk.to_string e);
          | _ -> Printf.printf "SYNTH: SUCCESS\n%!"
          );
          (cmds,env2,[])
        (* if some check(s) failed... *)
        ) else (
          Printf.printf "SYNTH: FAIL\n%!";
          (* add the bad traces to seen - NOTE: we are currently not using past bad traces *)
          let failures2 = Trace.S.union fails failures in
          (* dump the seen traces to Z3 *)
          Trace.S.iter (fun x ->
            Printf.printf "Handling bad trace: %s\n%!" (Trace.to_string x);
            (* loop through the pairs of packets in the trace *)
            let rec pairs l acc = (match l with
            | p1::p2::more -> 
              (*Printf.printf "packet 1: %s\n%!" (Pk.to_string p1);
              Printf.printf "packet 2: %s\n%!" (Pk.to_string p2);*)
              (* note that we ignore identical packet pairs (stuttering transitions) *)
              if Pk.compare p1 p2 <> 0 then (
                (* loop through the fields *)
                let vs = List.map (fun f ->
                  let (f2,had_suffix) = chop_suffix Nk.suffix f in
                  (* if the field name designates the "next" version of that field, use p2, otherwise p1 *)
                  let p = if had_suffix then p2 else p1 in
                  let fid = Field.get_or_assign_fid f2 in
                  let v = Field.M.find fid p in
                  (*Printf.printf "  field: %s -> %s (fid:%s, val:%s)\n%!" f f2 (Field.to_string fid) (Value.to_string v);*)
                  Arithmetic.Integer.mk_numeral_i ctx (Value.to_int v)
                ) fields in

                (* build the application of the transition relation to this packet pair *)
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
            (* assert (not (and (T p1 p2) (T p2 p3) ...)) *)
            Solver.add solver [n];
            (*Printf.printf "Trace formula:\n%s\n" (Expr.to_string n);*)
            ()
          ) fails (*failures2*); (* NOTE - we are only adding the new failures *)

          (* check sat Z3 *)
          (match Solver.check solver [] with
          (* in the SAT case, we will extract the concrete filters from Z3 model, and iterate with these *)
          | Solver.SATISFIABLE ->
            Printf.printf "SAT\n";
            (* get the model from the solver *)
            let model = Option.get (Solver.get_model solver) in
            (*Printf.printf "MODEL:\n%s\n" (Model.to_string model);*)
            let get model e = Option.get (Model.eval model e true) in
            (* this deals with negative numbers like this "(- 1)" returned by Z3 *)
            let strip s = Str.global_replace (Str.regexp "[() ]") "" s in
            let get_int model e =
              let s = strip (Expr.to_string (Option.get (Model.eval model e true))) in
              try int_of_string s with _ -> failwith (Printf.sprintf "get_int: expected int: \"%s\"\n" s)
            in
            (* get the valuations from the Z3 model, and build the corresponding NetKAT filters *)
            let candidate_filters = List.map (fun (_,filter_enable,filter_type,filter_field,filter_val) ->
              let enable_v = Boolean.is_true (get model filter_enable) in
              let type_v = Boolean.is_true (get model filter_type) in
              let field_v = get_int model filter_field in
              let val_v = get_int model filter_val in
              let bl = Boolean.mk_and ctx [
                if enable_v
                then filter_enable
                else Boolean.mk_not ctx filter_enable;

                if type_v
                then filter_type
                else Boolean.mk_not ctx filter_type;

                Boolean.mk_eq ctx
                  filter_field
                  (Arithmetic.Integer.mk_numeral_i ctx field_v);

                Boolean.mk_eq ctx
                  filter_val
                  (Arithmetic.Integer.mk_numeral_i ctx val_v);
              ] in
              if enable_v then (
                try
                  let field_name = List.nth fields_temp field_v in
                  (*Printf.printf "Filter from model: enabled=%b, type=%b, field=%d (%s), val=%d\n%!" enable_v type_v field_v field_name val_v;*)
                  let e = Nk.Filter(not type_v,Field.get_or_assign_fid field_name,Value.of_int val_v) in
                  (*Printf.printf "Candidate NetKAT filter: %s\n" (Nk.to_string e);*)
                  (Some(e),bl)
                with _ -> (None,bl)
              ) else (None,bl)
            ) filters_data in

            Printf.printf "Candidate NetKAT filter: ";
            List.iter (fun (a,_) -> match a with
            | Some(e) ->
                  Printf.printf ", %s" (Nk.to_string e);
            | None -> ()) candidate_filters;
            Printf.printf "\n%!";

            (* this generates a "blocking clause" to prevent this specific set of filters from *)
            (* being generated again *)
            let bl = (List.map snd candidate_filters) in
            if not (List.is_empty bl) then (
              let block = Boolean.mk_not ctx (Boolean.mk_and ctx bl) in
              (*Printf.printf "Blocking clause formula:\n%s\n" (Expr.to_string block);*)
              Solver.add solver [block]
            );
            (* put these filters in "hole" in the env and recurse *)
            let env3 = Env.bind_val_access env2 "hole" (Env.Expr(Nk.Intersect(List.filter_map fst candidate_filters))) Env.ReadOnly Env.Force in
            loop (count+1) env3 failures2 num_filters
          | _ ->
            (* if UNSAT, this is a failure to generate a candidate *)
            (* TODO: couple of options here: (1) exit (failure), (2) increase num_filters and try again *)
            Printf.printf "SYNTH: UNSAT\n%!";
            (*loop (count+1) env2 failures2 (num_filters+1)*)
            (cmds,env2,[])
          )
        )
      ) in
      (* start the CEGIS loop *)
      loop 1 env cr 1
    | _ ->
      Printf.printf "ERROR: count not find \"hop\" expression in input file\n";
      (cmds,env,results)
  )
