(** Representation of a Netkat command *)

(** A value in a diversify field's [DExplicit] domain, e.g. the
    [Medical_Device] in "@dev=[Medical_Device,Provider_Host]" -- either a
    literal ([Dnum]) or a named constant ([Dvar], e.g. a prior
    "Medical_Device = 5" binding) not resolved until the simulate command
    actually runs, against whatever [Env.t] is current then. *)
type dvalue =
  | Dnum of Value.t
  | Dvar of string

(** How a simulate command's "{...}" list asks for one field to be
    enumerated -- the not-yet-resolved, syntax-level counterpart of
    [Nka.diversify_mode] (resolving [DExplicit]'s [dvalue list] against an
    [Env.t] is what turns this into an actual [Nka.diversify_mode]). *)
type dmode =
  | DBestEffort
  | DExhaustive
  | DExplicit of dvalue list

type t =
  | Import of string
  | Check of string option * bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Prints of string
  | Tikz of Nkexp.t
  | Let of string * Nkexp.t
  | VLet of string * Value.t
  | Rep of Nkexp.t
  | Simulate of string option * int option * (Field.t * dmode) list * Nkexp.t option * Nkexp.t
  | For of string * int * int * t

let dvalue_to_string = function
  | Dnum v -> Value.to_string v
  | Dvar s -> s

let diversify_field_to_string (f, m) =
  let fname = Field.get_or_fail_fid f in
  match m with
  | DBestEffort -> fname
  | DExhaustive -> fname ^ "=exhaustive"
  | DExplicit vs -> fname ^ "=[" ^ (String.concat "," (List.map dvalue_to_string vs)) ^ "]"

(** Pretty print the netkat expression. *)
let rec to_string t =
  match t with
  | Import s -> "import \"" ^ s ^ "\""
  | Check (tag, b, e1, e2) -> "check " ^ (match tag with None -> "" | Some s -> "\"" ^ s ^ "\" ") ^ (Nkexp.to_string e1) ^ (if b then "≡" else "≢") ^ (Nkexp.to_string e2)
  | Print e -> "print " ^ (Nkexp.to_string e)
  | Prints s -> "print \"" ^ s ^ "\""
  | Tikz e -> "tikz " ^ (Nkexp.to_string e)
  | Let (s, e) -> "let " ^ s ^ " = " ^ (Nkexp.to_string e)
  | VLet (s, v) -> "let " ^ s ^ " = " ^ (Value.to_string v)
  | Rep e -> "rep " ^ (Nkexp.to_string e)
  | Simulate (tag, mr, fs, pkt, e) ->
    "simulate " ^ (match tag with None -> "" | Some s -> "\"" ^ s ^ "\" ")
    ^ (match mr with None -> "" | Some n -> string_of_int n ^ " ")
    ^ (if fs = [] then "" else "{" ^ (String.concat ", " (List.map diversify_field_to_string fs)) ^ "} ")
    ^ (match pkt with None -> "" | Some p -> "[" ^ (Nkexp.to_string p) ^ "] ")
    ^ (Nkexp.to_string e)
  | For (v, i_0, i_n, cmd) -> Printf.sprintf "for %s ∈ %d..%d do %s" v i_0 i_n (to_string cmd)

let expect_val v = match v with
| Env.Num(v) -> v
| _ -> failwith (Printf.sprintf "unexpected closure: %s" "<closure>") (* TODO XXX - more contextual error message needed *)

(* [env]-driven, but never fails on an unresolvable/wrongly-shaped name:
   VFilter/VMod's own named-constant value is recorded as Value.S.empty
   (field known, value unknown) rather than raising, if [env] doesn't
   resolve it to a plain Env.Num -- callers that don't have (or don't
   care about) a real environment can safely pass Env.empty and still
   get every referenced field back, just with fewer known values. Var is
   resolved like Interp.eval's own Var case: a name bound to an already-
   evaluated Env.Expr recurses into it (via Nkexp.of_nk, so an evaluated
   Nk.t's fields are found the same way an un-evaluated Nkexp.t's are),
   one bound to an Env.Closure recurses into its captured body under its
   captured env (its own formal parameter is deliberately left
   unresolved -- see below, not extended into that env -- rather than
   simulating application) -- so any field a bare name like "net"
   actually stands for is found, not just the ones visible in a call
   site's own local syntax. An App's own formal parameter is likewise
   deliberately left unresolved when walking into a Closure's body
   (which surfaces as a harmless Var-not-found, contributing no fields
   for that one occurrence) rather than simulating substitution -- App's
   own case here already unions in the argument's fields directly
   (same as Diff/Xor), which is exactly the fields that formal parameter
   would have contributed wherever it's actually used in the body, so
   the overall result is the same total set either way. *)
let rec get_field_vals_from_exp (env:Env.t) (e:Nkexp.t) : Value.S.t Field.M.t =
match e with
| Drop | Skip | Dup -> Field.M.empty
| Filter(_,f,v) | Mod(f,v) -> Field.M.singleton f (Value.S.singleton v)
| VFilter(_,f,v) | VMod(f,v) ->
  let vals = match Env.lookup_val_opt env v with
    | Some (Env.Num v1) -> Value.S.singleton v1
    | Some (Env.Expr _ | Env.Closure _) | None -> Value.S.empty
  in
  Field.M.singleton f vals
| Seq(el)
| Union(el)
| Intersect(el) ->
  List.fold_left (fun acc e -> Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals_from_exp env e) acc) Field.M.empty el
| Diff(e1,e2)
| App(e1,e2)
| Xor(e1,e2) -> Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals_from_exp env e1) (get_field_vals_from_exp env e2)
| Star(e)
| Neg(e) -> get_field_vals_from_exp env e
| Fwd(po,e)
| Bwd(po,e) ->
  let po_vals = match po with None -> Field.M.empty | Some p -> get_field_vals_from_exp env p in
  Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) po_vals (get_field_vals_from_exp env e)
| Exists(f,e)
| Forall(f,e) -> Field.M.add f Value.S.empty (get_field_vals_from_exp env e) (* TODO - not sure if this is right *)
| Var(x) ->
  (match Env.lookup_val_opt env x with
   | Some (Env.Expr nk) -> get_field_vals_from_exp env (Nkexp.of_nk nk)
   | Some (Env.Closure (env',_,body)) -> get_field_vals_from_exp env' body
   | Some (Env.Num _) | None -> Field.M.empty)
| Num(_) -> Field.M.empty
| Lambda(_,e) -> get_field_vals_from_exp env e

let rec get_field_vals (env: Env.t) c : Value.S.t Field.M.t =
  match c with
  | Import(s) -> Field.M.empty
  | Check(_, _, e1, e2) ->
    Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals_from_exp env e1) (get_field_vals_from_exp env e2)
  | Print(e) -> get_field_vals_from_exp env e
  | Prints(_) -> Field.M.empty
  | Tikz(e) -> get_field_vals_from_exp env e
  | Let(_, e) -> get_field_vals_from_exp env e
  | VLet(_, v) -> Field.M.empty (* TODO - is this right? *)
  | Rep(e) -> get_field_vals_from_exp env e
  | Simulate(_, _, fs, pkt, e) ->
    let fs_vals = List.fold_left (fun acc (f,_) -> Field.M.add f Value.S.empty acc) Field.M.empty fs in
    let pkt_vals = match pkt with None -> Field.M.empty | Some p -> get_field_vals_from_exp env p in
    Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) fs_vals
      (Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) pkt_vals (get_field_vals_from_exp env e))
  | For(_, _, _, cmd) -> get_field_vals env cmd

let get_field_vals_from_cmds (env: Env.t) cl : Value.S.t Field.M.t =
  List.fold_left (fun acc c -> Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals env c) acc) Field.M.empty cl
