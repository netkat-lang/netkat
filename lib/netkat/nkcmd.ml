(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Prints of string
  | Tikz of Nkexp.t
  | Let of string * Nkexp.t
  | VLet of string * Value.t
  | Rep of Nkexp.t
  | For of string * int * int * t

(** Pretty print the netkat expression. *)
let rec to_string t =
  match t with
  | Import s -> "import \"" ^ s ^ "\""
  | Check (b, e1, e2) -> "check " ^ (Nkexp.to_string e1) ^ (if b then "≡" else "≢") ^ (Nkexp.to_string e2)
  | Print e -> "print " ^ (Nkexp.to_string e)
  | Prints s -> "print \"" ^ s ^ "\""
  | Tikz e -> "tikz " ^ (Nkexp.to_string e)
  | Let (s, e) -> "let " ^ s ^ " = " ^ (Nkexp.to_string e)
  | VLet (s, v) -> "let " ^ s ^ " = " ^ (Value.to_string v)
  | Rep e -> "rep " ^ (Nkexp.to_string e)
  | For (v, i_0, i_n, cmd) -> Printf.sprintf "for %s ∈ %d..%d do %s" v i_0 i_n (to_string cmd)

let expect_val v = match v with
| Env.Num(v) -> v
| _ -> failwith (Printf.sprintf "unexpected closure: %s" "<closure>") (* TODO XXX - more contextual error message needed *)

let rec get_field_vals_from_exp (env:Env.t) (e:Nkexp.t) : Value.S.t Field.M.t =
Printf.printf "Nkexp.get_field_vals_from_exp env: %s\n" (Nkexp.to_string e);
match e with
| Drop | Skip | Dup -> Field.M.empty
| Filter(_,f,v) | Mod(f,v) ->
  Printf.printf ">> Collecting: %s -> %s\n" (Field.to_string f) (Value.to_string v);
  Field.M.singleton f (Value.S.singleton v)
| VFilter(_,f,v) | VMod(f,v) ->
  let v1 = (Env.lookup_val env v |> expect_val) in
  Printf.printf "## Vfilter: %s\n" (Value.to_string v1);
  Field.M.singleton f (Value.S.singleton v1)
| Seq(el)
| Union(el)
| Intersect(el) ->
  List.fold_left (fun acc e -> Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals_from_exp env e) acc) Field.M.empty el
| Diff(e1,e2)
| App(e1,e2)
| Xor(e1,e2) -> Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals_from_exp env e1) (get_field_vals_from_exp env e2)
| Star(e)
| Neg(e)
| Fwd(_,e)
| Bwd(_,e) -> get_field_vals_from_exp env e
| Exists(f,e)
| Forall(f,e) -> Field.M.add f Value.S.empty (get_field_vals_from_exp env e) (* TODO - not sure if this is right *)
| Var(_)
| Num(_) -> Field.M.empty
| Lambda(_,e) -> get_field_vals_from_exp env e

let rec get_field_vals (env: Env.t) c : Value.S.t Field.M.t =
  match c with
  | Import(s) -> Field.M.empty
  | Check(_, e1, e2) ->
    Printf.printf "Nkcmd.get_field_vals: check\n";
    Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals_from_exp env e1) (get_field_vals_from_exp env e2)
  | Print(e) -> get_field_vals_from_exp env e
  | Prints(_) -> Field.M.empty
  | Tikz(e) -> get_field_vals_from_exp env e
  | Let(_, e) -> get_field_vals_from_exp env e
  | VLet(_, v) -> Field.M.empty (* TODO - is this right? *)
  | Rep(e) -> get_field_vals_from_exp env e
  | For(_, _, _, cmd) -> get_field_vals env cmd

let get_field_vals_from_cmds (env: Env.t) cl : Value.S.t Field.M.t =
  List.fold_left (fun acc c -> Field.M.union (fun k v1 v2 -> Some(Value.S.union v1 v2)) (get_field_vals env c) acc) Field.M.empty cl
