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

let rec get_fields c : Field.S.t =
  match c with
  | Import(s) -> Field.S.empty
  | Check(_, e1, e2) -> Field.S.union (Nkexp.get_fields e1) (Nkexp.get_fields e2)
  | Print(e) -> Nkexp.get_fields e
  | Prints(_) -> Field.S.empty
  | Tikz(e) -> Nkexp.get_fields e
  | Let(_, e) -> Nkexp.get_fields e
  | VLet(_, v) -> Field.S.empty
  | Rep(e) -> Nkexp.get_fields e
  | For(_, _, _, cmd) -> get_fields cmd

let rec get_values c : Value.S.t =
  match c with
  | Import(s) -> Value.S.empty
  | Check(_, e1, e2) -> Value.S.union (Nkexp.get_values e1) (Nkexp.get_values e2)
  | Print(e) -> Nkexp.get_values e
  | Prints(_) -> Value.S.empty
  | Tikz(e) -> Nkexp.get_values e
  | Let(_, e) -> Nkexp.get_values e
  | VLet(_, v) -> Value.S.singleton v
  | Rep(e) -> Nkexp.get_values e
  | For(_, _, _, cmd) -> get_values cmd

let get_fields_from_cmds cl : Field.S.t =
  List.fold_left (fun acc c -> Field.S.union (get_fields c) acc) Field.S.empty cl

let get_values_from_cmds cl : Value.S.t =
  List.fold_left (fun acc c -> Value.S.union (get_values c) acc) Value.S.empty cl
