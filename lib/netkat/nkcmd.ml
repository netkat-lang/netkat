(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Let of string * Nkexp.t
  | VLet of string * Pk.value

(** Pretty print the netkat expression. *)
let to_string t =
  match t with
  | Import s -> "import \"" ^ s ^ "\""
  | Check (b, e1, e2) -> "check " ^ (Nkexp.to_string e1) ^ (if b then "≡" else "≢") ^ (Nkexp.to_string e2)
  | Print e -> "print " ^ (Nkexp.to_string e)
  | Let (s, e) -> "let " ^ s ^ " = " ^ (Nkexp.to_string e)
  | VLet (s, v) -> "let " ^ s ^ " = " ^ (Pk.string_of_val v)
