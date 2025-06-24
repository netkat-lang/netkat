(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
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
  | Tikz e -> "tikz " ^ (Nkexp.to_string e)
  | Let (s, e) -> "let " ^ s ^ " = " ^ (Nkexp.to_string e)
  | VLet (s, v) -> "let " ^ s ^ " = " ^ (Value.to_string v)
  | Rep e -> "rep " ^ (Nkexp.to_string e)
  | For (v, i_0, i_n, cmd) -> Printf.sprintf "for %s ∈ %d..%d do %s" v i_0 i_n (to_string cmd)
