(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Let of string * Nkexp.t

let interp t =
  match t with
  | Import s -> failwith "TODO"
  | Check (b, e1, e2) -> if b = Nka.(bisim (autom e1) (autom e2)) then
                            Printf.printf "Check success.\n%!"
                         else
                            begin
                              Printf.printf "Expected ";
                              if b then Printf.printf "equal, got unequal\n%!"
                                   else Printf.printf "unequal, got equal\n%!"
                            end
  | Print e -> Printf.printf "%s\n%!" (Nkexp.to_string e)
  | Let (s, e) -> failwith "TODO"

(** Pretty print the netkat expression. *)
let to_string t =
  match t with
  | Import s -> "import \"" ^ s ^ "\""
  | Check (b, e1, e2) -> "check " ^ (Nkexp.to_string e1) ^ (if b then "≡" else "≢") ^ (Nkexp.to_string e2)
  | Print e -> "print " ^ (Nkexp.to_string e)
  | Let (s, e) -> "let " ^ s ^ " = " ^ (Nkexp.to_string e)
