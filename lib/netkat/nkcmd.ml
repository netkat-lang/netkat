(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Let of string * Nkexp.t

let interp t =
  match t with
  | Import s -> failwith ("TODO" ^ __LOC__)
  | Check (b, e1, e2) -> let a1 = Nka.autom e1 in
                         (* let () = Printf.printf "Autom a1:\n%s\n-----\n%!" (Nka.to_string a1) in *)
                         let a2 = Nka.autom e2 in
                         (* let () = Printf.printf "Autom a2:\n%s\n-----\n%!" (Nka.to_string a2) in *)
                         let sgn = if b then "≡" else "≢" in
                         if b = Nka.bisim a1 a2 then
                           Printf.printf "*** Check \u{001b}[32mSUCCESS!\u{001b}[0m *** (%s %s %s)\n%!"
                            (Nkexp.to_string e1) sgn (Nkexp.to_string e2)
                         else
                            begin
                              Printf.printf "XXX Check \u{001b}[31mFAILED.\u{001b}[0m XXX (expected: %s %s %s)\n%!"
                                (Nkexp.to_string e1) sgn (Nkexp.to_string e2);
                              exit 1
                            end
  | Print e -> Printf.printf "%s\n%!" (Nkexp.to_string e)
  | Let (s, e) -> failwith ("TODO" ^ __LOC__)

(** Pretty print the netkat expression. *)
let to_string t =
  match t with
  | Import s -> "import \"" ^ s ^ "\""
  | Check (b, e1, e2) -> "check " ^ (Nkexp.to_string e1) ^ (if b then "≡" else "≢") ^ (Nkexp.to_string e2)
  | Print e -> "print " ^ (Nkexp.to_string e)
  | Let (s, e) -> "let " ^ s ^ " = " ^ (Nkexp.to_string e)
