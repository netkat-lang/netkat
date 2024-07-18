open Nerode_netkat

let () =
  let a1 = Nka.autom Nk.dup in
  
  let f = Field.get_or_assign_fid "@f" in
  let filter = Nk.filter true f (Value.of_int 1) in
  let a2 = Nka.autom (Nk.seq_pair filter Nk.dup) in
  let () = Printf.printf "scratchpad\n%!" in
  Printf.printf "%s\n%!" @@ string_of_bool @@ Nka.bisim a1 a2
