open Nerode_netkat

let f = Field.get_or_assign_fid "@f"
let set = Nk.modif f (Value.of_int 0)

let a1 = Nka.autom (Nk.seq_pair Nk.dup set)
  
let filter = Nk.filter true f (Value.of_int 1)
let a2 = Nka.autom (Nk.seq [filter; Nk.dup; set])
