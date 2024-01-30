
let env : (string, Nkexp.t) Hashtbl.t = Hashtbl.create 11

let bind (s:string) (e:Nkexp.t) = Hashtbl.add env s e
let lookup (s:string) : Nkexp.t = match Hashtbl.find_opt env s with
                                  | None -> failwith ("Undefined symbol: " ^ s)
                                  | Some e -> e
