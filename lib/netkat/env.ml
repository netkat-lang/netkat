
let env : (string, Nk.t) Hashtbl.t = Hashtbl.create 11

let bind (s:string) (e:Nk.t) = Hashtbl.add env s e
let lookup (s:string) : Nk.t = match Hashtbl.find_opt env s with
                                  | None -> failwith ("Undefined symbol: " ^ s)
                                  | Some e -> e
