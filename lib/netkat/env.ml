open Pk

type t = { valmap: value Value.SMap.t; expmap: Nk.t Value.SMap.t }

let empty = { valmap = Value.SMap.empty; expmap = Value.SMap.empty }

let bind_exp (t:t) (s:string) (e:Nk.t) = { t with expmap = Value.SMap.add s e t.expmap }
let lookup_exp (t:t) (s:string) : Nk.t = match Value.SMap.find_opt s t.expmap with
                                         | None -> failwith ("Undefined symbol: " ^ s)
                                         | Some e -> e

let bind_val (t:t) (s:string) (v: value) = { t with valmap = Value.SMap.add s v t.valmap }
let lookup_val (t:t) (s:string) : value = match Value.SMap.find_opt s t.valmap with
                                          | None -> failwith ("Undefined symbol: " ^ s)
                                          | Some e -> e
