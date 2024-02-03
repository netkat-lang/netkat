open Pk

module SMap = Map.Make(String)

type t = { valmap: value SMap.t; expmap: Nk.t SMap.t }

let empty = { valmap = SMap.empty; expmap = SMap.empty }

let bind_exp (t:t) (s:string) (e:Nk.t) = {t with expmap = SMap.add s e t.expmap}
let lookup_exp (t:t) (s:string) : Nk.t = match SMap.find_opt s t.expmap with
                                         | None -> failwith ("Undefined symbol: " ^ s)
                                         | Some e -> e

let bind_val (t:t) (s:string) (v: value) = {t with valmap = SMap.add s v t.valmap}
let lookup_val (t:t) (s:string) : value = match SMap.find_opt s t.valmap with
                                          | None -> failwith ("Undefined symbol: " ^ s)
                                          | Some e -> e
