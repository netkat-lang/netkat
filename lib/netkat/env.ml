open Pk

module SMap = Map.Make(String)

type t = nk_val SMap.t
and  nk_val = Num of value | Expr of Nk.t | Closure of t * string * Nkexp.t

let empty = SMap.empty

let bind_val (t:t) (s:string) (v:nk_val) = SMap.add s v t
let lookup_val (t:t) (s:string) : nk_val = match SMap.find_opt s t with
                                         | None -> failwith ("Undefined exp symbol: " ^ s)
                                         | Some e -> e

let rec nk_val_to_string (v:nk_val) = match v with
| Num(v) -> Printf.sprintf "Num(%s)" (Value.to_string v)
| Expr(e) -> Printf.sprintf "Expr(%s)" (Nk.to_string e)
| Closure(env,s,e) -> Printf.sprintf "Closure(%s, %s, %s)" (to_string env) s (Nkexp.to_string e)

and to_string (t:t) =
  Printf.sprintf "{%s}"
  (SMap.fold (fun k v acc -> Printf.sprintf "%s, %s->%s" acc k (nk_val_to_string v)) t "")
