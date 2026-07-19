open Pk

module SMap = Map.Make(String)

type access = ReadOnly | ReadWrite
type action = Normal | Force
type t = (nk_val * access) SMap.t
and  nk_val = Num of value | Expr of Nk.t | Closure of t * string * Nkexp.t

let empty = SMap.empty

let bind_val_access (t:t) (s:string) (v:nk_val) (ac:access) (a:action) =
  match (SMap.find_opt s t, a) with
  | (None, _)
  | (Some(_,ReadWrite), _)
  | (Some(_,ReadOnly), Force) -> SMap.add s (v,ac) t
  | (Some(_,ReadOnly), Normal) -> t

let bind_val (t:t) (s:string) (v:nk_val) =
  bind_val_access t s v ReadWrite Normal

let lookup_val_opt (t:t) (s:string) : nk_val option =
  match (SMap.find_opt s t) with
  | None -> None
  | Some(v,_) -> Some(v)

let lookup_val (t:t) (s:string) : nk_val = match lookup_val_opt t s with
                                         | None -> failwith ("Undefined exp symbol: " ^ s)
                                         | Some e -> e

let rec nk_val_to_string (v:nk_val) = match v with
| Num(v) -> Printf.sprintf "Num(%s)" (Value.to_string v)
| Expr(e) -> Printf.sprintf "Expr(%s)" (Nk.to_string e)
| Closure(env,s,e) -> Printf.sprintf "Closure(%s, %s, %s)" (to_string env) s (Nkexp.to_string e)

and to_string (t:t) =
  Printf.sprintf "{%s}"
  (SMap.fold (fun k (v,_) acc -> Printf.sprintf "%s, %s->%s" acc k (nk_val_to_string v)) t "")

