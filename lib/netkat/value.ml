module SMap = Map.Make(String)

module Env = struct
  type t = int SMap.t
  let compare = SMap.compare Int.compare
  let empty = SMap.empty
  let add = SMap.add
  let singleton = SMap.singleton
  let to_string (en:t) = SMap.fold (fun k v acc -> Printf.sprintf "%s, %s=%d" acc k v) en ""
end

module IntSet = Set.Make(struct
  type t = int
  let compare = Int.compare
end)

module EnvMap = Map.Make(Env)

let collecting_assignments = ref false
let temp_assignments = ((Hashtbl.create 10) : (string,IntSet.t) Hashtbl.t)
let assignments = ((Hashtbl.create 10) : (string,int option) Hashtbl.t)

let print_assignments () = Hashtbl.iter (fun k v ->
  Printf.printf "%s -> %s\n" k (match v with None -> "None" | Some(i) -> Printf.sprintf "Some(%d)" i)
) assignments

(*let _ = Hashtbl.add assignments "x" (Some(5))
let _ = Hashtbl.add assignments "y" (Some(3))*)

let start_collecting (en: Env.t) =
  Printf.printf "START COLLECTING\n";
  Hashtbl.clear assignments;
  Hashtbl.clear temp_assignments;
  collecting_assignments := true;
  SMap.iter (fun s i -> Hashtbl.replace assignments s (Some(i))) en

let stop_collecting () =
  Printf.printf "STOP COLLECTING\n";
  collecting_assignments := false;
  ()

let add_temp_assignment s i =
  let st = (
    match (Hashtbl.find_opt temp_assignments s) with
    | None -> IntSet.empty
    | Some(s) -> s
  ) in
  Hashtbl.replace temp_assignments s (IntSet.add i st)

(*let _ = add_temp_assignment "z" 123
let _ = add_temp_assignment "z" 456*)

let cartesian_product tbl =
  let bindings =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  in
  List.fold_left (fun acc (key, values) ->
    let with_values =
      IntSet.fold (fun v acc_inner ->
        List.fold_left (fun acc_inner2 partial ->
          ((key, v) :: partial) :: acc_inner2
        ) acc_inner acc
      ) values []
    in
    let with_null = acc in
    with_values @ with_null
  ) [ [] ] bindings

let get_temp_bindings () = cartesian_product temp_assignments


(* e.nkpl *)
(*let _ = Hashtbl.add temp (300,400) 0
let _ = Hashtbl.add temp (300,3) 0*)
(* d.nkpl *)

(*let _ = Hashtbl.add temp (400,300) 0
let _ = Hashtbl.add temp (300,3) 0
let _ = Hashtbl.add temp (400,3) 0*)

type int_or_var = Int of int | Metavar of string
type temp = int_or_var

module CustomInt = struct
  type t = int_or_var 

  let to_string i = match i with
  | Int(i) -> Printf.sprintf "Int(%d)" i
  | Metavar(s) -> Printf.sprintf "Metavar(%s)" s

  let compare_env use_env (e:Env.t) (a:t) (b:t) : int EnvMap.t =
  let result = (match (a,b) with
  | (Int(a),Int(b)) -> EnvMap.singleton e (Int.compare a b)
  | (Metavar(s1),Metavar(s2)) -> (
    if s1=s2 then EnvMap.singleton e 0 else
    match (Hashtbl.find_opt assignments s1, Hashtbl.find_opt assignments s2) with
    | (Some(v1),Some(v2)) -> EnvMap.singleton e (Option.compare Int.compare v1 v2)
    | _ -> EnvMap.singleton e (String.compare s1 s2)
  )
  | (Int(i),Metavar(s)) -> (
    match (Hashtbl.find_opt assignments s) with
    | Some(Some(v)) -> EnvMap.singleton e (Int.compare i v)
    (*| None -> EnvMap.add e (-1) (if use_env then EnvMap.singleton (SMap.add s i e) 0 else EnvMap.empty)*)
    | _ -> if !collecting_assignments then add_temp_assignment s i; EnvMap.singleton e (-1)
  )
  | (Metavar(s),Int(i)) -> (
    match (Hashtbl.find_opt assignments s) with
    | Some(Some(v)) -> EnvMap.singleton e (Int.compare v i)
    (*| None -> EnvMap.add e 1 (if use_env then EnvMap.singleton (SMap.add s i e) 0 else EnvMap.empty)*)
    | _ -> if !collecting_assignments then add_temp_assignment s i; EnvMap.singleton e 1
  )) in
  Printf.printf "Value.compare: %s ?= %s\n" (to_string a) (to_string b);
  result

  let compare (a:t) (b:t) =
  let result = match EnvMap.choose_opt (compare_env true SMap.empty a b) with
  | None -> failwith "CustomInt.compare: expected list of length > 0"
  | Some(_,i) -> i in
  if true(*!collecting_assignments*) then Printf.printf "Value.compare: %s <--> %s = %d\n" (to_string a) (to_string b) result;
  (*let stack = Printexc.get_callstack 20 in
  print_endline (Printexc.raw_backtrace_to_string stack);*)
  result

  (*let compare (a:t) (b:t) =
    let cmp one two = (
        let x = Hashtbl.find_opt temp (one,two) in
        match x with
        | None -> ((*if !binding_mode then let _ = Hashtbl.add temp (one,two) 0 in 0 else*) Int.compare a b)
        | Some(y) -> y 
    ) in
    let result = (
    if (a>=100 && b < 100) then (cmp a b)
    else if (b>=100 && a<100) then (cmp b a)
    else if (a>=100 && b>=100 && a<>b) then (
      let r = (Hashtbl.find_opt temp (a,b)) in
      match r with
      | None -> Int.compare a b
      | Some(r) -> r
    )
    else Int.compare a b) in
    if !binding_mode then Printf.printf "Value.compare: %d <--> %d = %d\n" a b result;
    result*)
end

type t = CustomInt.t

module M = struct
  include Map.Make(CustomInt)
  let fold_bdgs f a m = List.fold_left (fun accum (v,b) -> f accum v b) a (bindings m)
  (* TODO XXX -> this only subset compares in one direction *)
  let rec compare_env (en:Env.t) (cmp:(Env.t -> 'a -> 'a -> int EnvMap.t)) (m1:'a t) (m2:'a t) : int EnvMap.t =
    if is_empty m1 then EnvMap.singleton en 0
    else (
      let (k1, v1) = List.hd (bindings m1) in
      let m1' = remove k1 m1 in
      let temp = List.fold_left (fun acc2 (k2, v2) ->
        EnvMap.fold (fun en' _ acc' ->
          (* compare keys *)
          Printf.printf "    >> comparing: %s to %s\n" (CustomInt.to_string k1) (CustomInt.to_string k2);
          let result = CustomInt.compare_env true en' k1 k2 in
          EnvMap.fold (fun (k3:Env.t) (v3:int) acc3 ->
            if v3 = 0 then (
              let m2' = remove k2 m2 in
              (* compare values *)
              let res = cmp k3 v1 v2 in
              EnvMap.fold (fun (k4:Env.t) (v4:int) acc4 ->
                if v4=0 then (
                  let res_rest = compare_env k4 cmp m1' m2' in
                  EnvMap.union (fun k4 one two -> Some(two)) acc4 res_rest
                ) else acc4
              ) res acc3
            ) else acc3
          ) result acc'
        ) acc2 acc2
      ) (EnvMap.singleton en 0) (bindings m2) in
      if EnvMap.is_empty temp then EnvMap.singleton en 1 else temp
    )
end

module S = Set.Make(CustomInt)

let keys m =
  List.fold_left (fun s (v,_) -> S.add v s) S.empty (M.bindings m)
let union_keys ms =
  List.map keys ms |> List.fold_left S.union S.empty

let of_int i = Int(i)
let to_string x = match x with
| Int(i) -> string_of_int i
| Metavar(s) -> s
let of_string s = try Int(int_of_string s) with _ -> Metavar(s)

let compare = CustomInt.compare

let map_op_pair d op m1 m2 = M.merge(fun _ x y ->
    match x, y with
    | None, None -> None
    | None, Some y' -> Some (op d y')
    | Some x', None -> Some (op x' d)
    | Some x', Some y' -> Some (op x' y')) m1 m2

let map_op d op = List.fold_left (map_op_pair d op) M.empty

let right_join d (m1: 'a M.t) (m2: 'a M.t) = map_op_pair d (fun a b -> b) m1 m2
let left_join d (m1: 'a M.t) (m2: 'a M.t) = map_op_pair d (fun a b -> a) m1 m2

let val_outside (vs: S.t) = Int(0)(*S.fold (fun v a -> if v = a then (max a v) + 1 else a) vs 0*) (*TODO XXX*)
let choose = Int(0)

let compare_value = compare
