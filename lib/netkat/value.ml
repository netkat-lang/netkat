let binding_mode = ref false
let temp = ((Hashtbl.create 100) : ((int*int),int) Hashtbl.t)
(* e.nkpl *)
(*let _ = Hashtbl.add temp (300,400) 0
let _ = Hashtbl.add temp (300,3) 0*)
(* d.nkpl *)
let _ = Hashtbl.add temp (300,3) 0
let _ = Hashtbl.add temp (400,4) 0


(*let _ = Hashtbl.add temp (400,300) 0
let _ = Hashtbl.add temp (300,3) 0
let _ = Hashtbl.add temp (400,3) 0*)

module CustomInt = struct
  type t = int

  let compare (a:t) (b:t) =
    let cmp one two = (
        let x = Hashtbl.find_opt temp (one,two) in
        match x with
        | None -> (if !binding_mode then let _ = Hashtbl.add temp (one,two) 0 in 0 else Int.compare a b)
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
    Printf.printf "Value.compare: %d <--> %d = %d\n" a b result;
    result
end

type t = CustomInt.t

module M = struct
  include Map.Make(CustomInt)
  let fold_bdgs f a m = List.fold_left (fun accum (v,b) -> f accum v b) a (bindings m)
end

module S = Set.Make(CustomInt)

let keys m =
  List.fold_left (fun s (v,_) -> S.add v s) S.empty (M.bindings m)
let union_keys ms =
  List.map keys ms |> List.fold_left S.union S.empty

let of_int = Fun.id
let to_string = string_of_int
let of_string = Fun.id (fun s -> int_of_string s)

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

let val_outside (vs: S.t) = S.fold (fun v a -> if v = a then (max a v) + 1 else a) vs 0
let choose = 0

let compare_value = compare
