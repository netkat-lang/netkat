
type field = int
type value = int

module FieldSet = Set.Make(Int)
module FieldMap = Map.Make(Int)

module ValueMap = struct
  include Map.Make(Int)
  let fold_bdgs f a m = List.fold_left (fun accum (v,b) -> f accum v b) a (bindings m)
end

module ValueSet = Set.Make(Int)

let cmp_field = Int.compare
let cmp_value = Int.compare

(* Symbol table for field names *)
let fields : (string, field) Hashtbl.t = Hashtbl.create 11
let field_labels : (field, string) Hashtbl.t = Hashtbl.create 11

let value_of_int = Fun.id
let string_of_val = string_of_int

let get_or_assign_fid (f: string) : field =
  match Hashtbl.find_opt fields f with
  | Some n -> n
  | None -> let n = Hashtbl.length fields in
            let () = Hashtbl.add fields f n in
            let () = Hashtbl.add field_labels n f in
            n

let get_or_fail_fid (n: field) : string =
  match Hashtbl.find_opt field_labels n with
  | Some f -> f
  | None -> failwith ("unknown field index: " ^ string_of_int n)

let map_op_pair d op m1 m2 = ValueMap.merge(fun _ x y ->
    match x, y with
    | None, None -> None
    | None, Some y' -> Some (op d y')
    | Some x', None -> Some (op x' d)
    | Some x', Some y' -> Some (op x' y')) m1 m2

let map_op d op = List.fold_left (map_op_pair d op) ValueMap.empty

let right_join d (m1: 'a ValueMap.t) (m2: 'a ValueMap.t) = map_op_pair d (fun a b -> b) m1 m2
let left_join d (m1: 'a ValueMap.t) (m2: 'a ValueMap.t) = map_op_pair d (fun a b -> a) m1 m2
(* XXX: Doesn't compile?
    left_join = Fun.flip right_join *)

let keys m =
  List.fold_left (fun s (v,_) -> ValueSet.add v s) ValueSet.empty (ValueMap.bindings m)

let union_keys ms =
  List.map keys ms |> List.fold_left ValueSet.union ValueSet.empty


(* The remaining type t and funtions deal with concrete packets. *)

type t = value FieldMap.t
