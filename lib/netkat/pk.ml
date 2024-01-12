
type field = int
type value = int

module FieldMap = Map.Make(Int)
module ValueMap = Map.Make(Int)

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

