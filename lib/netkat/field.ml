type t = int

module S = Set.Make(Int)
module M = Map.Make(Int)

let keys m =
  List.fold_left (fun s (v,_) -> S.add v s) S.empty (M.bindings m)

let compare = Int.compare

(* Symbol table for field names *)
let fields : (string, t) Hashtbl.t = Hashtbl.create 11
let field_labels : (t, string) Hashtbl.t = Hashtbl.create 11

let get_fields () =
  Hashtbl.to_seq_keys field_labels |> S.of_seq

let get_or_assign_fid (f: string) : t =
  match Hashtbl.find_opt fields f with
  | Some n -> n
  | None -> let n = Hashtbl.length fields in
            let () = Hashtbl.add fields f n in
            let () = Hashtbl.add field_labels n f in
            n

let get_or_fail_fid (n: t) : string =
  match Hashtbl.find_opt field_labels n with
  | Some f -> f
  | None -> failwith ("unknown field index: " ^ string_of_int n)
