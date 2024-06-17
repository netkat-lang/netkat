type field = Field.t
type value = Value.t

(* The remaining type t and funtions deal with concrete packets. *)

type t = value Field.M.t

let compare = Field.M.compare Value.compare

let to_string p =
  let bdgs = Field.M.bindings p
           |> List.map (fun (f,v) -> (Field.get_or_fail_fid f) ^ "=" ^ (Value.to_string v)) in
  let hdr = String.concat "," bdgs in
  "[" ^ hdr ^ "]"
