
(* A trace is a list of packets with length >= 2 *)
type t = Pk.t list

let rec pairs (t: t) : Pkpair.t list =
  match t with
  | []
  | [_] -> failwith "Invariant violated: traces must have at least two packets."
  | p1::p2::[] -> [Pkpair.mk p1 p2]
  | p1::p2::t -> (Pkpair.mk p1 p2)::(pairs (p2::t))

let to_string t = List.map Pk.to_string t |> String.concat ";"
