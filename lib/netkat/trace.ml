
(* A trace is a list of packets with length >= 2 *)
type t = Pk.t list

module Comp = struct
  type t = Pk.t list
  let compare = List.compare Pk.compare
end
module S = Set.Make(Comp)
module M = Map.Make(Comp)

let empty = []

let rec pairs (t: t) : Pkpair.t list =
  match t with
  | []
  | [_] -> failwith "Invariant violated: traces must have at least two packets."
  | p1::p2::[] -> [Pkpair.mk p1 p2]
  | p1::p2::t -> (Pkpair.mk p1 p2)::(pairs (p2::t))

let to_string t = List.map Pk.to_string t |> String.concat ";"


let rec suffixes t =
  match t with
  | [] -> S.singleton []
  | pk::rem -> S.add t (suffixes rem)
      
let prefixes t = S.map List.rev (suffixes (List.rev t))
