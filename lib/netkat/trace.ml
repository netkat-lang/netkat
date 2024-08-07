
(* A trace is a list of packets with length >= 2 *)
type t = Pk.t list

let compare = List.compare Pk.compare

let eq t1 t2 = compare t1 t2 = 0

module Comp = struct
  type t = Pk.t list
  let compare = compare
end
module S = Set.Make(Comp)
module M = Map.Make(Comp)

let empty = []

let rec pairs (t: t) : PrTrace.t =
  match t with
  | []
  | [_] -> failwith "Invariant violated: regular traces must have at least two packets."
  | p1::p2::[] -> [Pkpair.mk p1 p2]
  | p1::p2::t -> (Pkpair.mk p1 p2)::(pairs (p2::t))

let of_pairs (pairs: PrTrace.t) : t =
  let rec match_pk pk lst partial =
    match lst with
    | [] -> List.rev (pk::partial)
    | pr::rem ->
        let pk',pk'' = Pkpair.split pr in
        if not (Pk.eq pk pk') then
          failwith "PrTrace has mismatched pairs"
        else match_pk pk'' rem (pk::partial) in
  match pairs with
  | [] -> []
  | pr::rem -> let pk,pk' = Pkpair.split pr in
               match_pk pk' rem [pk]

let to_string t =
  match t with
  | [] -> "ε"
  | _ -> List.map Pk.to_string t |> String.concat ";"

let rec suffixes t =
  match t with
  | []
  | [_] -> failwith "Invariant violated: regular traces must have at least two packets."
  | pk::pk'::[] -> [[pk;pk']]
  | pk::rem -> t::(suffixes rem)
      
let prefixes t = List.map List.rev (suffixes (List.rev t)) |> List.rev

(** Compute suffixes all the way down to 1-packet traces *)
let rec suffixes1 t =
  match t with
  | [] -> failwith "Invariant violation: 1-traces must have at least one packet."
  | [pk] -> [[pk]]
  | pk::pk'::[] -> [[pk']; [pk;pk']]
  | pk::rem -> t::(suffixes1 rem)

let prefixes1 t = List.map List.rev (suffixes1 (List.rev t)) |> List.rev

let hd = List.hd
let tl = List.tl
let lt t = List.rev t |> List.tl |> List.rev
let dh t = List.rev t |> List.hd

let length = List.length
