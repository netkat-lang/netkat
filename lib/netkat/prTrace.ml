type t = Pkpair.t list
let compare = List.compare Pkpair.compare
let eq t1 t2 = compare t1 t2 = 0

let concat t1 t2 : t option =
  match (List.rev t1), t2 with
  | [], _ -> Some t2
  | _, [] -> Some t1
  | (pp::_), (pp'::_) ->
      let _,pk = Pkpair.split pp in
      let pk',_ = Pkpair.split pp' in
      if Pk.eq pk pk' then
        Some (t1 @ t2)
      else
        None

let (@) = concat

(** Return suffixes down to size 1 *)
let rec suffixes1 t =
  match t with
  | [] -> failwith "Cannot take suffixes1 of []"
  | [pp] -> [t]
  | _::rem -> t::(suffixes1 rem)

(** Return suffixes down to empty list *)
let rec suffixes t =
  match t with
  | [] -> [t]
  | _::rem -> t::(suffixes rem)
      
(** Return prefixes down to empty list *)
let prefixes t =
  List.map List.rev (suffixes (List.rev t)) |> List.rev

let unsnoc (t:t) : (t * Pkpair.t) option =
  match List.rev t with
  | [] -> None
  | pp::rem -> Some (List.rev rem, pp)
  
(* Return the list of (pk,pre,suf) tuples resulting from splitting this trace at
   each position. Note prefixes have min length 0, suffixes have min length 1.
   The prefixes are either empty or end in pk, and the suffixes all start with pk.
   The triples are in ascending order for the prefixes. *)
let splits (t: t) : ((Pk.t * t * t) list) =
  let rec splits_r pre suf partial =
    match suf with
    | [] -> failwith "Unreachable! Suffixes have at least 1 pair."
    | [pp] ->
        let a,_ = Pkpair.split pp in
        (a,pre,suf)::partial
    | pp::rem ->
        let a,_ = Pkpair.split pp in
        let pre' = match pre@[pp] with
                   | None -> failwith "Impossible packet mismatch"
                   | Some res -> res in
        splits_r pre' rem ((a,pre,suf)::partial)
  in 
  splits_r [] t [] |> List.rev
