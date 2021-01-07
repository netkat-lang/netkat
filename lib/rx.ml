open Alphabet
open Core_kernel

module MakeRx (A : Alphabet) : sig
    type t =
      | Empty 
      | Epsilon
      | Char of A.symbol
      | Seq of t list
      | Union of t list
      | Star of t
    val compare : t -> t -> int

    (* [equiv r] decides if the two regexs are equivalent *)
    val equiv : t -> t -> bool

    val seq : t list -> t
    val union : t list -> t
    val union_pair : t -> t -> t
    val star : t -> t

end = struct
    
    type t = 
      | Empty 
      | Epsilon
      | Char of A.symbol
      | Seq of t list
      | Union of t list
      | Star of t

    let rec compare (t1:t) (t2:t) = 
      match t1,t2 with 
      | Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Epsilon, Epsilon -> 0
      | Epsilon, _ -> -1
      | _, Epsilon -> 1
      | Char s1, Char s2 -> A.compare s1 s2                      
      | Char _, _ -> -1
      | _, Char _ -> 1
      | Seq lst1, Seq lst2 -> List.compare compare lst1 lst2
      (*
          begin
            match lst1, lst2 with 
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | elt1::tail1, elt2::tail2 -> let result = compare elt1 elt2  in
              if result = 0 then compare (seq tail1) (seq tail2) else result 
          end
          *)
      | Seq _, _ -> -1
      | _, Seq _ -> 1
      | Union t1, Union t2 -> List.compare compare t1 t2
      | Union _, _ -> -1
      | _, Union _ -> 1
      | Star t11, Star t21 -> compare t11 t21

    and seq (lst1:t list) = 
      let lst = List.filter ~f:(fun x -> not (equiv x Epsilon)) lst1 in
      match lst with
      | [] -> Epsilon
      | _  -> if List.exists ~f:(fun x -> equiv x Empty) lst then Empty else Seq lst

    (* Syntactic equivalence with reordering of union *)
    and equiv (r1:t) (r2:t) = ((compare r1 r2) = 0)

    let union (lst:t list) : t =
      Union (List.filter ~f:(fun x -> not (equiv x Empty)) lst)

    let union_pair (r1:t) (r2:t) : t =
      match r1,r2 with 
      | Empty, _ -> r2
      | _, Empty -> r1
      | Union t1, Union t2 -> Union (t1 @ t2)
      | Union t1, _ -> if List.exists ~f:(fun x -> equiv x r2) t1 then r1 else Union (r2::t1)
      | _, Union t2 -> if List.exists ~f:(fun x -> equiv x r1) t2 then r2 else Union (r1::t2)
      | _, _ -> 
         let cmp = compare r1 r2 in
         if cmp < 0 then Union [r1;r2]
         else Union [r2;r1]

    let star (r0:t) : t =
      match r0 with
      | Epsilon | Empty -> Epsilon
      | Star _ -> r0
      | _ -> Star r0
end
(*
and S (A: Alphabet) : Set.S with type elt = MakeRx(A).t = Set.Make(MakeRx(A))
*)
