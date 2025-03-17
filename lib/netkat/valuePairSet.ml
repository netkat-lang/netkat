(** Set of a pair of NetKAT values. *)

include Set.Make(struct
  type t = Value.t * Value.t
  let compare (v1, v2) (v1', v2') =
    if Value.compare v1 v1' = 0 then
      Value.compare v2 v2'
    else
      Value.compare v1 v1'
end)

(** [cross A X] is the cross product [A×B] of two value sets [A], [B]. *)
let cross (s1: Value.S.t) (s2: Value.S.t) :  t =
  Value.S.fold
    (fun v r ->
      let r' = Value.S.fold (fun v' r' -> add (v, v') r') s2 r in
      union r r') s1 empty
