include Set.Make(struct
  type t = Value.t * Value.t
  let compare (v1, v2) (v1', v2') =
    if Value.compare v1 v1' = 0 then
      Value.compare v2 v2'
    else
      Value.compare v1 v1'
end)

let cross (s1: Value.S.t) (s2: Value.S.t) :  t =
  Value.S.fold
    (fun v r ->
      let r' = Value.S.fold (fun v' r' -> add (v, v') r') s1 r in
      union r r') s2 empty
