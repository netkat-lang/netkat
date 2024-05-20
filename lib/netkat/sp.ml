(** SP implementation *)

open Pk

type t = Skip | Drop | Union of field * t Value.M.t * t
type spt = t

let rec to_exp = function
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f, vm, d) ->
      let tsts =
        Nk.union
          (List.map
             (fun (v, t') ->
               let tst = Nk.filter true f v in
               let next = to_exp t' in
               Nk.seq_pair tst next)
             (Value.M.bindings vm))
      in
      let ntsts =
        Nk.seq
          (List.map (fun (v, _) -> Nk.filter false f v) (Value.M.bindings vm))
      in
      Nk.union_pair tsts (Nk.seq_pair ntsts (to_exp d))

let to_string t = to_exp t |> Nk.to_string

let rec compare sp1 sp2 =
  match (sp1, sp2) with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union (f1, fs1, d1), Union (f2, fs2, d2) ->
      if f1 < f2 then -1
      else if f2 < f1 then 1
      else
        let cmp_maps = Value.M.compare compare fs1 fs2 in
        if cmp_maps = 0 then compare d1 d2 else cmp_maps

module SPHashtbl = Hashtbl.Make (struct
  type t = spt

  let equal spp1 spp2 = compare spp1 spp2 = 0
  let hash sp = sp |> to_string |> Hashtbl.hash
end)

let pool = SPHashtbl.create 8

let fetch x =
  match SPHashtbl.find_opt pool x with
  | Some refx -> refx
  | None ->
      let refx = ref x in
      SPHashtbl.add pool x refx;
      refx

module Memo = struct
  let rec hash sp1 =
    match sp1 with
    | Drop -> 0
    | Skip -> 1
    | Union (f, fs, d) ->
        Hashtbl.hash
          ( f,
            Value.M.fold (fun k d acc -> (acc * Hashtbl.hash k) + hash d) fs 0,
            hash d )

  module SPHashtbl = Hashtbl.Make (struct
    type t = spt

    let equal sp1 sp2 = compare sp1 sp2 = 0
    let hash = hash
  end)

  type opcode = Union | Seq | Neg

  let main1 = Hashtbl.create 8
  let main2 = Hashtbl.create 8
  let add_op opcode table = Hashtbl.add table opcode (Hashtbl.create 8)

  let memo1 (opcode : opcode) (f : (t -> t) -> t -> t) x =
    let main = Hashtbl.find main1 opcode in
    let rec f' x =
      let refx = fetch x in
      match Hashtbl.find_opt main refx with
      | Some z -> !z
      | None ->
          let z = f f' x in
          let refz = fetch z in
          Hashtbl.add main refx refz;
          !refz
    in
    f' !(fetch x)

  let memo2 (opcode : opcode) (f : (t -> t -> t) -> t -> t -> t) x y =
    let main = Hashtbl.find main2 opcode in
    let rec f' x y =
      let refx, refy = (fetch x, fetch y) in
      match Hashtbl.find_opt main (refx, refy) with
      | Some z -> !z
      | None ->
          let z = f f' x y in
          let refz = fetch z in
          Hashtbl.add main (refx, refy) refz;
          !refz
    in
    f' !(fetch x) !(fetch y)
end

let skip = !(fetch Skip)
let drop = !(fetch Drop)

let eq = ( == )

let () =
  Memo.(
    add_op Union main2;
    add_op Seq main2;
    add_op Neg main1)

let mk (f, m, d) =
  let m' =
    List.fold_left
      (fun m (vi, spi) -> if spi == d then m else Value.M.add vi spi m)
      Value.M.empty (Value.M.bindings m)
  in
  if Value.M.cardinal m' = 0 then !(fetch d) else !(fetch (Union (f, m', d)))

let union_pair =
  let union_pair self (t1 : t) (t2 : t) : t =
    match (t1, t2) with
    | Skip, _ | _, Skip -> Skip
    | Drop, _ -> t2
    | _, Drop -> t1
    | Union (f1, vm1, d1), Union (f2, vm2, d2) ->
        if Field.compare f1 f2 = 0 then
          mk
            ( f1,
              Value.M.merge
                (fun v p1o p2o ->
                  match (p1o, p2o) with
                  | None, None -> None
                  | Some p1, None -> Some (self p1 d2)
                  | None, Some p2 -> Some (self d1 p2)
                  | Some p1, Some p2 -> Some (self p1 p2))
                vm1 vm2,
              self d1 d2 )
        else if Field.compare f1 f2 < 0 then
          mk (f1, Value.M.map (fun p -> self p t2) vm1, self d1 t2)
        else self t2 t1
  in
  Memo.memo2 Union union_pair

let union = List.fold_left union_pair Drop

let le sp1 sp2 =
  match (sp1, sp2) with
  | Drop, _ | _, Skip -> true
  | _, Drop | Skip, _ -> false (* because _, Skip is already marked true *)
  | _, _ -> sp2 == (union_pair sp1 sp2)

let seq_pair =
  let seq_pair self (t1 : t) (t2 : t) : t =
    match (t1, t2) with
    | Drop, _ | _, Drop -> Drop
    | Skip, _ -> t2
    | _, Skip -> t1
    | Union (f1, vm1, d1), Union (f2, vm2, d2) ->
        if Field.compare f1 f2 = 0 then
          mk
            ( f1,
              Value.M.merge
                (fun v p1o p2o ->
                  match (p1o, p2o) with
                  | None, None -> None
                  | Some p1, None -> Some (self p1 d2)
                  | None, Some p2 -> Some (self d1 p2)
                  | Some p1, Some p2 -> Some (self p1 p2))
                vm1 vm2,
              self d1 d2 )
        else if Field.compare f1 f2 < 0 then
          mk (f1, Value.M.map (fun p -> self p t2) vm1, self d1 t2)
        else self t2 t1
  in
  Memo.memo2 Seq seq_pair

let seq = List.fold_left seq_pair Skip
let intersect_pair = seq_pair
let intersect = seq
let star _ = Skip

let neg =
  let neg self e =
    match e with
    | Skip -> Drop
    | Drop -> Skip
    | Union (f, vm, d) -> mk (f, Value.M.map self vm, self d)
  in
  Memo.memo1 Neg neg

let diff t1 t2 = intersect_pair t1 (neg t2)

let rec to_exp = function
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f, vm, d) ->
      let tsts =
        Nk.union
          (List.map
             (fun (v, t') ->
               let tst = Nk.filter true f v in
               let next = to_exp t' in
               Nk.seq_pair tst next)
             (Value.M.bindings vm))
      in
      let ntsts =
        Nk.seq
          (List.map (fun (v, _) -> Nk.filter false f v) (Value.M.bindings vm))
      in
      Nk.union_pair tsts (Nk.seq_pair ntsts (to_exp d))

let to_string t = to_exp t |> Nk.to_string

let xor t1 t2 = union_pair (diff t1 t2) (diff t2 t1)
