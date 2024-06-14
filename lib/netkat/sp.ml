(** SP implementation *)

open Pk

type sp = Skip | Drop | Union of field * sp ref Value.M.t * sp ref * int
type t = sp ref

let get_hash = function
  | Skip -> Hashtbl.hash Skip
  | Drop -> Hashtbl.hash Drop
  | Union (_, _, _, x) -> x

let deref_fms fms = Value.M.map (fun m -> Value.M.map (fun x -> !x) m) fms
let deref_ms = Value.M.map (fun x -> !x)
let ( !!! ) = deref_fms
let ( !! ) = deref_ms

let compare_inner sp1 sp2 =
  let magic_compare sp1 sp2 = Stdlib.compare (get_hash !sp1) (get_hash !sp2) in
  match (sp1, sp2) with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union (f1, fs1, d1, _), Union (f2, fs2, d2, _) ->
      if f1 < f2 then -1
      else if f2 < f1 then 1
      else
        let cmp_maps = Value.M.compare magic_compare fs1 fs2 in
        if cmp_maps = 0 then magic_compare d1 d2 else cmp_maps

let compare sp1 sp2 = compare_inner !sp1 !sp2

let init_hash (f, fs, d) =
  let fs_v =
    Value.M.fold (fun k m acc -> Hashtbl.hash (k, get_hash !m, acc)) fs 0
  in
  Hashtbl.hash (f, fs_v, d)

module SPHashtbl = Hashtbl.Make (struct
  type t = sp

  let equal spp1 spp2 =
    match (spp1, spp2) with
    | Drop, Drop -> true
    | Drop, _ -> false
    | _, Drop -> false
    | Skip, Skip -> true
    | Skip, _ -> false
    | _, Skip -> false
    | Union (f1, fs1, d1, _), Union (f2, fs2, d2, _) ->
        f1 = f2 && Value.M.equal ( == ) fs1 fs2 && d1 == d2

  let hash = get_hash
end)

let eq = ( == )
let pool_size = 64
let pool = SPHashtbl.create pool_size

let fetch x =
  match SPHashtbl.find_opt pool x with
  | Some refx -> refx
  | None ->
      let refx = ref x in
      SPHashtbl.add pool x refx;
      refx

let skip = fetch Skip
let drop = fetch Drop

let mk (f, m, d) =
  let m' =
    List.fold_left
      (fun m (vi, spi) -> if eq spi d then m else Value.M.add vi spi m)
      Value.M.empty (Value.M.bindings m)
  in
  if Value.M.cardinal m' = 0 then d
  else fetch (Union (f, m', d, init_hash (f, m', d)))

module Memo_op = struct
  module Memo1_tbl = Hashtbl.Make (struct
    type t = sp ref

    let equal = ( == )
    let hash x = get_hash !x
  end)

  module Memo2_tbl = Hashtbl.Make (struct
    type t = sp ref * sp ref

    let equal (a, b) (c, d) = a == c && b == d
    let hash (x, y) = Hashtbl.hash (get_hash !x, get_hash !y)
  end)

  type opcode = Union | Seq | Neg

  let init_size = 64
  let main1 = Hashtbl.create init_size
  let main2_uncom = Hashtbl.create init_size
  let main2_com = Hashtbl.create init_size

  let add_op_1 opcode table =
    Hashtbl.add table opcode (Memo1_tbl.create init_size)

  let add_op_2 opcode table =
    Hashtbl.add table opcode (Memo2_tbl.create init_size)

  let memo1 (opcode : opcode) (f : (sp ref -> sp ref) -> sp ref -> sp ref) xref
      =
    let main = Hashtbl.find main1 opcode in
    let rec f' refx =
      match Memo1_tbl.find_opt main refx with
      | Some z -> z
      | None ->
          let refz = f f' refx in
          Memo1_tbl.add main refx refz;
          refz
    in
    f' xref

  let memo2_com (opcode : opcode)
      (f : (sp ref -> sp ref -> sp ref) -> sp ref -> sp ref -> sp ref) xref yref
      =
    let main = Hashtbl.find main2_com opcode in
    let rec f' refx refy =
      match
        ( Memo2_tbl.find_opt main (refx, refy),
          Memo2_tbl.find_opt main (refy, refx) )
      with
      | Some refz, Some _ | Some refz, None | None, Some refz -> refz
      | None, None ->
          let refz = f f' refx refy in
          Memo2_tbl.add main (refx, refy) refz;
          refz
    in
    f' xref yref
end

let () =
  Memo_op.(
    add_op_2 Union main2_com;
    add_op_2 Seq main2_com;
    add_op_1 Neg main1)

let union_pair =
  let union_pair self t1ref t2ref =
    let t1, t2 = (!t1ref, !t2ref) in
    match (t1, t2) with
    | Skip, _ | _, Skip -> skip
    | Drop, _ -> t2ref
    | _, Drop -> t1ref
    | Union (f1, vm1, d1, _), Union (f2, vm2, d2, _) ->
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
          mk (f1, Value.M.map (fun p -> self p t2ref) vm1, self d1 t2ref)
        else self t2ref t1ref
  in
  Memo_op.memo2_com Union union_pair

let union = List.fold_left union_pair drop

let le sp1ref sp2ref =
  let sp1, sp2 = (!sp1ref, !sp2ref) in
  match (sp1, sp2) with
  | Drop, _ | _, Skip -> true
  | _, Drop | Skip, _ -> false (* because _, Skip is already marked true *)
  | _, _ -> eq sp2ref (union_pair sp1ref sp2ref)

let seq_pair =
  let seq_pair self t1ref t2ref =
    let t1, t2 = (!t1ref, !t2ref) in
    match (t1, t2) with
    | Drop, _ | _, Drop -> drop
    | Skip, _ -> t2ref
    | _, Skip -> t1ref
    | Union (f1, vm1, d1, _), Union (f2, vm2, d2, _) ->
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
          mk (f1, Value.M.map (fun p -> self p t2ref) vm1, self d1 t2ref)
        else self t2ref t1ref
  in
  Memo_op.memo2_com Seq seq_pair

let seq = List.fold_left seq_pair skip
let intersect_pair = seq_pair
let intersect = seq
let star _ = skip

let neg =
  let neg self eref =
    let e = !eref in
    match e with
    | Skip -> drop
    | Drop -> skip
    | Union (f, vm, d, _) -> mk (f, Value.M.map self vm, self d)
  in
  Memo_op.memo1 Neg neg

let diff t1 t2 = intersect_pair t1 (neg t2)
let xor t1 t2 = union_pair (diff t1 t2) (diff t2 t1)

let rec to_exp_inner = function
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f, vm, d, _) ->
      let tsts =
        Nk.union
          (List.map
             (fun (v, t') ->
               let tst = Nk.filter true f v in
               let next = to_exp_inner t' in
               Nk.seq_pair tst next)
             (Value.M.bindings !!vm))
      in
      let ntsts =
        Nk.seq
          (List.map (fun (v, _) -> Nk.filter false f v) (Value.M.bindings vm))
      in
      Nk.union_pair tsts (Nk.seq_pair ntsts (to_exp_inner !d))

let rep (spref : t) (fields : Field.S.t) : Pk.t =
  let fillin = Field.S.fold (fun f a -> match Field.M.find_opt f a with
                                        | None -> Field.M.add f Value.choose a
                                        | Some _ -> a) fields in
  let rec r (sp: t) (partial: Pk.t) =
      match !sp with
      | Skip -> fillin partial
      | Drop -> failwith "Cannot take representative of Sp.Drop!"
      | Union (f, vm, d, _) ->
          if not (eq d drop) then
            r d (Field.M.add f (Value.val_outside (Value.keys vm)) partial)
          else
            let v, sp' = List.find (fun (v, p) -> not (eq p drop)) (Value.M.bindings vm) in
            r sp' (Field.M.add f v partial) in
  r spref Field.M.empty

let rec of_pk (pk: Pk.t) =
  if Field.M.is_empty pk then
    skip
  else
    let f,v = Field.M.min_binding pk in
    mk (f, Value.M.singleton v (of_pk (Field.M.remove f pk)), drop)

let to_exp sp = to_exp_inner !sp
let to_string t = to_exp t |> Nk.to_string
