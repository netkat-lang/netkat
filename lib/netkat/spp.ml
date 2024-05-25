open Pk
(** SPP implementation *)

type spp =
  | Skip
  | Drop
  | Union of
      field * spp ref Value.M.t Value.M.t * spp ref Value.M.t * spp ref * int

type t = spp ref

let get_hash = function
  | Skip -> Hashtbl.hash Skip
  | Drop -> Hashtbl.hash Drop
  | Union (_, _, _, _, x) -> x

let compare spp1ref spp2ref =
  let spp1, spp2 = (!spp1ref, !spp2ref) in
  let magic_compare spp1 spp2 =
    Stdlib.compare (get_hash !spp1) (get_hash !spp2)
  in
  match (spp1, spp2) with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union (f1, fms1, ms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
      if f1 < f2 then -1
      else if f2 < f1 then 1
      else
        let cmp_fms =
          Value.M.compare (Value.M.compare magic_compare) fms1 fms2
        in
        if cmp_fms < 0 then -1
        else if cmp_fms > 0 then 1
        else
          let cmp_ms = Value.M.compare magic_compare ms1 ms2 in
          if cmp_ms < 0 then -1
          else if cmp_ms > 0 then 1
          else magic_compare d1 d2

let init_hash (f, fms, ms, d) =
  let fms_v =
    Value.M.fold
      (fun v m acc ->
        Value.M.fold (fun x y acc -> Hashtbl.hash (x, get_hash !y, acc)) m 0)
      fms (get_hash Drop)
  in
  let ms_v =
    Value.M.fold
      (fun x y acc -> Hashtbl.hash (x, get_hash !y, acc))
      ms (get_hash Skip)
  in
  Hashtbl.hash (f, fms_v, ms_v, get_hash !d)

module SPPHashtbl = Hashtbl.Make (struct
  type t = spp

  let equal spp1 spp2 =
    match (spp1, spp2) with
    | Drop, Drop -> true
    | Drop, _ -> false
    | _, Drop -> false
    | Skip, Skip -> true
    | Skip, _ -> false
    | _, Skip -> false
    | Union (f1, fms1, ms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
        f1 = f2
        && Value.M.equal (fun m1 m2 -> Value.M.equal ( == ) m1 m2) fms1 fms2
        && Value.M.equal ( == ) ms1 ms2
        && d1 == d2

  let hash = get_hash
end)

let eq = ( == )
let pool_size = 64
let pool = SPPHashtbl.create pool_size

let fetch x =
  match SPPHashtbl.find_opt pool x with
  | Some refx -> refx
  | None ->
      let refx = ref x in
      SPPHashtbl.add pool x refx;
      refx

let skip = fetch Skip
let drop = fetch Drop
let map_op = Value.map_op drop
let map_op_pair = Value.map_op_pair drop
let right_join = Value.right_join drop

let dummy f d =
  fetch
    (Union
       ( f,
         Value.M.empty,
         Value.M.empty,
         d,
         init_hash (f, Value.M.empty, Value.M.empty, d) ))

(** [mk (f, fms, ms, d)] performs canonicalization to guarantee that only
    semantically equivalent SPPs have the same representation. *)
let mk (f, fms, ms, d) =
  (*Union (f, fms, ms, d)*)
  (* First remove filter/mod branches that are exactly [Drop] *)
  let fms' =
    Value.M.map
      (fun m ->
        Value.M.filter_map
          (fun _ spp -> match !spp with Drop -> None | _ -> Some spp)
          m)
      fms
  in
  let fms'' =
    List.fold_left
      (fun m (vi, sppi) ->
        if sppi == drop && not (Value.M.mem vi fms') then Value.M.remove vi m
        else m)
      fms' (Value.M.bindings ms)
  in
  let ms' = Value.M.filter (fun _ sppi -> not (sppi == drop)) ms in
  let fms''' =
    Value.M.filter
      (fun vi mi ->
        let drop_branch =
          if Value.M.mem vi ms' || d == drop then ms'
          else Value.M.add vi d ms'
        in
        not (Value.M.equal ( == ) mi drop_branch))
      fms''
  in
  if Value.M.is_empty fms''' && Value.M.is_empty ms' then d
  else fetch (Union (f, fms''', ms', d, init_hash (f, fms''', ms', d)))

let rec to_exp sppref =
  let spp = !sppref in
  match spp with
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f, fms, ms, d, _) ->
      let branches =
        Value.M.bindings fms
        |> List.map (fun (vi, m) ->
               let mods =
                 Value.M.bindings m
                 |> List.map (fun (vj, spp) ->
                        Nk.seq_pair (Nk.modif f vj) (to_exp spp))
                 |> Nk.union
               in
               Nk.seq_pair (Nk.filter true f vi) mods)
        |> Nk.union
      in
      let uneq =
        Value.M.bindings fms
        |> List.map (fun (vi, _) -> Nk.filter false f vi)
        |> Nk.seq
      in
      let mods =
        Value.M.bindings ms
        |> List.map (fun (vi, sppi) ->
               Nk.seq_pair (Nk.modif f vi) (to_exp sppi))
        |> Nk.union
      in
      let def =
        Nk.seq_pair
          (Value.M.bindings ms
          |> List.map (fun (vi, _) -> Nk.filter false f vi)
          |> Nk.seq)
          (to_exp d)
      in
      let defaults = Nk.seq_pair uneq (Nk.union_pair mods def) in
      Nk.union_pair branches defaults

let to_string t = to_exp t |> Nk.to_string

let rec of_sp sp =
  match !sp with
  | Sp.Skip -> skip
  | Sp.Drop -> drop
  | Sp.Union (f, vs, d, _) ->
      let vvs =
        List.fold_left
          (fun a (v, t) -> Value.M.add v (Value.M.singleton v (of_sp t)) a)
          Value.M.empty (Value.M.bindings vs)
      in
      mk (f, vvs, Value.M.empty, of_sp d)

let rec to_sp_bwd (sppref : t) : Sp.t =
  let spp = !sppref in
  match spp with
  | Skip -> Sp.skip
  | Drop -> Sp.drop
  | Union (f, fms, ms, d, _) ->
      let ms1 =
        Value.M.map
          (fun ms ->
            List.map (fun (_, sppi) -> to_sp_bwd sppi) (Value.M.bindings ms)
            |> Sp.union)
          fms
      in
      let y =
        List.map (fun (_, sppi) -> to_sp_bwd sppi) (Value.M.bindings ms)
        |> Sp.union
      in
      let ms2 =
        List.fold_left
          (fun m (v, sppi) ->
            match Value.M.find_opt v fms with
            | None -> Value.M.add v y m
            | Some _ -> m)
          Value.M.empty (Value.M.bindings ms)
      in
      Sp.mk (f, Value.right_join Sp.drop ms1 ms2, to_sp_bwd d)

let filter b f v =
  if b then
    mk
      (f, Value.M.singleton v (Value.M.singleton v skip), Value.M.empty, drop)
  else mk (f, Value.M.singleton v Value.M.empty, Value.M.empty, skip)

let modf f v = mk (f, Value.M.empty, Value.M.singleton v skip, drop)

(*
let rec to_exp = function
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f, fms, ms, d) ->
      let branches =
        Value.M.bindings !!!fms
        |> List.map (fun (vi, m) ->
               let mods =
                 Value.M.bindings m
                 |> List.map (fun (vj, spp) ->
                        Nk.seq_pair (Nk.modif f vj) (to_exp spp))
                 |> Nk.union
               in
               Nk.seq_pair (Nk.filter true f vi) mods)
        |> Nk.union
      in
      let uneq =
        Value.M.bindings fms
        |> List.map (fun (vi, _) -> Nk.filter false f vi)
        |> Nk.seq
      in
      let mods =
        Value.M.bindings !!ms
        |> List.map (fun (vi, sppi) ->
               Nk.seq_pair (Nk.modif f vi) (to_exp sppi))
        |> Nk.union
      in
      let def =
        Nk.seq_pair
          (Value.M.bindings ms
          |> List.map (fun (vi, _) -> Nk.filter false f vi)
          |> Nk.seq)
          (to_exp d)
      in
      let defaults = Nk.seq_pair uneq (Nk.union_pair mods def) in
      Nk.union_pair branches defaults
      *)

let to_string t = to_exp t |> Nk.to_string

let vm_to_string (m : Sp.t Value.M.t) : string =
  List.map
    (fun (vj, sp) -> Value.to_string vj ^ "↦" ^ Sp.to_string sp)
    (Value.M.bindings m)
  |> String.concat ", "

let vmpp_to_string (m : spp ref Value.M.t) : string =
  List.map
    (fun (vj, sp) -> Value.to_string vj ^ "↦" ^ to_string sp)
    (Value.M.bindings m)
  |> String.concat ", "

let vmm_to_string (m : spp ref Value.M.t Value.M.t) : string =
  List.map
    (fun (vi, mi) -> Value.to_string vi ^ "--->" ^ vmpp_to_string mi)
    (Value.M.bindings m)
  |> String.concat "; "

module Memo_op = struct
  module Memo1_tbl = Hashtbl.Make (struct
    type t = spp ref
    let equal = ( == )
    let hash x = get_hash !x
  end)

  module Memo2_tbl = Hashtbl.Make (struct
    type t = spp ref * spp ref 
    let equal (a, b) (c, d) = a == c && b == d
    let hash (x, y) = Hashtbl.hash (get_hash !x, get_hash !y)
  end)

  type opcode = Union | Seq | Diff | Star | Intersect

  let init_size = 64
  let main1 = Hashtbl.create init_size
  let main2_uncom = Hashtbl.create init_size
  let main2_com = Hashtbl.create init_size
  let add_op_1 opcode table = Hashtbl.add table opcode (Memo1_tbl.create init_size)
  let add_op_2 opcode table = Hashtbl.add table opcode (Memo2_tbl.create init_size)

  let memo1 (opcode : opcode) (f : (spp ref -> spp ref) -> spp ref -> spp ref)
      xref =
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

  let memo2_uncom (opcode : opcode)
      (f : (spp ref -> spp ref -> spp ref) -> spp ref -> spp ref -> spp ref)
      xref yref =
    let main = Hashtbl.find main2_uncom opcode in
    let rec f' refx refy =
      match Memo2_tbl.find_opt main (refx, refy) with
      | Some refz -> refz
      | None ->
          let refz = f f' refx refy in
          Memo2_tbl.add main (refx, refy) refz;
          refz
    in
    f' xref yref

  let memo2_com (opcode : opcode)
      (f : (spp ref -> spp ref -> spp ref) -> spp ref -> spp ref -> spp ref)
      xref yref =
    let main = Hashtbl.find main2_com opcode in
    let rec f' refx refy =
      match
        (Memo2_tbl.find_opt main (refx, refy), Memo2_tbl.find_opt main (refy, refx))
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
    add_op_2 Intersect main2_com;
    add_op_2 Seq main2_uncom;
    add_op_2 Diff main2_uncom;
    add_op_1 Star main1)

let get_branch (x : spp ref) (v : value) : spp ref Value.M.t =
  match !x with
  | Drop -> Value.M.empty
  | Skip -> Value.M.singleton v skip
  | Union (f, fms, ms, d, _) -> (
      match Value.M.find_opt v fms with
      | Some m -> m
      | None ->
          if Value.M.mem v ms || d == drop then ms else Value.M.add v d ms)

let union_pair =
  let union_pair self spp1ref spp2ref =
    let spp1, spp2 = (!spp1ref, !spp2ref) in
    match (spp1, spp2) with
    | Drop, _ -> spp2ref
    | _, Drop -> spp1ref
    | Skip, Skip -> skip
    | Skip, Union (f, _, _, _, _) -> self (dummy f skip) spp2ref
    | Union (f, _, _, _, _), Skip -> self spp1ref (dummy f skip)
    | Union (f1, fms1, ms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
        if f1 < f2 then self spp1ref (dummy f1 spp2ref)
        else if f1 > f2 then self (dummy f2 spp1ref) spp2ref
        else
          (* XXX As-yet unadapted section here:
             if (idL eq False) && (idR eq False) then
               if branchesL.isEmpty && branchesR.isEmpty then
                 if mutsR.size == 1 && !mutsL.contains(mutsR.head._1) then
                   // (1) If right is a single assignment only in the default test case
                   return TestMut.mk(xL, branchesL, mutsL.updated(mutsR.head._1, mutsR.head._2), idL)
                 else
                   // (2) Any number of assignments, again only in the default test case
                   return TestMut(xL, branchesL, unionMap(mutsL, mutsR), idL)
               if mutsL.isEmpty && branchesR.size == 1 && !branchesL.contains(branchesR.head._1) && mutsR.size == 0
                      // (3) Right side is only a single test?
                 then return TestMut.mk(xL, branchesL.updated(branchesR.head._1, branchesR.head._2), mutsL, idL)
          *)
          let keyset =
            Value.(S.union (union_keys [ fms1; fms2 ]) (union_keys [ ms1; ms2 ]))
          in
          let fms =
            Value.S.fold
              (fun v m ->
                let vm1 = get_branch spp1ref v in
                let vm2 = get_branch spp2ref v in
                let vm = map_op_pair self vm1 vm2 in
                Value.M.add v vm m)
              keyset Value.M.empty
          in
          let ms = map_op_pair self ms1 ms2 in
          let d = self d1 d2 in
          mk (f1, fms, ms, d)
  in
  Memo_op.memo2_com Union union_pair

let union = List.fold_left union_pair drop
let union_map_pair = map_op_pair union_pair
let union_maps = map_op union_pair

let seq_pair =
  let seq_pair self spp1ref spp2ref =
    let spp1, spp2 = (!spp1ref, !spp2ref) in
    match (spp1, spp2) with
    | Skip, _ -> spp2ref
    | _, Skip -> spp1ref
    | Drop, _ | _, Drop -> drop
    | Union (f1, fms1, ms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
        if f1 < f2 then self spp1ref (dummy f1 spp2ref)
        else if f2 < f1 then self (dummy f2 spp1ref) spp2ref
        else
          let matchup (vi, sppi) =
            get_branch spp2ref vi |> Value.M.bindings
            |> List.fold_left
                 (fun m (vj, sppj) -> Value.M.add vj (self sppi sppj) m)
                 Value.M.empty
          in
          let msA = union_maps (List.map matchup (Value.M.bindings ms1)) in
          let keyset =
            Value.(S.union
              (union_keys [ fms1; fms2 ])
              (union_keys [ ms1; ms2; msA ]))
          in
          let fms =
            Value.S.fold
              (fun v m ->
                let spp_v = get_branch spp1ref v |> Value.M.bindings in
                let res =
                  List.map
                    (fun (vi, sppi) ->
                      let spp_v_i =
                        get_branch spp2ref vi |> Value.M.bindings
                      in
                      List.fold_left
                        (fun mi (vj, sppj) ->
                          Value.M.add vj (self sppi sppj) mi)
                        Value.M.empty spp_v_i)
                    spp_v
                in
                Value.M.add v (union_maps res) m)
              keyset Value.M.empty
          in
          let msB =
            List.fold_left
              (fun m (v, spp) -> Value.M.add v (self d1 spp) m)
              Value.M.empty (Value.M.bindings ms2)
          in
          mk (f1, fms, union_map_pair msA msB, self d1 d2)
  in
  Memo_op.memo2_uncom Seq seq_pair

let seq = List.fold_left seq_pair skip

let intersect_pair =
  let intersect_pair self spp1ref spp2ref =
    let spp1, spp2 = (!spp1ref, !spp2ref) in
    match (spp1, spp2) with
    | Drop, _ | _, Drop -> drop
    | Skip, Skip -> skip
    | Skip, Union (f, fms, ms, d, _) -> self (dummy f skip) spp2ref
    | Union (f, fms, ms, d, _), Skip -> self spp1ref (dummy f skip)
    | Union (f1, fms1, ms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
        if f1 < f2 then self spp1ref (dummy f1 spp2ref)
        else if f2 < f1 then self (dummy f2 spp1ref) spp2ref
        else
          let keyset =
            Value.(S.union (union_keys [ fms1; fms2 ]) (union_keys [ ms1; ms2 ]))
          in
          let fms =
            Value.S.fold
              (fun v m ->
                Value.M.add v
                  (map_op_pair self (get_branch spp1ref v)
                     (get_branch spp2ref v))
                  m)
              keyset Value.M.empty
          in
          let ms = map_op_pair self ms1 ms2 in
          let d = self d1 d2 in
          mk (f1, fms, ms, d)
  in

  Memo_op.memo2_com Intersect intersect_pair

let intersect spps =
  match spps with
  | [] -> drop
  | [ x ] -> x
  | x :: ys -> List.fold_left intersect_pair x ys

let diff =
  let diff self spp1ref spp2ref =
    let spp1, spp2 = (!spp1ref, !spp2ref) in
    match (spp1, spp2) with
    | Skip, Skip | Drop, _ -> drop
    | _, Drop -> spp1ref
    | Skip, Union (f, fms, ms, d, _) -> self (dummy f skip) spp2ref
    | Union (f, fms, ms, d, _), Skip -> self spp1ref (dummy f skip)
    | Union (f1, fms1, ms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
        if f1 < f2 then self spp1ref (dummy f1 spp2ref)
        else if f2 < f1 then self (dummy f2 spp1ref) spp2ref
        else
          let keyset =
            Value.(S.union (union_keys [ fms1; fms2 ]) (union_keys [ ms1; ms2 ]))
          in
          let fms =
            Value.S.fold
              (fun v m ->
                let vm =
                  map_op_pair self (get_branch spp1ref v) (get_branch spp2ref v)
                in
                Value.M.add v vm m)
              keyset Value.M.empty
          in
          let ms = map_op_pair self ms1 ms2 in
          let d = self d1 d2 in
          mk (f1, fms, ms, d)
  in
  Memo_op.memo2_uncom Diff diff

let xor spp1 spp2 = union_pair (diff spp1 spp2) (diff spp2 spp1)

(* TODO Optimize this somehow? *)
let star =
  let star_ref self spp =
    let spp' = union [ skip; spp; seq_pair spp spp ] in
    if spp == spp' then spp else self spp'
  in
  Memo_op.memo1 Star star_ref

let rec push (spref : Sp.t) (sppref : t) =
  let sp, spp = (!spref, !sppref) in
  match (sp, spp) with
  | Sp.Drop, _ | _, Drop -> Sp.drop
  | _, Skip -> spref
  | Sp.Skip, Union (f, fms, ms, d, _) ->
      let thru_fms =
        List.map
          (fun (v, vms) -> Value.M.mapi (fun v spp -> push Sp.skip spp) vms)
          (Value.M.bindings fms)
        |> Value.map_op Sp.drop Sp.union_pair
      in
      let thru_ms = Value.M.mapi (fun v spp -> push Sp.skip spp) ms in
      let thru_both = Value.map_op_pair Sp.drop Sp.union_pair thru_fms thru_ms in
      let matched_values = Value.S.union (Value.keys fms) (Value.keys ms) in

      (* Next we need to identify the right slice of [Skip] to push through [d],
         i.e. whatever doesn't match [fms] or [ms]. *)
      let branchesD, unmatched_vs =
        Value.S.fold
          (fun v (m, uvs) ->
            match Value.M.find_opt v thru_both with
            | None -> (Value.M.add v Sp.drop m, v :: uvs)
            | Some _ -> (m, uvs))
          matched_values (thru_both, [])
      in
      let push_default = push Sp.skip d in
      let diffkeys =
        Value.S.diff (Value.keys branchesD) matched_values |> Value.S.elements
      in
      let branchesE =
        List.fold_left
          (fun m v ->
            let sp_cur = Value.M.find v branchesD in
            Value.M.add v (Sp.union_pair sp_cur push_default) m)
          branchesD diffkeys
      in
      Sp.mk (f, branchesE, push_default)
  | Union (f1, fms1, d1, _), Union (f2, fms2, ms2, d2, _) ->
      if f1 < f2 then
        let fms = Value.M.mapi (fun v spi -> push spi sppref) fms1 in
        Sp.mk (f1, fms, push d1 sppref)
      else if f2 < f1 then
        let fmsA =
          List.map
            (fun (_, muts) ->
              let tsts = Value.M.mapi (fun v sppi -> push spref sppi) muts in
              Sp.mk (f2, tsts, Sp.drop))
            (Value.M.bindings fms2)
          |> Sp.union
        in
        let ms =
          Value.right_join Sp.drop
            (Value.M.map (Fun.const Sp.drop) fms2)
            (Value.M.map (fun sppi -> push spref sppi) ms2)
        in
        let fmsB = Sp.mk (f2, ms, push spref d2) in
        Sp.union_pair fmsA fmsB
      else
        (* f1 = f2 *)
        (* XXX Do we need the comparison to drop case? if so it goes here. *)
        let pkA =
          List.map
            (fun (v, spi) ->
              match Value.M.find_opt v fms2 with
              | None -> (
                  let tsts = Value.M.map (fun sppj -> push spi sppj) ms2 in
                  match Value.M.find_opt v ms2 with
                  | None ->
                      let tsts' = Value.M.add v (push spi d2) tsts in
                      Sp.mk (f1, tsts', Sp.drop)
                  | Some sppj -> Sp.mk (f1, tsts, Sp.drop))
              | Some m ->
                  let tsts = Value.M.map (fun sppj -> push spi sppj) m in
                  Sp.mk (f1, tsts, Sp.drop))
            (Value.M.bindings fms1)
          |> Sp.union
        in
        let pkB =
          List.map
            (fun (v, sppi) ->
              if Value.M.mem v fms1 then Sp.drop
              else
                let tsts = Value.M.map (fun sppj -> push d1 sppj) sppi in
                Sp.mk (f1, tsts, Sp.drop))
            (Value.M.bindings fms2)
          |> Sp.union
        in
        let ms =
          Value.M.map (Fun.const Sp.drop) fms1
          |> Value.left_join Sp.drop (Value.M.map (Fun.const Sp.drop) fms2)
          |> Value.left_join Sp.drop (Value.M.map (fun sppi -> push d1 sppi) ms2)
        in
        let pkC = Sp.mk (f1, ms, push d1 d2) in
        Sp.union [ pkA; pkB; pkC ]

let pull (spp : t) (sp : Sp.t) = seq_pair spp (of_sp sp) |> to_sp_bwd

let mem (spp: t) (pp: Pkpair.t) : bool = failwith "TODO: convert to refs"
  (*
  let rec memrec spp bdgs =
    match spp, bdgs with
    | Drop, _ -> false
    | Skip, [] -> true
    | Skip, (f, (v0,v1))::bdgs' ->
        if v0 != v1 then false else memrec Skip bdgs'
    | Union (f,_,_,_), [] -> failwith ("(a) Packet is missing field [" ^ (Field.get_or_fail_fid f) ^ "]!%!")
    | Union (f, b, m, d), (f', (v0,v1))::bdgs' ->
        if Field.compare f f' < 0 then failwith ("(b) Packet is missing field [" ^ (Field.get_or_fail_fid f) ^ "]!%!") else
        if Field.compare f f' > 0 then memrec spp bdgs' else
        match Value.M.find_opt v0 b with
        | Some bm -> begin
                     match Value.M.find_opt v1 bm with
                     | Some spp' -> memrec spp' bdgs'
                     | None -> false
                     end
        | None ->
            match Value.M.find_opt v1 m with
            | Some spp' -> memrec spp' bdgs' (* XXX Suspect this is not quite right*)
            | None -> memrec d bdgs' (* XXX: Need to look at v0 ...*)
  in memrec spp (Value.air.to_list pp)
  *)

(** Give a pair of packets that are in the semantics of a given spp [t] *)
let rep (spp: t) (fields: Field.S.t) : Pkpair.t = failwith "TODO: convert to refs"
  (*
  let fresh_const s f m =
    let v = Value.val_outside s in
    Value.air.addf f (v,v) m in
  let rec repr (p: t) (fs: Field.S.t) (partial: Value.air.t) =
    if fs = Field.S.empty then partial else
    match p with
      | Drop -> failwith "Can't take representative of empty SPP!"
      | Skip -> Field.S.fold (fresh_const Value.S.empty) fs partial
      | Union (f, b, m, d) ->
          let mub = Value.(S.union (keys m) (keys b)) in
          let nextf = Field.S.min_elt fs in
          let fs' = Field.S.remove f fs in
          if nextf < f then
            let fs'' = Field.S.remove nextf fs in
            repr p fs'' (fresh_const Value.S.empty nextf partial)
          else if not (eq d Drop) then
            (*
            let () = Printf.printf "picking val outside...\n" in
            let () = Value.S.iter (fun v -> Printf.printf "%s\n" (Value.to_string v)) mub in
            *)
            repr d fs' (fresh_const mub f partial)
          else if m != Value.M.empty then
            let v0 = Value.val_outside mub in
            let (v1, q) = Value.M.choose m in (* [q] can't be Drop since [p] is canonical *)
            repr q fs' (Value.air.addf f (v0, v1) partial)
          else (* find one in b *)
            let (v0, bm) = List.find (fun (v, m) -> m != Value.M.empty) (Value.M.bindings b) in
            let (v1, q) = Value.M.choose bm in (* [q] can't be Drop since [p] is canonical *)
            repr q fs' (Value.air.addf f (v0, v1) partial)
    in repr spp fields Value.air.empty
    *)
