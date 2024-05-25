(* From the paper, a netkat automaton consists of:
States A set of states 𝑄.
Initial state A state 𝑞0 ∈ 𝑄.
Transitions A function 𝛿 : 𝑄 × 𝑄 → SPP.
Output A function 𝜖 : 𝑄 → SPP
*)

type state = Nk.t

module StatePair = struct
  type t = Nk.t * Nk.t
  let compare (a,b) (c,d) = if Nk.eq a c then Nk.compare b d else Nk.compare a c 
end

module StateSet = Set.Make(Nk)
module PairMap = Map.Make(StatePair)
module StateMap = Map.Make(Nk)

type t = {
  states: StateSet.t;
  start: state;
  trans: Sts.t StateMap.t;
  obs: Spp.t StateMap.t;
}


let to_string (a: t) =
  let ebinding_to_string ((e,p): Nk.t * Spp.t) =
    (Nk.to_string e) ^ "↦(" ^ (Spp.to_string p) ^ ")" in
  "States: " ^ (StateSet.elements a.states |> List.map Nk.to_string |> String.concat ", ") ^
  "\nStart: " ^ (Nk.to_string a.start) ^
  "\nTrans:\n  " ^ (StateMap.bindings a.trans
  |> List.map (fun (e,sts) -> "  " ^ (Nk.to_string e) ^ "↦" ^ Sts.to_string sts
  ) |> String.concat "  \n  ") ^
  "\nObs:\n  " ^ (StateMap.bindings a.obs |> List.map ebinding_to_string  |>
  String.concat "  \n  ") ^ "\n\n"

let autom (e: Nk.t) : t =
  let rec loop (q: Nk.t list) (visited: StateSet.t) tr ob =
    match q with
    | [] -> {
      states = visited;
      start = e;
      trans = tr;
      obs = ob;
    }
    | e0::rem -> if StateSet.mem e0 visited then
                   loop rem visited tr ob
                 else
                   let vis = StateSet.add e0 visited in
                   let sts = Deriv.d e0 in
                   let ob' = StateMap.add e0 (Deriv.e e0) ob in
                   let tr' = StateMap.add e0 sts tr in
                   let q' = Sts.to_list sts |> List.map (fun (e,_) -> e) in
                   loop (q'@rem) vis tr' ob' in
    loop [e; Nk.drop] StateSet.empty StateMap.empty StateMap.empty

let bisim (a1: t) (a2: t) : bool =
  (* let () = Printf.printf "bisim let's goooooo\na1:\n%s\na2:\n%s\n" (to_string
     a1) (to_string a2) in *)
  let rec bq q visited = 
    match q with
    | [] -> true
    | (pk,s1,s2)::rem -> let () = () in
                         (* let () = Printf.printf "comparing %s ; %s (for
                            pk=%s)\n%!" (Nk.to_string s1) (Nk.to_string s2)
                            (Sp.to_string pk) in *)
                         if Nk.eq s1 s2 || Sp.eq pk Sp.drop ||
                            (PairMap.mem (s1,s2) visited) && 
                            (Sp.le pk (PairMap.find (s1,s2) visited)) then
                           bq rem visited
                         else
                           let prev = match PairMap.find_opt (s1,s2) visited with
                                      | None -> Sp.drop
                                      | Some a -> a in
                           let rem = Sp.diff pk prev in
                         if not (Spp.eq (Spp.seq_pair (Spp.of_sp rem) (StateMap.find s1 a1.obs))
                                        (Spp.seq_pair (Spp.of_sp rem) (StateMap.find s2 a2.obs))) then
                           false
                         else
                           let tr1 = StateMap.find s1 a1.trans |> Sts.to_list in
                           let tr2 = StateMap.find s2 a2.trans |> Sts.to_list in
                           let next = List.fold_left (fun a (ei, sppi)->
                              (List.map (fun (ej, sppj) ->
                                (* let () = Printf.printf "pushing %s through %s...\n" (Sp.to_string pk) (Spp.intersect_pair sppi sppj |> Spp.to_string) in *)
                                let pk' = Spp.push rem (Spp.intersect_pair sppi sppj) in
                                (* let () = Printf.printf "got %s...\n" (Sp.to_string pk') in *)
                                (pk', ei, ej)) tr2)@a) [] tr1 in
                           let all1 = List.map (fun (_,spp) -> spp) tr1 |> Spp.union in
                           let all2  = List.map (fun (_,spp) -> spp) tr2 |> Spp.union in
                           let rem1 = List.map (fun (ei,sppi) ->
                               Spp.((push rem (diff sppi all2), ei, Nk.drop))) tr1 in
                           let rem2 = List.map (fun (ei,sppi) ->
                               Spp.((push rem (diff sppi all1), Nk.drop, ei))) tr2 in
                           let next' = next @ rem1 @ rem2 in

                           (* Update the visited set to include everything in
                              this packet (plus everything there already for this pair of states. *)
                           let vpk = Sp.union_pair prev rem in
                           let visited' = PairMap.add (s1,s2) vpk visited in
                           bq next' visited'
  in bq [(Sp.skip, a1.start, a2.start)] PairMap.empty

let forward (e: Nk.t) : Sp.t =
  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match StateMap.find_opt exp m with
                  | None -> Sp.drop
                  | Some sp -> sp in

  let rec loop (todo: (Nk.t * Sp.t) list) (visited: Sp.t StateMap.t) =
    match todo with
    | [] -> StateMap.bindings visited |>
            List.map (fun (e, pk) -> Spp.push pk (Deriv.e e)) |>
            Sp.union
    | (e, pkref) :: rem -> 
      let pk = !pkref in 
      match (e, pk) with 
      | (Nk.Drop, _)
      | (_, Sp.Drop) -> loop rem visited
      | (e, pk) ->
          let p = Sp.diff pkref (get visited e) in
          let v' = StateMap.add e (Sp.union_pair p (get visited e)) visited in
          let next = Deriv.d e
                    |> Sts.to_list
                    |> List.map (fun (e', spp) -> (e', Spp.push p spp)) in
          loop (next@rem) v'

  in loop [(e, Sp.skip)] StateMap.empty

let backward (e: Nk.t) : Sp.t =
  let a = autom e in

  let todo_init = 
    StateSet.elements a.states |> List.map (fun e -> (e, Spp.pull (Deriv.e e) Sp.skip)) in

  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match StateMap.find_opt exp m with
                  | None -> Sp.drop
                  | Some sp -> sp in

  let rec loop (todo: (Nk.t * Sp.t) list) (visited: Sp.t StateMap.t) =
    match todo with
    | [] -> get visited a.start
    | (e, pkref) :: rem -> 
      let pk = !pkref in 
      match (e, pk) with 
    | (Nk.Drop, _)
    | (_, Sp.Drop) -> loop rem visited
    | (e,pk) ->
        let p = Sp.diff pkref (get visited e) in
        let v' = StateMap.add e (Sp.union_pair p (get visited e)) visited in
        let next = StateSet.elements a.states 
                   |> List.map (fun e' -> (e', Deriv.d e' |> Sts.trans e))
                   |> List.map (fun (e', spp) -> (e', Spp.pull spp p)) in
        loop (next@rem) v'
  in loop todo_init StateMap.empty
