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
  "states: " ^ (StateSet.elements a.states |> List.map Nk.to_string |> String.concat ", ") ^
  "\nstart: " ^ (Nk.to_string a.start) ^
  "\ntrans: " ^ (StateMap.bindings a.trans
          |> List.map (fun (e,sts) -> (Nk.to_string e) ^ "↦" ^ Sts.to_string sts) |> String.concat "; ") ^
  "\nobs: " ^ (StateMap.bindings a.obs |> List.map ebinding_to_string  |> String.concat ", ") ^ "\n\n"

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
    loop [e] StateSet.empty StateMap.empty StateMap.empty

let bisim (a1: t) (a2: t) : bool =
  (* let () = Printf.printf "bisim let's goooooo\na1:\n%s\na2:\n%s\n" (to_string a1) (to_string a2) in *)
  let rec bq q visited = 
    match q with
    | [] -> true
    | (pk,s1,s2)::rem -> let () = () in
                         (* let () = Printf.printf "comparing %s ; %s (for pk=%s)\n%!" (Nk.to_string s1) (Nk.to_string s2) (Sp.to_string pk) in *)
                         if Sp.eq pk Sp.drop ||
                            (PairMap.mem (s1,s2) visited) && 
                            (Sp.le pk (PairMap.find (s1,s2) visited)) then
                           (* let () = Printf.printf "%s\n%!" __LOC__ in *)
                           bq rem visited
                         else if not (Spp.eq (Spp.seq_pair (Spp.of_sp pk) (StateMap.find s1 a1.obs))
                                             (Spp.seq_pair (Spp.of_sp pk) (StateMap.find s2 a2.obs))) then
                           false
                         else
                           let tr1 = StateMap.find s1 a1.trans |> Sts.to_list in
                           let tr2 = StateMap.find s2 a2.trans |> Sts.to_list in
                           let next = List.fold_left (fun a (ei, sppi)->
                              (List.map (fun (ej, sppj) ->
                                (* let () = Printf.printf "pushing %s through %s...\n" (Sp.to_string pk) (Spp.intersect_pair sppi sppj |> Spp.to_string) in *)
                                let pk' = Spp.push pk (Spp.intersect_pair sppi sppj) in
                                (* let () = Printf.printf "got %s...\n" (Sp.to_string pk') in *)
                                (pk', ei, ej)) tr2)@a) [] tr1 in
                           let visited' = PairMap.add (s1,s2) pk visited in
                           bq next visited'
  in bq [(Sp.skip, a1.start, a2.start)] PairMap.empty
