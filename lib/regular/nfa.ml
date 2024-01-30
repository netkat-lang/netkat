type symbol = Alphabet.symbol
type word = Word.t

module type State = sig
  type t
  module StateSet : Set.S with type elt = t
  val compare : t -> t -> int
  val to_string : t -> string
  val fresh : StateSet.t -> t
end

type nsymbol = Char of symbol | Eps

let to_string alpha nsym =
  match nsym with
  | Char x -> Alphabet.sym_to_string alpha x
  | Eps -> "ε"

module type N = sig

  type state
  type t

  module StateSet : Set.S with type elt = state
  module StateMap : Map.S with type key = state
  module CharMap : Map.S with type key = nsymbol
  module StateSetPair : sig
    type t = StateSet.t * StateSet.t
    val compare : t -> t -> int
  end
  module StateRel : Set.S with type elt = StateSetPair.t

  type upto = StateSet.t -> StateSet.t -> StateRel.t -> bool

  val mk_nfa : Alphabet.t -> state list -> state list -> (state*nsymbol*state) list -> t
  val get_alpha : t -> Alphabet.t
  val get_start : t -> StateSet.t
  val contains_final : t -> StateSet.t -> bool
  val accept : t -> symbol list -> bool 
  val next : t -> StateSet.t -> symbol -> StateSet.t
  val trans_list : t -> (state * nsymbol * state) list
  val reverse : t -> t
  val print : t -> unit
  val to_rx : t -> Rx.t

  val naive : upto
  val context : upto
  val congruence : upto
  val bisim : upto -> t -> StateSet.t -> StateSet.t -> StateRel.t option
end


module Make (S : State) = struct

  type state = S.t

  module NSymOrdered = struct
    type t = nsymbol
    let compare c1 c2 = match c1,c2 with
    | Eps, Eps -> 0
    | Eps, _ -> -1
    | _, Eps -> 1
    | Char s1, Char s2 -> Alphabet.compare s1 s2
  end

  module StateSet = S.StateSet
  module StateMap = Map.Make(S)
  module CharMap = Map.Make(NSymOrdered)
  module StatePair = struct
    type t = state * state
    let compare ((a, b):t) ((c, d):t) = List.compare S.compare [a; b] [c; d]
  end
  module StateSetPair = struct
    type t = StateSet.t * StateSet.t
    let compare ((a, b):t) ((c, d):t) = List.compare StateSet.compare [a; b] [c; d]
  end

  module StateRel = Set.Make(StateSetPair)

  type tmap = (StateSet.t CharMap.t) StateMap.t

  type t = {
    alpha : Alphabet.t;   (* Σ *)
    states : StateSet.t;  (* Q *)
    transition : tmap;    (* δ *)
    start : StateSet.t;   (* q0 *)
    final : StateSet.t    (* F *)
  }

  type upto = StateSet.t -> StateSet.t -> StateRel.t -> bool
  
  let mk_trans (tr: (state*nsymbol*state) list) : tmap =
    List.fold_left (fun m (s1,x,s2) ->
      let m' = match StateMap.find_opt s1 m with
      | None -> CharMap.empty
      | Some cm -> cm in
      let m'' = match CharMap.find_opt x m' with
      | None -> CharMap.add x (StateSet.singleton s2) m'
      | Some set -> CharMap.add x (StateSet.add s2 set) m' in
      StateMap.add s1 m'' m) StateMap.empty tr

  let mk_nfa (alpha: Alphabet.t) (start: state list) (final: state list) (tr: (state*nsymbol*state) list) =
    let s = StateSet.of_list start in
    let f = StateSet.of_list final in
    let found = StateSet.of_list (List.fold_left (fun a (s1,x,s2) -> s1::s2::a) [] tr) in
    let states = StateSet.union s f |> StateSet.union found in
    let trans = mk_trans tr in
    {
      alpha = alpha;
      states = states;
      transition = trans;
      start = s;
      final = f;
    }

  (* Computes a fixpoint by repeatedly applying f to a. That is,
     returns b such that b = f^(n+1) a = f^n a for smallest natural number n, 
     where e is the equality check
     Careful! This loops infinitely if the fixpoint does not exist. *)
  let rec fixpt f e a =
    let a' = f a in
    if e a' a then
      a
    else
      fixpt f e a'

  let delta (nfa: t) (s: state) (x: nsymbol) : StateSet.t =
    match StateMap.find_opt s nfa.transition with
    | None -> StateSet.empty
    | Some cm -> match CharMap.find_opt x cm with
                 | None -> StateSet.empty
                 | Some s' -> s'

  let one_step (nfa: t) (x: nsymbol) (init: StateSet.t) (states: StateSet.t) =
    StateSet.fold (fun s a -> StateSet.union a (delta nfa s x)) states init

  let eps_closure (nfa: t) (s: StateSet.t) : StateSet.t =
    fixpt (one_step nfa Eps s) StateSet.equal s

  let next (nfa: t) (states: StateSet.t) (x: symbol) : StateSet.t =
    eps_closure nfa states |> one_step nfa (Char x) StateSet.empty |> eps_closure nfa

  let trans_list (nfa: t) : (state * nsymbol * state) list =
    List.fold_left (fun a1 (s1, m) ->
      List.fold_left (fun a2 (x, ss) -> 
        StateSet.fold (fun s2 a3 -> (s1, x, s2)::a3) ss a2
      ) a1 (CharMap.bindings m)
    ) [] (StateMap.bindings nfa.transition)

  let reverse (nfa: t) : t =
    let trans_rev = trans_list nfa |> List.map (fun (s1, x, s2) -> (s2, x, s1)) |> mk_trans in
    { nfa with
      start = nfa.final;
      final = nfa.start;
      transition = trans_rev }

  let get_alpha (nfa: t) = nfa.alpha
  let get_start (nfa: t) = nfa.start

  let contains_final (nfa: t) (q: StateSet.t) =
    StateSet.inter nfa.final q |> StateSet.is_empty |> not

  let accept (nfa: t) (w: word) : bool =
    List.fold_left (next nfa) nfa.start w
    |> contains_final nfa

  let print (nfa: t) =
    Printf.printf "Σ:%s\n%!" (Alphabet.to_string nfa.alpha);

    Printf.printf "Q: ";
    StateSet.iter (fun s -> S.to_string s |> Printf.printf "%s ") nfa.states;
    Printf.printf "\n%!";

    Printf.printf "δ: ";
    trans_list nfa |> 
    List.iter (fun (s1, x, s2) ->
      Printf.printf "(%s, %s, %s) " (S.to_string s1) (to_string nfa.alpha x) (S.to_string s2));
    Printf.printf "\n%!";

    Printf.printf "q0: ";
    StateSet.iter (fun s -> S.to_string s |> Printf.printf "%s ") nfa.start;
    Printf.printf "\n%!";

    Printf.printf "F: ";
    StateSet.iter (fun s -> S.to_string s |> Printf.printf "%s ") nfa.final;
    Printf.printf "\n%!"


    (* -- NFA -> Rx Conversion -- *)
    module LabelMap = Stdlib.Map.Make(StatePair)

    (* Construct Rx from NFA using Node-elimination *)
    let to_rx (nfa: t) : Rx.t =
      (* Start with Empty for every pair of states *)
      let empties: Rx.t LabelMap.t =
        StateSet.fold (fun s1 a1 ->
          StateSet.fold (fun s2 a2 ->
            LabelMap.add (s1, s2) Rx.Empty a2) nfa.states a1) nfa.states LabelMap.empty in

      (* Add in the transitions from the NFA *)
      let dfa_labels = List.fold_left (fun m (s1, x, s2) ->
        let sym = match x with
        | Eps -> Rx.Epsilon
        | Char y -> Rx.Char y in
        LabelMap.update (s1,s2) (fun b ->
          match b with
          | None -> failwith "panic!!"
          | Some rx -> Some (Rx.union_pair rx sym)) m
      ) empties (trans_list nfa) in

      (* Add a new start state *)
      let new_start = S.fresh nfa.states in
      let new_final = S.fresh nfa.states in

      (* Compute (ss U {new_start}) x (ss U {new_final}) *)
      let states_squared (ss:StateSet.t) : (state * state) list =
        let from_set = StateSet.union ss (StateSet.singleton new_start) in
        let to_set = StateSet.union ss (StateSet.singleton new_final) in
        StateSet.fold (fun s1 a1 ->
          StateSet.fold (fun s2 a2 -> (s1,s2)::a2) to_set a1) from_set [] in

      let final_trans (s: state) : Rx.t =
        if StateSet.mem s nfa.final then Rx.Epsilon else Rx.Empty in

      let nonstart = StateSet.diff nfa.states nfa.start in

      (* Add epsilon transition from new start state to old start state, and
      from final states to new final state. *)
      let extended_labels: Rx.t LabelMap.t =
        StateSet.fold (fun s a -> LabelMap.add (new_start, s) Rx.Empty a) nonstart dfa_labels |>
        StateSet.fold (fun s a -> LabelMap.add (new_start, s) Rx.Epsilon a) nfa.start |>
        StateSet.fold (fun s a -> LabelMap.add (s, new_final) (final_trans s) a) nfa.states |>
        LabelMap.add (new_start, new_final) Rx.Empty in

      (* Now "remove" every original state, updating labels -
         We don't actually remove anything, each round, just update the
         "remaining" labels to account for the "removed" states *)
      let final_labels: Rx.t LabelMap.t =
        let rec eliminate (ss:StateSet.t) (labels: Rx.t LabelMap.t) =

          (* "recreate" s *)
          if StateSet.is_empty ss then labels else
          let s = StateSet.min_elt ss in
          let ns = StateSet.remove s ss in
          let new_labels =
            List.fold_left (fun a (s1,s2) ->
              LabelMap.update (s1,s2) (fun r ->
                match r with
                | None -> failwith "of_dfa: Invalid key"
                | Some rx -> Some (Rx.union_pair rx (Rx.seq [LabelMap.find (s1, s) a;
                                                             Rx.star (LabelMap.find (s, s) a);
                                                             LabelMap.find (s, s2) a]))) a)
                labels (states_squared ns) in
          eliminate ns new_labels in
        eliminate nfa.states extended_labels in

      (* All the states are "removed", just look at new_start -> new_final *)
      LabelMap.find (new_start, new_final) final_labels

    let naive a b r = StateRel.mem (a, b) r
    let context = naive
    let congruence a b r =
      (* Compute a ``normal form'' for a stateset by
         applying rewrites from the relation until reaching fixpt *)
      let rec nf r s =
        let s' = StateRel.fold (fun (s1, s2) acc ->
          let first = if StateSet.subset s1 acc then
            StateSet.union acc s2 else acc in
          if StateSet.subset s2 first then
            StateSet.union first s1 else first) r s in
        if StateSet.equal s s' then
          s
        else
          nf r s'
      in
      StateSet.equal (nf r a) (nf r b)

    let bisim (u: upto) (nfa: t) (a: StateSet.t) (b: StateSet.t) : StateRel.t option =
      let rec bisim' (r: StateRel.t) (todo: StateSetPair.t list) : StateRel.t option = 
        match todo with
        | [] -> Some r
        | (a', b')::todo' ->
          if u a' b' r then
            bisim' r todo'
          else if StateSet.is_empty (StateSet.inter nfa.final a') <>
                  StateSet.is_empty (StateSet.inter nfa.final b') then 
            None
          else
            let todo' = todo @ (Alphabet.map (fun x -> (next nfa a' x, next nfa b' x)) nfa.alpha) in
            let r' = StateRel.add (a',b') r in
            bisim' r' todo'
      in bisim' StateRel.empty [(a,b)]
end 
