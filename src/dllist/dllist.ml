(* invariants:
    - [c.prev.next == c]
    - [c.next.prev == c]
*)
type 'a cell = {
  el: 'a;
  mutable prev: 'a cell;
  mutable next: 'a cell;
}

(* invariants:
    - [l.first.prev == l.first], i.e. [l.first] is an initial list cell;
    - [l.last.next == l.last], i.e. [l.last] is a final list cell; and
    - [l.first.next.next....next = l.last], i.e. [l.first] and [l.last] are
      connected.
*)
type 'a t = {
  first : 'a cell;
  last : 'a cell;
}

let singleton el =
  let rec cell = { el; prev=cell; next=cell } in
  { first = cell; last = cell; }

let is_singleton t =
  t.first == t.last

let add_front t ~el =
  let rec first = { el; prev=first; next=t.first } in
  t.first.prev <- first;
  { t with first }

let add_back t ~el =
  let rec last = { el; prev=t.last; next=last } in
  t.last.next <- last;
  { t with last }

let append t1 t2 =
  t1.last.next <- t2.first;
  t2.first.prev <- t1.last;
  { first = t1.first; last = t2.last }

let append_join t1 t2 ~combine =
  match combine t1.last.el t2.first.el with
  | None ->
    append t1 t2
  | Some el ->
    let combined = { el; prev = t1.last.prev; next = t2.first.next; } in
    t1.last.prev.next <- combined;
    t2.first.next.prev <- combined;
    { first = t1.first; last = t2.last }

let to_list t =
  let rec go acc cell =
    let acc = cell.el :: acc in
    if cell.prev == cell then acc else go acc cell.prev
  in
  go [] t.last

let of_list = function
  | [] -> None
  | el::xs ->
    let rec first = { el; prev = first; next = first } in
    let rec go last = function
      | [] -> { first; last }
      | el::xs ->
        let rec new_last = { el; prev=last; next = new_last } in
        go new_last xs
    in
    Some (go first xs)