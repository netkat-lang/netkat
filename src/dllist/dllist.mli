(** Imperative doubly-linked list module. *)


(** {1 Types} *)

(** Cell of a doubly-linked list, consisting of an element [el] and neighboring
    cells. A cell [c : 'a cell] is
    - {e initial} if [c.prev == c];
    - {e final} if [c.next == c]; and
    - {e singleton} if [c.prev == c.next == c].

    Invariants:
    - c.prev.next == c
    - c.next.prev == c
*)
type 'a cell = private {
  el: 'a;
  mutable prev: 'a cell;
  mutable next: 'a cell;
}

(** A non-empty doubly-linked list with first and last cells.

    Invariants:
    - [l.first] is initial, i.e. [l.first.prev == l.first];
    - [l.last] is a final, i.e. [l.last.next == l.last]; and
    - [l.first] and [l.last] are connected, i.e. 
      [l.first.next.next....next == l.last].
  *)
type 'a t = private {
  first : 'a cell;
  last : 'a cell;
}


(** {1 Operations} *)

val singleton : 'a -> 'a t
val is_singleton : 'a t -> bool

val to_list : 'a t -> 'a list

(** Returns doubly-linked version of [l : 'a list] for [l] non-empty,
    or [None] for [l] empty. *)
val of_list : 'a list -> 'a t option


(** {2 Destructive, constant-time operations}
    {e Do not} reuse lists after passing them to any of these operations, or all
    bets are off. *)

val add_front : 'a t -> el:'a -> 'a t
val add_back : 'a t -> el:'a -> 'a t

(** [append t1 t2] is the concatenation of [t1] and [t2] *)
val append : 'a t -> 'a t -> 'a t

(** [append_join t1 t2] is like [append t1 t2], except that the former will
    combine [t1]'s last and [t2]'s first element into [el] if
    [combine t1.last.el t2.last.el = Some el]. *)
val append_join : 'a t -> 'a t -> combine:('a -> 'a -> 'a option) -> 'a t