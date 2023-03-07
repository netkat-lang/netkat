(** Represents a finite alphabet of symbols. *)

(** A single symbol in an alphabet. *)
type symbol

(** A finite sequence of symbols. *)
type word = symbol list

(** An alphabet: a finite collection of symbols. *)
type t

(** [intalph k] returns an alphabet containing the symbols {0, 1, ..., k-1},
   with string representations "0", "1", ... *)
val intalph : int -> t

(** [size alpha] returns the size of [alpha] *)
val size : t -> int

(** [of_string_array a] constructs an alphabet with symbols corresponding to the
    strings in [a]. *)
val of_string_array : string array -> t

(** [compare x y] returns a number greater than 0 if x is greater than y, 0 if
  x = y,  and less than 0 if x is less than y. *)
val compare : symbol -> symbol -> int

(** [symbols  alpha] returns a list of the symbols in this alphabet. *)
val symbols : t -> symbol list

(** [iter f alpha] performs [f x] for each symbol [x] in [alpha]. *)
val iter : (symbol->unit) -> t -> unit

(** [fold f init alpha] folds the function [f] over [alpha], i.e.,
    returning [f ( f init x1 ) x2 ...] for all the symbols [x1, x2, ...] in [alpha]. *)
val fold :  ('a->symbol->'a) -> 'a -> t -> 'a

(** [map f alpha] returns a list [[f x1, f x2, ...]] for the symbols
    [x1, x2, ...] in [alpha]. *)
val map : (symbol->'a) -> t -> 'a list

(** [prefix_of w1 w2] returns [true] if [w1 = w2@suffix] for some (possibly
    empty) word [suffix]. *)
val prefix_of : word -> word -> bool

(** [resid pre w] returns [Some s] if [w = pre@s]. Otherwise returns [None]. *)
val resid : word -> word -> word option

val sym_of_sexp : Core.Sexp.t -> symbol
val sexp_of_sym : symbol -> Core.Sexp.t
val sym_of_json : Yojson.Basic.t -> symbol
val sym_to_json : symbol -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t

val sym_to_string : t -> symbol -> string
val to_string : t -> string
val w_to_string : t -> word -> string

val sym_of_int : int -> symbol
val sym_to_int : symbol -> int

val w_of_ints : int list -> word
val w_to_ints : word -> int list
val ws_of_strings : t -> string list -> word list
