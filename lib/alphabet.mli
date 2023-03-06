(** Represents a finite alphabet of symbols. *)

type symbol
type word = symbol list
type t

val intalph : int -> t

val size : t -> int

val of_string_array : string array -> t

val compare : symbol -> symbol -> int

val symbols : t -> symbol list

val iter : (symbol->unit) -> t -> unit

val fold :  ('a->symbol->'a) -> 'a -> t -> 'a

val map : (symbol->'a) -> t -> 'a list

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

(** [prefix_of w1 w2] returns [true] if [w1 = w2@suffix] for some (possibly empty) word [suffix] *)
val prefix_of : word -> word -> bool

(** [resid pre w] returns [Some s] if [w = pre@s]. Otherwise returns [None] *)
val resid : word -> word -> word option
