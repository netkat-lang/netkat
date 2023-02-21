open Alphabet

type t =
  | Empty
  | Epsilon
  | Char of symbol
  | Seq of t list
  | Union of t list
  | Star of t
  | QMark of t
  | Intersect of t list
  | Neg of t
val compare : t -> t -> int

(* [equiv r] decides if the two regexs are *syntactically* equivalent *)
val equiv : t -> t -> bool

val seq : t list -> t
val seq_pair : t -> t -> t

val union : t list -> t
val union_pair : t -> t -> t

val intersect : t list -> t
val intersect_pair : t -> t -> t

val star : t -> t
val qmark : t -> t
val neg : t -> t

val difference : t -> t -> t

val to_string : Alphabet.t -> t -> string

(* Nullable *)
val e : t -> bool

(* Brzozowski derivative *)
val d : symbol -> t -> t

(* DFA construction *)
(*
val to_dfa : Alphabet.t -> t -> Dfa.t
val of_dfa : Dfa.t -> t
*)

(* Representative *)
(*
val rep_symlist : t -> symbol list option
val rep_string : t -> string
*)
