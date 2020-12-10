(** Closed (i.e. Hole-free) Regular expressions *)
open Alphabet
open DFA

module S

module rec R : 
sig 
  type t =
    | Empty 
    | Char of Alphabet.symbol
    | Seq of t list
    | Union of S.t
    | Star of t
  val compare : t -> t -> int
end

open R

(* constructors *)


module MakeRx (A : Alphabet) : sig
    (* [rep r] gives a representative member string *)
    val rep : t -> Alphabet.symbol list

    (* [equiv r] decides if the two regexs are equivalent *)
    val equiv : t -> t -> bool

    (* [empty r] returns true if the regex is empty *)
    val is_empty : t -> bool

    val string_of_t : t -> string

    (* DFA conversion *)
    let goto A.symbol -> A.symbol -> A.symbol list -> A.symbol list
    let explore -> A.symbol list -> A.symbol list -> A.symbol
    let to_dfa t -> Dfa.t
end
