open Core_kernel
open Alphabet

module MakeRx (A : Alphabet) : sig
    type t =
      | Empty 
      | Epsilon
      | Char of A.symbol
      | Seq of t list
      | Union of t * t
      | Star of t
    val compare : t -> t -> int

    (* [equiv r] decides if the two regexs are equivalent *)
    val equiv : t -> t -> bool

    val seq : t list -> t
    val union : t -> t -> t
    val star : t -> t
end
