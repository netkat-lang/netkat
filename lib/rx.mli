open Alphabet

module MakeRx (A : Alphabet) : sig
    type t =
      | Empty 
      | Epsilon
      | Char of A.symbol
      | Seq of t list
      | Union of t list
      | Star of t
    val compare : t -> t -> int

    (* [equiv r] decides if the two regexs are equivalent *)
    val equiv : t -> t -> bool

    val seq : t list -> t
    val union : t list -> t
    val union_pair : t -> t -> t
    val star : t -> t
end
