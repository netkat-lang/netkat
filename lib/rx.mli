open Alphabet

module MakeRx (A : Alphabet) : sig
    
    module Dfa : sig
        type t
    end

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
    val seq_pair : t -> t -> t
    val star : t -> t
    
    val to_string : t -> string
    
    (* Nullable *)
    val e : t -> bool

    (* Brzowski derivative *)
    val d : A.symbol -> t -> t

    (* DFA construction *)
    (*
    val goto : A.symbol -> A.symbol -> A.symbol list -> A.symbol list
    val explore : A.symbol list -> A.symbol list -> A.symbol
    val to_dfa : t -> Dfa.t
    *)

end
