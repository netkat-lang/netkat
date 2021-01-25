open Alphabet

module MakeRx (A : Alphabet) : sig
    
    module Dfa : sig
        type t
        val dfa_to_json : t -> Yojson.Basic.t
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

    (* Brzozowski derivative *)
    val d : A.symbol -> t -> t

    (* DFA construction *)

    type trans
    type dfa_graph = t list * trans list

    val goto : t -> A.symbol -> t list -> trans list -> dfa_graph
    val explore : t list -> trans list -> t -> dfa_graph
    val to_dfa : t -> Dfa.t

end
