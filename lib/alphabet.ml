module type Alphabet = sig

  type symbol
  type t

  val alphabet : t

  val compare : symbol -> symbol -> int

  val iter : (symbol->unit) -> t -> unit
  val fold :  ('a->symbol->'a) -> 'a -> 'a
  val map : (symbol->'a) -> t -> 'a list

  val extract_json : Yojson.Basic.t -> symbol

  val to_json : symbol -> Yojson.Basic.t

  val to_string : symbol -> string

  val of_string : string -> t list

end

module A : Alphabet = struct

  type symbol = int

  let compare = Stdlib.compare

  type t = int list

  let alphabet: t = [0 ; 1]

  let iter = List.iter

  let fold (f:'a->symbol->'a) (a:'a) = List.fold_left f a alphabet

  let map = List.map

  let extract_json = function
    | `Int i -> i
    | _ -> failwith "invalid json format"

  let to_json i = `Int i

  let to_string = string_of_int

  let of_string s = 
    let rec consume_char i lst =
      if i < 0 then lst
      else if s.[i] = 'X' then 
        consume_char (pred i) (List.fold_left (fun acc w -> (1::w)::(0::w)::acc) [] lst)
      else 
        consume_char (pred i) (List.map (fun w -> (int_of_char s.[i] - 48)::w) lst)
    in
    consume_char (String.length s |> pred) [[]]

end
