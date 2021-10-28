type symbol = int

let compare = Stdlib.compare

type t = int list

let alphabet = [0 ; 1]

let iter (f: symbol->unit) (alph: t) = List.iter f alph

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

let rec enum_strings (strlen: int) =
  let succ (ll: t list) =
    List.fold_left (fun a1 s ->
      List.fold_left (fun a2 x -> (x::s)::a2) a1 alphabet) [[]] ll |> List.rev in
  if strlen = 0 then [[]] else succ (enum_strings (strlen-1))
