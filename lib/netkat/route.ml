
type ip = int * int * int * int
type prefix = ip * int

type action =
  | Hop of ip
  | Drop
  | Rcv
  | Attch

type t = prefix * action list

let string_of_ip (q1,q2,q3,q4) =
  string_of_int q1 ^ "." ^
  string_of_int q2 ^ "." ^
  string_of_int q3 ^ "." ^
  string_of_int q4

let string_of_prefix (ip,p) = string_of_ip ip ^ "/" ^ string_of_int p

let string_of_action = function
  | Hop ip -> string_of_ip ip
  | Drop -> "drop"
  | Rcv -> "receive"
  | Attch -> "attached"

let string_of_route ((p,acts): t) =
  string_of_prefix p ^ "-> [" ^ String.concat "," (List.map string_of_action acts) ^ "]"

let to_nk (rt: t) : Nkexp.t = failwith ("TODO: " ^ __LOC__)
