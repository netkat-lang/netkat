open Nkpl_parser

type lexer_state = {
  lookahead : Nkpl_parser.token Queue.t;
  mutable prev_token : Nkpl_parser.token option ref;
}

let fresh_state () = {
  lookahead = Queue.create ();
  prev_token = ref None;
}

let to_string t = match t with
| DROP -> "DROP"
| SKIP -> "SKIP"
| IMPORT -> "IMPORT"
| CHECK -> "CHECK"
| PRINT -> "PRINT"
| TIKZ -> "TIKZ"
| REP -> "REP"
| SIMULATE -> "SIMULATE"
| FOR -> "FOR"
| FWD -> "FWD"
| BWD -> "BWD"
| EXISTS -> "EXISTS"
| FORALL -> "FORALL"
| DO -> "DO"
| DOTDOT -> "DOTDOT"
| IN -> "IN"
| LPAR -> "LPAR"
| RPAR -> "RPAR"
| PLUS -> "PLUS"
| DIFF -> "DIFF"
| DOT -> "DOT"
| STAR -> "STAR"
| NEG -> "NEG"
| AND -> "AND"
| TST -> "TST"
| XOR -> "XOR"
| DUP -> "DUP"
| MOD -> "MOD"
| EQUIV -> "EQUIV"
| NEQUIV -> "NEQUIV"
| LEQ -> "LEQ"
| NTST -> "NTST"
| FILENAME s -> Printf.sprintf "FILENAME(%s)" s
| IDENT s -> Printf.sprintf "IDENT(%s)" s
| NUM i -> Printf.sprintf "NUM(%d)" i
| NEWLINE -> "<newline>"
| RANGESUM -> "RANGESUM"
| EOF -> "EOF"
| LAMBDA -> "LAMBDA"
| ARROW -> "ARROW"
| RBRACE -> "RBRACE"
| LBRACE -> "LBRACE"
| LCURLY -> "LCURLY"
| RCURLY -> "RCURLY"
|COM -> "COM"

let can_end_cmd t = match t with
| Some(DROP | SKIP | DUP | RPAR | STAR | NUM _ | IDENT _ | FILENAME _) -> true
| _ -> false

let can_begin_cmd t = match t with
| IMPORT | CHECK | PRINT | TIKZ | REP | SIMULATE | FOR | IDENT _ -> true
| _ -> false

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Opt '-', Plus digit]
let subdigit = [%sedlex.regexp? 0x2080 .. 0x2089]
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let letteru = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_']
let alphanum = [%sedlex.regexp? digit | letter | subdigit | '_' | '-' ]
let ch = [%sedlex.regexp? digit | number | letter | '.' | '/' | '_' | '-']
let fn = [%sedlex.regexp? Star ch]
let whsp = [%sedlex.regexp? ' ' | '\t' | '\r' | '\n' | '?']
let newline = [%sedlex.regexp? '\n']
let comment = [%sedlex.regexp? "--", Star (Compl (Chars "\n")), '\n']

let rec peek_token state buf =
  if Queue.is_empty state.lookahead then begin
    let t = raw_token buf in
    Queue.add t state.lookahead
  end;
  Queue.peek state.lookahead

and token state buf =
  let t = if not (Queue.is_empty state.lookahead) then
    Queue.take state.lookahead
  else
    raw_token buf
  in
  let next_token = peek_token state buf in
  let return t = (
    state.prev_token := Some(t);
    t
  ) in
  let test = (can_end_cmd !(state.prev_token)) && (can_begin_cmd next_token) in
  (*Printf.printf "prev: %s, current: %s --> %s\n"
    (match !(state.prev_token) with None -> "<none>" | Some(t) -> to_string t)
    (to_string t)
    (if test then "true" else "false");*)
  match t with
  | NEWLINE -> if test then return t else token state buf
  | _ -> return t

and raw_token buf =
  match%sedlex buf with
  | comment (* line comment *)
  | newline, Star (newline | whsp) -> NEWLINE
  | whsp -> raw_token buf    (* ignore whitespace *)
  | "rangesum" ->  RANGESUM
  | "import" ->  IMPORT
  | "check" ->  CHECK
  | "print" ->  PRINT
  | "tikz" ->  TIKZ
  | "drop"
  | "emp" ->  DROP
  | "eps"
  | "skip" ->  SKIP
  | "forward" ->  FWD
  | "backward" ->  BWD
  | "exists" ->  EXISTS
  | "forall" ->  FORALL
  | "rep" ->  REP
  | "simulate" ->  SIMULATE
  | "for" ->  FOR
  | "do" ->  DO
  | ".." ->  DOTDOT
  | "in" ->  IN
  | '(' ->  LPAR
  | ')' ->  RPAR
  | '[' ->  LBRACE
  | ']' ->  RBRACE
  | '{' ->  LCURLY
  | '}' ->  RCURLY
  | '|'
  | '+' ->  PLUS
  | '-' ->  DIFF
  | '.'
  | ';' ->  DOT
  | ',' ->  COM
  | '*' ->  STAR
  | '~' ->  NEG
  | '&' ->  AND
  | '=' ->  TST
  | '^' ->  XOR
  | "dup" ->  DUP
  | ":="
  | "<-" ->  MOD
  | "==" ->  EQUIV
  | "!==" ->  NEQUIV
  | "<=" ->  LEQ
  | "!=" ->  NTST
  | number ->  (NUM (int_of_string (Sedlexing.Latin1.lexeme buf)))

  (* Unicode symbols for compatibility with 5stars *)
  | math -> let s = Sedlexing.Utf8.lexeme buf in
            begin match s with
            | "\u{21d2}" ->  ARROW  (* ⇒ *)
            | "\u{2295}" ->  XOR    (* ⊕ *)
            | "\u{2227}"           (* ∧ *)
            | "\u{2229}" ->  AND    (* ∩ *)
            | "\u{00AC}" ->  NEG    (* ¬ *)
            | "\u{22c5}" ->  DOT    (* ⋅ *)
            | "\u{03b4}" ->  DUP    (* δ *)
            | "\u{03b5}" ->  SKIP   (* ε *)
            | "\u{2205}" ->  DROP   (* ∅ *)
            | "\u{22a4}" ->  SKIP   (* ⊤ *)
            | "\u{22a5}" ->  DROP   (* ⊥ *)
            | "\u{222a}"           (* ∪ *)
            | "\u{2228}" ->  PLUS   (* ∨ *)
            | "\u{2190}" ->  MOD    (* ← *)
            | "\u{22c6}" ->  STAR   (* ⋆ *)
            | "\u{2261}" ->  EQUIV  (* ≡ *)
            | "\u{2262}" ->  NEQUIV (* ≢ *)
            | "\u{2260}" ->  NTST   (* ≠ *)
            | "\u{2208}" ->  IN     (* ∈ *)
            | _ ->
                let first,last = Sedlexing.lexing_positions buf in
                let () = Printf.printf "unknown math symbol: %s (line %d, col %d)\n%!"
                             s first.pos_lnum (first.pos_cnum - first.pos_bol) in
                exit 1
            end
  | 'T' -> SKIP
  | letteru, Star alphanum ->
    let s = Sedlexing.Utf8.lexeme buf in
     (IDENT(s))
  | lowercase ->
      begin match Sedlexing.Utf8.lexeme buf with
      | "\u{03b5}" ->  SKIP  (* ε *)
      | "\u{03b4}" ->  DUP    (* δ *)
      | "\u{03bb}" ->  LAMBDA (* λ *)
      | _ ->
         let first,last = Sedlexing.lexing_positions buf in
         let () = Printf.printf "unrecognized character line %d, col %d\n%!"
                        first.pos_lnum (first.pos_cnum - first.pos_bol) in
         exit 1
      end
  | '@', Plus alphanum ->
    let s = Sedlexing.Utf8.lexeme buf in
    (*String.iter (fun c -> Printf.printf "char: %c\n" c) s;*)
     (IDENT (s))
  | '"', fn, '"' -> 
      let s = Sedlexing.Latin1.lexeme buf in
      let fn = String.sub s 1 (String.length s - 2) in
       (FILENAME fn)
  | eof -> EOF
  | _ -> let first,last = Sedlexing.lexing_positions buf in
         let () = Printf.printf "unrecognized character line %d, col %d\n%!"
                        first.pos_lnum (first.pos_cnum - first.pos_bol) in
         exit 1
