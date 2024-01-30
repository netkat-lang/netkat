open Nkpl_parser

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let alphanum = [%sedlex.regexp? digit | letter ]
let ch = [%sedlex.regexp? digit | number | letter | '.' | '/']
let fn = [%sedlex.regexp? Star ch]

let rec token buf =
  match%sedlex buf with
  | "--", Star (Compl (Chars "\n")), '\n' (* line comment *)
  | Plus (Chars " \t\n?") -> token buf    (* ignore whitespace *)
  | "import" -> IMPORT
  | "check" -> CHECK
  | "print" -> PRINT
  | "drop"
  | "emp" -> DROP
  | "eps"
  | "skip" -> SKIP
  | "forward" -> FWD
  | "backward" -> BWD
  | '(' -> LPAR
  | ')' -> RPAR
  | '|'
  | '+' -> PLUS
  | '.'
  | ';' -> DOT
  | '*' -> STAR
  | '~' -> NEG
  | '&' -> AND
  | '=' -> TST
  | "dup" -> DUP
  | ":="
  | "<-" -> MOD
  | "==" -> EQUIV
  | "!==" -> NEQUIV
  | "!=" -> NTST
  | number -> NUM (int_of_string (Sedlexing.Latin1.lexeme buf))

  (* Unicode symbols for compatibility with 5stars *)
  | math -> begin match Sedlexing.Utf8.lexeme buf with
            | "\u{2295}" -> XOR   (* ⊕ *)
            | "\u{2227}"          (* ∧ *)
            | "\u{2229}" -> AND   (* ∩ *) 
            | "\u{00AC}" -> NEG   (* ¬ *)
            | "\u{22c5}" -> DOT   (* ⋅ *)
            | "\u{03b4}" -> DUP   (* δ *)
            | "\u{03b5}" -> SKIP  (* ε *)
            | "\u{2205}" -> DROP  (* ∅ *)
            | "\u{222a}"          (* ∪ *)
            | "\u{2228}" -> PLUS  (* ∨ *)
            | "\u{2190}" -> MOD   (* ← *)
            | "\u{22c6}" -> STAR  (* ⋆ *)
            | "\u{2261}" -> EQUIV (* ≡ *)
            | "\u{2262}" -> NEQUIV (* ≢ *)
            | "\u{2260}" -> NTST  (* ≠ *)
            | _ -> failwith "unknown math symbol"
            end
  | lowercase -> begin match Sedlexing.Utf8.lexeme buf with
                 | "\u{03b4}" -> DUP   (* δ *)
                 | "\u{03b5}" -> SKIP  (* ε *)
                 | _ -> failwith "unexpected lowercase"
                 end
          
  | letter, Star alphanum -> VAR (Sedlexing.Latin1.lexeme buf)
  | '@', Plus letter -> IDENT (Sedlexing.Latin1.lexeme buf)
  | '"', fn, '"' -> FILENAME (Sedlexing.Latin1.lexeme buf)
  | eof -> EOF
  | _ -> let first,last = Sedlexing.lexing_positions buf in
         let () = Printf.printf "unrecognized character line %d, col %d\n%!"
                        first.pos_lnum (first.pos_cnum - first.pos_bol) in
         exit 1
