open Route_parser

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? digit, Opt digit, Opt digit]
let quad = [%sedlex.regexp? number, '.', number, '.', number, '.', number]

let cap = [%sedlex.regexp? 'A' .. 'Z']
let lower = [%sedlex.regexp? 'a' .. 'z']
let alphanum = [%sedlex.regexp? digit | cap | lower | '_' ]
let ifname = [%sedlex.regexp? cap, Star alphanum, Opt ('/', Star (digit | '.'))]
(* let ch = [%sedlex.regexp? digit | number | letter | '.' | '/' | '_'] *)
(* let fn = [%sedlex.regexp? Star ch] *)

let lexer_fail buf msg = 
  let first,last = Sedlexing.lexing_positions buf in
  let () = Printf.printf "%s: %d, col%d\n" msg first.pos_lnum (first.pos_cnum - first.pos_bol) in
  exit 1

let rec token buf =
  match%sedlex buf with
  | "Prefix" | "Next Hop" | "Interface" (* ignore column headers *)
  | ifname                              (* ignore interface names *)
  | Plus (Chars " \t\n") -> token buf     (* ignore whitespace *)
  | Plus lower -> begin
                  match Sedlexing.Latin1.lexeme buf with
                  | "receive" -> RCV
                  | "attached" -> ATTCH
                  | "drop" -> DROP
                  | s ->  lexer_fail buf ("Unrecognized action (" ^ s ^ ")")
                  end
  | quad, '/', number -> begin
                         match Sedlexing.Latin1.lexeme buf |> String.split_on_char '/' with
                         | q::n::[] ->
                             let ip = String.split_on_char '.' q |> List.map int_of_string in
                             let nn = int_of_string n in
                             begin
                             match ip with
                             | q1::q2::q3::q4::[] -> PREFIX ((q1,q2,q3,q4),nn)
                             | _ -> lexer_fail buf "Impossible lex"
                             end
                         | _ -> lexer_fail buf "Impossible lex"
                         end
  | quad -> begin
            let ns = Sedlexing.Latin1.lexeme buf |> String.split_on_char '.'
                                                 |> List.map int_of_string in
            match ns with
            | q1::q2::q3::q4::[] -> QUAD (q1,q2,q3,q4)
            | _ -> lexer_fail buf "Impossible lex"
            end
  (* | "\n" -> EOL *)
  | eof -> EOF
  | _ -> lexer_fail buf "Unrecognized character"
