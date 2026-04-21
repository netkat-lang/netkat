let exp_of_string (s: string) : Nkexp.t option =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer  = Sedlexing.with_tokenizer (Nkpl_lexer.token (Nkpl_lexer.fresh_state ())) lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.single_exp in
  (try
    Some(parser lexer)
  with
    | Nkpl_parser.Error s ->
      let (x,y) = Sedlexing.lexing_positions lexbuf in
      Printf.printf "Parse error: %s (%d:%d)\n" (Sedlexing.Utf8.lexeme lexbuf) x.pos_lnum (x.pos_cnum - x.pos_bol);
      None)


