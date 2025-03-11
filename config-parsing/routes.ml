open Route

let convert_file (fn: string) : (*Nkexp.t*) unit =
  let f = In_channel.open_text fn in
  let lexbuf = Sedlexing.Utf8.from_channel f in
  let lexer  = Sedlexing.with_tokenizer Route_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Route_parser.route_file in

  try
    let rs = parser lexer in
    let () = List.iter (fun r -> Printf.printf "%s\n" (string_of_route r)) rs in
    ()
  with _ ->
    let _, p1, p2 = lexer () in
    Printf.printf "Routes parse failed (line %d, col %d),(line %d, col %d)!\n"
    p1.pos_lnum (p1.pos_cnum - p1.pos_bol)
    p2.pos_lnum (p2.pos_cnum - p2.pos_bol)
