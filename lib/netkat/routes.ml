
let convert_file (fn: string) : Nkexp.t =
  let f = In_channel.open_text fn in
  let lexbuf = Sedlexing.Utf8.from_channel f in
  let lexer  = Sedlexing.with_tokenizer Route_lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Route_parser.route_file in
  let () = ignore (parser lexer) in
  failwith ("TODO: " ^ __LOC__)
