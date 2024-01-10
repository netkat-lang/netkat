open Nerode_netkat
open Stdlib

let () =
  if Array.length Sys.argv < 2 then
    failwith "usage: nk <nkpl-file>"
  else
    let f = In_channel.open_text Sys.argv.(1) in
    let lexbuf = Sedlexing.Utf8.from_channel f in
    let lexer  = Sedlexing.with_tokenizer Nkpl_lexer.token lexbuf in
    let parser = MenhirLib.Convert.Simplified.traditional2revised Nkpl_parser.nkpl_file in
    let cmds = parser lexer in
    List.fold_left (fun _ -> Nkcmd.interp) () cmds
