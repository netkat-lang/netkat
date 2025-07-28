open Netkat_netkat
open Stdlib

let () =
  if Array.length Sys.argv < 2 then
    failwith "usage: dune exec netkat-parse <nkpl-file>"
  else
    let _ = Interp.parse_file Sys.argv.(1)
    in ()
