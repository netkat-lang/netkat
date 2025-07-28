open Netkat_netkat
open Stdlib

let () =
  if Array.length Sys.argv < 2 then
    failwith "usage: dune exec netkat <nkpl-file>"
  else
    let _ = Interp.interp_file Sys.argv.(1)
    in ()
