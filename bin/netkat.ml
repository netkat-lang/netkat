open Netkat_netkat
open Stdlib

let () =
  if Array.length Sys.argv < 2 then
    failwith "usage: nk <nkpl-file>"
  else
    let _ = Interp.interp_file Sys.argv.(1)
    in ()
