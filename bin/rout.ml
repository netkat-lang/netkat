open Nerode_netkat
open Stdlib

let () =
  if Array.length Sys.argv < 2 then
    failwith "usage: rout <route-file>"
  else
    let _ = Routes.convert_file Sys.argv.(1)
    in ()
