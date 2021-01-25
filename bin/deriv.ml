open Core
open Nerode

let () =
  let sysargs = Sys.get_argv () in
  let arg = Array.get sysargs 1 in
  let c = Array.get sysargs 2 in
  let rx = (Parser.parse_string arg) in
  begin
    Printf.printf "Your rx: %s\n%!" (Intrx.to_string rx);
    Printf.printf "d_%s(rx): %s\n%!" c (Intrx.to_string (Intrx.d (int_of_string c) rx))
  end
