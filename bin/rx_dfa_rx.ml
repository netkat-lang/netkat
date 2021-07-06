open Core
open Nerode
open Yojson

let () =
  let sysargs = Sys.get_argv () in
  let arg = Array.get sysargs 1 in
  let rx = (Parser.parse_string arg) in
  begin
    Printf.printf "Your rx: %s\n%!" (Intrx.to_string rx);

    let dfa: Intrx.Dfa.t = rx |> Intrx.to_dfa in

    Printf.printf "Dfa: \n%s\n" (rx |> Intrx.to_dfa |> Intrx.Dfa.dfa_to_json |> Basic.to_string);

    (*
    Printf.printf "running 011: %B\n%!" (Intrx.Dfa.accepts dfa [0; 1; 1]);
    Printf.printf "running 10: %B\n%!" (Intrx.Dfa.accepts dfa [1; 0]);
    *)

    Printf.printf "Dfa->Rx: \n%s\n" (dfa |> Intrx.of_dfa |> Intrx.to_string);

    Printf.printf "Representative: %s\n" (Intrx.Dfa.representative dfa)
  end
