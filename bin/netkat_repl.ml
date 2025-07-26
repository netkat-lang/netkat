open Netkat_netkat
open Stdlib

let rec loop env =
  let () = Printf.printf "nkpl> " in
  try read_line () |> Interp.interp_string env |> loop

  with End_of_file -> Printf.printf "exit\n"
  

let () = 
  loop Env.empty
