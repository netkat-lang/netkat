open Netkat_netkat
open Stdlib

let rec loop env =
  let () = Printf.printf "nkpl> " in
  try
    let line = read_line () in
    let (env',result) = Interp.interp_string print_string env line in
    loop env'
  with End_of_file -> Printf.printf "exit\n"
  

let () = 
  loop Env.empty
