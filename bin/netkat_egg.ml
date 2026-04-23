open Netkat_netkat
open Stdlib

(*let _ = Nkego.test () *)

let rec loop () =
  let () = Printf.printf "netkat> " in
  try read_line () |> Nkego.interp_string |> loop

  with
  | End_of_file -> Printf.printf "exit\n"
  | _ -> loop ()
  

let () = 
  loop ()
