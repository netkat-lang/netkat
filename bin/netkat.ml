open Netkat_netkat
open Stdlib

let filenames = ref []
let quiet = ref false

let add_filename filename =
  filenames := filename :: !filenames

let i = ref 0

let specs = [
  ("-i", Arg.Set_int i, "Test flag");
  ("-q", Arg.Set quiet, "Quiet output");
]

let usage = "usage: dune exec <program> [file] ..."

let () =
  Arg.parse specs add_filename usage

let () =
  if List.length (!filenames) < 1 then
    failwith usage
  else
    List.iter (fun f ->
      let (_,_,results) = Interp.interp_file (if !quiet then (fun s -> ()) else print_string) f in
      Core.printf "\n%s\n" (Interp.result_list_to_json results);
      ()
    ) (!filenames);
    ()
