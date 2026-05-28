open Netkat_netkat
open Stdlib

let filenames = ref []
let quiet = ref false
let synth = ref false
let fd = ref false
let allow_disjunction = ref false

let add_filename filename =
  filenames := filename :: !filenames

let i = ref 0
let n = ref 1
let ignore_fields = ref []

let add_string s =
  ignore_fields := s::!ignore_fields

let add_default_strings () =
  ignore_fields := ["@dir";"@dev";"@if"(*;"@srcip-0";"@srcip-1";"@dstip-0";"@dstip-1"*)]

let specs = [
  ("-i", Arg.Set_int i, "Test flag");
  ("-n", Arg.Set_int n, "Maximum number of filters");
  ("-f", Arg.String add_string, "Specify a field to ignore");
  ("-fd", Arg.Unit add_default_strings, "Ignore default fields");
  ("-dis", Arg.Set allow_disjunction, "Allow exploring disjunctions");
  ("-q", Arg.Set quiet, "Quiet output");
  ("-s", Arg.Set synth, "CEGIS synthesizer");
]

let usage = "usage: dune exec <program> [file] ..."

let () =
  Arg.parse specs add_filename usage

let () =
  if List.length (!filenames) < 1 then
    failwith usage
  else
    List.iter (fun f ->
      if not !synth then (
        let (_,(env,_),results) = Interp.interp_file (if !quiet then (fun s -> ()) else print_string) f in
        Core.printf "\n%s\n" (Interp.result_list_to_json results);
        ()
      ) else (
        let (_,(env,_),results,filter) = Cegis.interp_file !n !ignore_fields !allow_disjunction (if !quiet then (fun s -> ()) else print_string) f in
        (*Core.printf "\n%s\n" (Interp.result_list_to_json results);*)
        Printf.printf "\n";
        match filter with
        | Success(e) -> Printf.printf "SYNTHSIZER RESULT: SUCCESS: %s\n%!" (Nk.to_string e);
        | Fail -> Printf.printf "SYNTHESIZER RESULT: FAIL\n%!";
        ()
      )
    ) (!filenames);
    ()
