open Core
open Idds
open Katblib

(*===========================================================================*)
(* UTILITY FUNCTIONS                                                         *)
(*===========================================================================*)

let parse_expr (expr : [`File of string | `String of string]) =
  failwith "todo"
(*   match expr with
  | `String s -> Netkat.Parser.pol_of_string s
  | `File file -> Netkat.Parser.pol_of_file file *)

let time f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)

let print_time ?(prefix="") time =
  printf "%scompilation time: %.4f\n" prefix time


(*===========================================================================*)
(* FLAGS                                                                     *)
(*===========================================================================*)

module Flag = struct
  open Command.Spec

  let stdin =
    flag "--stdin" no_arg
      ~doc:"Read expression from stdin instead of from file."
end


(*===========================================================================*)
(* COMMANDS                                                                  *)
(*===========================================================================*)

module Idd = struct
  let spec = Command.Spec.(
    empty
    +> anon ("file" %: string)
    +> Flag.stdin
  )

  let run file_or_expr stdin () =
    let expr =
      parse_expr (if stdin then `String file_or_expr else `File file_or_expr)
    in
    let (time, idd) = time (fun () -> failwith "todo") in
    printf "%s\n" (Dd.to_string (idd :> Dd.t));
    print_time time;
    failwith "todo"

end


module Equiv = struct
  let spec = Command.Spec.(
      empty
      +> anon ("file1" %: string)
      +> anon ("file2" %: string)
      +> Flag.stdin
    )

  let run file_or_expr1 file_or_expr2 stdin () =
    let expr1 =
      parse_expr (if stdin then `String file_or_expr1 else `File file_or_expr1)
    in
    let expr2 =
      parse_expr (if stdin then `String file_or_expr1 else `File file_or_expr2)
    in
    if failwith "todo" then
      printf "true\n"
    else
      printf "false\n";

end



(*===========================================================================*)
(* BASIC SPECIFICATION OF COMMANDS                                           *)
(*===========================================================================*)

let idd : Command.t =
  Command.basic_spec
    ~summary:"Converts program to IDD and dumps it."
    Idd.spec
    Idd.run

let equiv : Command.t =
  Command.basic_spec
    ~summary:"Checks pair of programs for equivalence."
    Equiv.spec
    Equiv.run

let main : Command.t =
  Command.group
    ~summary:"Analyzes KAT+B! program."
    [("idd", idd); ("equiv", equiv)]

let () =
  Command.run ~version: "0.1" ~build_info: "N/A" main
