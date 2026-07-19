open Netkat_netkat
open Core
open Async

let quiet = ref false

let filenames = ref []

let add_filename filename =
  filenames := filename :: !filenames

let port = ref 8080

let specs = [
  ("-p", Arg.Set_int port, "Listen port");
  ("-q", Arg.Set quiet, "Quiet output");
  ("-v", Arg.Clear quiet, "Verbose output");
]

let usage = "usage: dune exec <program> [file] ..."

let () =
  Arg.parse specs add_filename usage

let rec process_line env buffer r w =
  Reader.read_line r
  >>= function
  | `Eof -> Core.printf "<DONE>\n%!"; exit 0
  | `Ok line -> (
    (*Core.printf "line: \"%s\"\n%!" line;*)
    let ((env2,m2),results) = Interp.interp_string (if !quiet then (fun x -> ()) else Writer.write w) (env,None) line in
    Writer.writef w "\n%s\n%!" (Interp.result_list_to_json results);
    process_line env2 buffer r w
  )
let run uppercase port =
  let host_and_port =
    Async.Tcp.Server.create
      ~on_handler_error:`Raise
      (Async.Tcp.Where_to_listen.of_port port)
      (fun _addr r w ->
        let buffer = Bytes.create (16 * 1024) in
        process_line Env.empty buffer r w) in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let start port = 
  Core.printf "[starting server on port %d]\n%!" port;
  let _ = run true port in
  never_returns (Scheduler.go ())

let _ = start !port
