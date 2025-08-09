open Netkat_netkat
open Core
open Async

let rec process_line env buffer r w =
  Reader.read_line r
  >>= function
  | `Eof -> Core.printf "<DONE>\n%!"; exit 0
  | `Ok line ->
    (*Core.printf "line: \"%s\"\n%!" line;*)
    let env2 = Interp.interp_string env line in
    process_line env2 buffer r w

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

let _ = start 8080
