let reraise exn =
  Printexc.(raise_with_backtrace exn (get_raw_backtrace ()))


module type PARSER = sig
  type token
  type result
  exception LexError of string
  exception ParseError
  (* val pp_token : Format.formatter -> token -> unit *)
  val parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> result
  val next_token : Lexing.lexbuf -> token
end

module type S = sig
  type token
  type result
  exception LexError of { msg: string; loc: Location.t }
  exception ParseError of { token: token; loc: Location.t }
  val parse_string : ?pos:Lexing.position -> string -> result
  val parse_chan : ?pos:Lexing.position -> in_channel -> result
  val parse_file : string -> result
  val pp_exceptions : unit -> unit
end


module Make (Parser : PARSER) = struct
  type token = Parser.token
  type result = Parser.result
  exception LexError of { msg: string; loc: Location.t }
  exception ParseError of { token: token; loc:Location.t }

  let pp_exceptions () = begin
    Location.register_error_of_exn (function
      | LexError {msg; loc} ->
        Some (Location.error ~loc msg)
      | ParseError {loc; _} ->
        Some (Location.error ~loc "[parser] unexpected token")
          (* Parser.pp_token token *)
      | _ ->
        None
    );
    Printexc.register_printer (function exn ->
      try
        ignore (Format.flush_str_formatter ());
        Location.report_exception Format.str_formatter exn;
        Some (Format.flush_str_formatter ());
      with _ ->
        None
    );
  end

  let curr_token : token option ref =
    ref None

  let next_token lexbuf =
    let token = Parser.next_token lexbuf in
    curr_token := Some token;
    token

  let parse ?(file="") lexbuf =
    Location.init lexbuf file;
    Location.input_name := file;
    Location.input_lexbuf := Some lexbuf;
    try 
      Parser.parse next_token lexbuf
    with
    | Parser.LexError msg ->
      reraise (LexError { msg; loc = Location.curr lexbuf })
    | Parser.ParseError ->
      let token = match !curr_token with Some t -> t | None -> assert false in
      reraise (ParseError { token; loc = Location.curr lexbuf })

  let parse_string ?pos s =
    let lexbuf =
      match pos with
      | None -> Lexing.from_string s
      | Some p -> Lexing.{(from_string s) with lex_start_p=p; lex_curr_p=p}
    in
    parse lexbuf

  let parse_chan ?pos chan =
    let lexbuf =
      match pos with
      | None -> Lexing.from_channel chan
      | Some p -> Lexing.{(from_channel chan) with lex_start_p=p; lex_curr_p=p}
    in
    parse lexbuf

  let parse_file file =
    Stdio.In_channel.with_file file ~f:(fun chan ->
      parse ~file (Lexing.from_channel chan)
    )

end