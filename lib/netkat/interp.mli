(** The module for interpreting nkpl commands from strings. *)

(** Parse a string as a nkpl program. *)
val parse_string : (string -> unit) -> Env.t -> string -> Nkcmd.t option

(** Opens a file by its filename and parses the contents. *)
val parse_file : (string -> unit) -> string -> Nkcmd.t list

(** Interprets / executes the nkpl command. *)
val interp : (string -> unit) -> string -> Env.t -> Nkcmd.t -> Env.t

(** Interprets a string as a nkpl program. *)
val interp_string : (string -> unit) -> Env.t -> string -> Env.t

(** Opens a file by its filename and interprets the contents; returns the resulting [Env.t]. *)
val interp_file : (string -> unit) -> string -> Env.t
