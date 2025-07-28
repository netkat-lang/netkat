(** The module for interpreting nkpl commands from strings. *)

(** Parse a string as a nkpl program. *)
val parse_string : Env.t -> string -> Nkcmd.t option

(** Opens a file by its filename and parses the contents. *)
val parse_file : string -> Nkcmd.t list

(** Interprets / executes the nkpl command. *)
val interp : string -> Env.t -> Nkcmd.t -> Env.t

(** Interprets a string as a nkpl program. *)
val interp_string : Env.t -> string -> Env.t

(** Opens a file by its filename and interprets the contents; returns the resulting [Env.t]. *)
val interp_file : string -> Env.t
