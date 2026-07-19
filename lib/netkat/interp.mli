(** The module for interpreting nkpl commands from strings. *)

type result = Success of string option * Trace.t option | Fail of string option * Trace.t option

(** Parse a string as a nkpl program. *)
val parse_string : (string -> unit) -> Env.t -> string -> Nkcmd.t option

(** Opens a file by its filename and parses the contents. *)
val parse_file : (string -> unit) -> string -> Nkcmd.t list

(** Interprets / executes the nkpl command. *)
val interp : (string -> unit) -> string -> (Env.t * Value.S.t Field.M.t option) -> Nkcmd.t -> ((Env.t * Value.S.t Field.M.t option) * result list)

(** Interprets a string as a nkpl program. *)
val interp_string : (string -> unit) -> (Env.t * Value.S.t Field.M.t option) -> string -> ((Env.t * Value.S.t Field.M.t option) * result list)

(** Opens a file by its filename and interprets the contents; returns the resulting [Env.t]. *)
val interp_file : (string -> unit) -> string -> (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list)

(** Parse a string as a full nkpl program (multiple commands), as opposed to [parse_string]
    which only accepts a single command. *)
val parse_program_string : (string -> unit) -> Env.t -> string -> Nkcmd.t list

(** Interprets a string as a full nkpl program (multiple commands), as opposed to [interp_string]
    which only accepts a single command. *)
val interp_program_string : (string -> unit) -> (Env.t * Value.S.t Field.M.t option) -> string -> (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list)
val interp_cmds_with_env : (string -> unit) -> (Env.t * Value.S.t Field.M.t option) -> string -> Nkcmd.t list -> (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list)
val interp_file_with_env : (string -> unit) -> (Env.t * Value.S.t Field.M.t option) -> string -> (string * (Nkcmd.t list * (Env.t * Value.S.t Field.M.t option) * result list))

val result_list_to_json : result list -> string
