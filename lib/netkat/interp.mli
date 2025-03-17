(** The module for interpreting nkpl commands from strings. *)

(** Interpret / execute the nkpl command *)
val interp : string -> Env.t -> Nkcmd.t -> Env.t

(** Interpret a string as a nkpl program *)
val interp_string : Env.t -> string -> Env.t

(** Open a file by its filename and interpret the contents; return the resulting Env.t *)
val interp_file : string -> Env.t
