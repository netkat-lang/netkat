(** Interpret / execute the nkpl command *)
val interp : Env.t -> Nkcmd.t -> Env.t

(** Open a file by its filename and interpret the contents; return the resulting Env.t *)
val interp_file : string -> Env.t
