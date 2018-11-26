(** Symbol table interface to implement var map *)

type 'a state

val initialize : unit -> 'a state

val enter : 'a state -> string -> 'a -> unit

val look : 'a state -> string -> 'a option

val begin_scope : 'a state -> unit

val end_scope : 'a state -> unit

val set_read_only : 'a state -> string -> unit

val get_read_only : 'a state -> string -> bool

val add_is_missing_fun_def : 'a state -> string -> unit

val remove_is_missing_fun_def : 'a state -> string -> unit

val is_missing_fun_def : 'a state -> string -> bool

val some_fun_is_missing_def : 'a state -> bool

val is_global : 'a state -> string -> bool

val unsafe_clear_symbol_table : 'a state -> unit

(* TODO: the following is very ugly, but we seem to need something like it to
 reproduce the (strange) behaviour in the current Stan that local variables
 have a block level that is determined by what has been assigned to them
 rather than by where they were declared. I'm not sure that behaviour makes
 sense unless we use static analysis as well to make sure these assignments
 actually get evaluated in that phase. *)

val unsafe_replace : 'a state -> string -> 'a -> unit
