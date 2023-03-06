include module type of Types
include module type of Util

type state
type t = {view: int; id: int; node_count: int; cmds: Cmd_set.t; commited: Cmd_set.t; crypto: crypto option; complain: (int, event list) Hashtbl.t; s: state}

(** [create_node id nodes pks useCrypto] returns the initialised state machine. *)
val create_state_machine : ?crypto:crypto option -> int -> int -> t * (action list)

(** [advance t event] applies the event to the state machine and returns the updated state machine and any actions to take. If this fails it returns an error message *)
(* assumes at most once delivery *)
val advance : t -> event -> t * (action list)

val create_node_internal : int option -> node_justify option -> node_internal option
val print_state : t -> unit
val get_qc_0 : unit -> qc option
val get_b_0 : unit -> node option