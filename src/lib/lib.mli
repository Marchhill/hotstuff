(* val start_server : int -> int -> int -> float -> bool -> 'a Lwt.t

type stats = {
	last_view: Base.Int63.t ref;
	view_times: float list ref;
	advance_times: float list ref;
	action_times: float list ref;
	connection_times: float list ref;
	send_times: float list ref;
	sizes: float list ref;
	recv_msg_times: float list ref;
	recv_req_times: float list ref;
	res_times: float list ref;
	msg_queue_times: float list ref;
	req_queue_times: float list ref;
}

val empty_stats : Base.Int63.t -> stats *)

include module type of Types
include module type of Server
include module type of Net
include module type of Util