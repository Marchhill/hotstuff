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

type ('a, 'b) node_state = {
	state_machine: Consensus.t ref;
	verbose: bool;
	conns: ('a Capnp_rpc_lwt.Sturdy_ref.t * Api_wrapper.Api.Client.Hs.t Capnp_rpc_lwt.Capability.t Lwt.t ref) list;
	client_callbacks: (string, bool Lwt.u option) Hashtbl.t;
	reset_timer: ('b -> unit) -> 'b -> unit;
	msgs: (Consensus.event * Base.Int63.t) Lwt_stream.t;
	push_msg: (Consensus.event * Base.Int63.t) option -> unit;
  reqs: (Consensus.event * Base.Int63.t) Lwt_stream.t;
	push_req: (Consensus.event * Base.Int63.t) option -> unit;
	iter_count: int ref;
  next_beat: Base.Int63.t ref;
  beat_interval: Base.Int63.t;
	stats: stats;
}