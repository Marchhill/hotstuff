open Capnp_rpc_lwt
open Lwt.Syntax
open Types
open Util

let secret_key = `Ephemeral

let serve s =
	let module Hs = Api_wrapper.Api.Service.Hs in
	Hs.local @@ object
		inherit Hs.service

		(* message from another node *)
		method send_msg_impl params release_param_caps =
			let open Hs.SendMsg in
			let t1 = Time_now.nanoseconds_since_unix_epoch () in
			let msg = Params.msg_get params in
			release_param_caps ();
			(* convert from api type to consensus machine internal type *)
			let event = Api_wrapper.api_msg_to_consensus_event msg in
			let () =
				if s.verbose then (
					Fmt.pr "%d: recv " (!(s.state_machine)).id;
					Consensus.print_event event;
				)
			in
			(* add event to stream *)
			s.push_msg (Some (event, Time_now.nanoseconds_since_unix_epoch ()));
			let t2 = Time_now.nanoseconds_since_unix_epoch () in
			s.stats.recv_msg_times := (delta t1 t2) :: !(s.stats.recv_msg_times);
			Service.return_empty ()
		
		(* request from client to commit some command *)
		method client_req_impl params release_param_caps =
			let open Hs.ClientReq in
			let t1 = Time_now.nanoseconds_since_unix_epoch () in
			(* get parameters *)
			let api_cmd = Params.cmd_get params in
			release_param_caps ();
			(* create a consensus command from the client's request*)
			let cmd = Api_wrapper.api_cmd_to_cmd api_cmd in
			(* store callback so we can respond to client later *)
			let res, callback = Lwt.wait () in
			Hashtbl.add s.client_callbacks cmd.callback_id (Some callback);
			(* create a consensus event from the client's command *)
			let event = (ClientCmd cmd : Consensus.event) in
			(* add event to stream *)
			s.push_req (Some (event, Time_now.nanoseconds_since_unix_epoch ()));
			let t2 = Time_now.nanoseconds_since_unix_epoch () in
			s.stats.recv_req_times := (delta t1 t2) :: !(s.stats.recv_req_times);
			let t1 = Time_now.nanoseconds_since_unix_epoch () in
			Service.return_lwt (fun () ->
				(* wait to send response until callback resolver is woken *)
				let* success = res in
				let response, results = Service.Response.create Results.init_pointer in
				Results.success_set results success;
				let t2 = Time_now.nanoseconds_since_unix_epoch () in
				s.stats.res_times := (delta t1 t2) :: !(s.stats.res_times);
				(* Fmt.pr "%d: yeet@." !(s.state_machine).id; *)
				Lwt_result.return response
			)
		
		(* request from client to end our experiment and quit *)
		method quit_impl _params release_param_caps =
			release_param_caps ();
			(* print statistics before exiting *)
			print_stats !(s.stats.view_times) "view" "s" !(s.state_machine).id;
			print_stats !(s.stats.send_times) "send" "s" !(s.state_machine).id;
			print_stats !(s.stats.connection_times) "conn" "s" !(s.state_machine).id;
			print_stats !(s.stats.advance_times) "advance" "s" !(s.state_machine).id;
			print_stats !(s.stats.action_times) "action" "s" !(s.state_machine).id;
			print_stats !(s.stats.recv_msg_times) "recv_msg" "s" !(s.state_machine).id;
			print_stats !(s.stats.recv_req_times) "recv_req" "s" !(s.state_machine).id;
			print_stats !(s.stats.res_times) "res_times" "s" !(s.state_machine).id;
			print_stats !(s.stats.req_queue_times) "req_queue_times" "s" !(s.state_machine).id;
			print_stats !(s.stats.msg_queue_times) "msg_queue_times" "s" !(s.state_machine).id;
			s.alive := false;
	  Service.return_empty ()
	end

(* intialise internal state *)
let init_state id nodes timeout batch_size verbose =
	let _sk, _pks = Util.gen_keys id nodes in (* generate public and private keys *)
	let crypto = Some ({sk = _sk; pks = _pks} : Consensus.crypto) in
        (* let crypto = None in *)
	let initial_state, new_view_actions = Consensus.create_state_machine id nodes batch_size ~crypto in (* initialise state machine *)
	let conns = Msg_sender.open_conns nodes in (* connect to other nodes *)
	let client_callbacks = Hashtbl.create 1000000 in (* store callbacks to respond to client commands *)
	let reset_timer = Util.create_timer timeout in (* create a view timer *)
	let msgs, push_msg = Lwt_stream.create () in
	let reqs, push_req = Lwt_stream.create () in
	(* send new-view message to first leader and start timer for first view *)
	let reset_timer_action = (ResetTimer {id = id; view = 1} : Consensus.action) in
	let actions = reset_timer_action :: new_view_actions in
	let s = {
		state_machine = ref initial_state;
		alive = ref true;
		verbose = verbose;
		conns = conns;
		client_callbacks = client_callbacks;
		reset_timer = reset_timer;
		msgs = msgs;
		push_msg = push_msg;
		reqs = reqs;
		push_req = push_req;
		iter_count = ref 0;
		stats = Util.empty_stats (Time_now.nanoseconds_since_unix_epoch ())
	} in
	Lwt.async (fun () -> Action_handler.do_actions s actions);
	s

let start_server id nodes batch_size timeout verbose =
	let listen_address = `TCP ("127.0.0.1", 9000 + id) in
	(* let listen_address = `TCP ("0.0.0.0", 9000) in *)
	let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
	let service_id = Capnp_rpc_net.Restorer.Id.public "" in
	let node_state = init_state id nodes timeout batch_size verbose in
	let restore = Capnp_rpc_net.Restorer.single service_id (serve node_state) in
	let* vat = Capnp_rpc_unix.serve config ~restore in
	let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
	Fmt.pr "Server ID=%s running. Connect to URI %S.@." (Int.to_string id) (Uri.to_string uri);
	Lwt.return node_state