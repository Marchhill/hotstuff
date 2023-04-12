open Capnp_rpc_lwt
open Lwt.Syntax
open Types
open Api_wrapper
open Net
open Util

let rec do_actions s = function
	| [] -> Lwt.return_unit
	| a::actions ->
		let _t1 = Time_now.nanoseconds_since_unix_epoch () in
		let* () = (match a with
			| Consensus.Broadcast m ->
				Lwt_list.iter_s (fun conn ->
					send_msg conn m s.stats) s.conns
			| Consensus.SendLeader m ->
				let leader = Consensus.leader_id m.view !(s.state_machine).node_count in
				let conn = List.nth s.conns leader in
				send_msg conn m s.stats
			| Consensus.SendNextLeader m ->
				let leader = Consensus.leader_id (m.view + 1) !(s.state_machine).node_count in
				let conn = List.nth s.conns leader in
				send_msg conn m s.stats
			| Consensus.SendClient x ->
				(match Hashtbl.find_opt s.client_callbacks x.callback_id with
					| Some callback ->
						(match callback with
							| Some c -> 
								(* Fmt.pr "%d: %s@." id x.callback_id; *)
								Hashtbl.replace s.client_callbacks x.callback_id None; (* remove from table to prevent waking up twice*)
								Lwt.wakeup c x.success
							| None -> () (* Fmt.pr "%d: %s xxx@." id x.callback_id;*)
						)
						
					| None -> ());
				Lwt.return_unit
			| Consensus.Execute _ ->
				Lwt.return_unit
			| Consensus.ResetTimer t ->
				let t_n = Time_now.nanoseconds_since_unix_epoch () in
				let elapsed = delta !(s.stats.last_view) t_n in
				let view_times =  elapsed :: !(s.stats.view_times) in
				s.stats.view_times := view_times;
				s.stats.last_view := t_n;
				s.reset_timer (fun view ->
					let event = (Timeout {view = view} : Consensus.event) in
					s.push_msg (Some (event, Time_now.nanoseconds_since_unix_epoch ()));
				) t.view;
				Lwt.return_unit
		) in
		let _t2 = Time_now.nanoseconds_since_unix_epoch () in
		(* Fmt.pr "%d: action %s elapsed %fs@." id (Consensus.get_action_type a) (delta t1 t2); *)
		do_actions s actions

(* hardcode same pk for all nodes to avoid pki! ??? *)
let gen_key _ =
	let sk = Tezos_crypto.Aggregate_signature.Secret_key.of_b58check_exn "BLsk2bwzMPpXwy9hoMuV7MP6muK8NLKMWRVUNAaMs7ZDJwahUhtadY" in
	let pk = Tezos_crypto.Aggregate_signature.Secret_key.to_public_key sk in
	let pkh = Tezos_crypto.Aggregate_signature.Public_key.hash pk in
	(pkh, pk, sk)

(* get our secret key and a list of public ketys for all nodes *)
let get_keys id nodes =
	let keys = List.init nodes gen_key in
	let _, _, sk = List.nth keys id in
	let pks = List.map (fun (_, x, _) -> x) keys in
	(sk, pks)

let init id nodes timeout batch_size verbose =
	let _sk, _pks = get_keys id nodes in (* generate public and private keys *)
	let crypto = Some ({sk = _sk; pks = _pks} : Consensus.crypto) in
	let initial_state, new_view_actions = Consensus.create_state_machine id nodes batch_size ~crypto in (* initialise state machine *)
	let conns = open_conns nodes in (* connect to other nodes *)
	let client_callbacks = Hashtbl.create 1000000 in (* store callbacks to respond to client commands *)
	let reset_timer = create_timer timeout in (* create a view timer *)
	let msgs, push_msg = Lwt_stream.create () in
	let reqs, push_req = Lwt_stream.create () in
	(* send new-view message to first leader and start timer for first view *)
	let reset_timer_action = (ResetTimer {id = id; view = 1} : Consensus.action) in
	let actions = reset_timer_action :: new_view_actions in
	let s = {
		state_machine = ref initial_state;
	alive = ref true;
		verbose = verbose; conns = conns;
		client_callbacks = client_callbacks;
		reset_timer = reset_timer;
		msgs = msgs;
		push_msg = push_msg;
		reqs = reqs;
		push_req = push_req;
		iter_count = ref 0;
		stats = empty_stats (Time_now.nanoseconds_since_unix_epoch ())
	} in
	Lwt.async (fun () -> do_actions s actions);
	s

let local s =
	let module Hs = Api.Service.Hs in
	Hs.local @@ object
		inherit Hs.service

		(* message from another node *)
		method send_msg_impl params release_param_caps =
			let open Hs.SendMsg in
			let t1 = Time_now.nanoseconds_since_unix_epoch () in
			let msg = Params.msg_get params in
			release_param_caps ();
			(* convert from api type to consensus machine internal type *)
			let event = api_msg_to_consensus_event msg in
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
			let data = Api.Reader.Cmd.data_get api_cmd in
			let callback_id = Api.Reader.Cmd.id_get api_cmd in
			release_param_caps ();
			(* create a consensus command from the client's request*)
			let cmd = ({data = data; callback_id = callback_id} : Consensus.cmd) in
			(* store callback so we can respond to client later *)
			let res, callback = Lwt.wait () in
			Hashtbl.add s.client_callbacks callback_id (Some callback);
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

let get_events s =
	(* Fmt.pr "loop %d@." !(s.iter_count); *)
	let msg_events = List.map (fun e -> (e, false)) (Lwt_stream.get_available s.msgs) in
	let req_events = List.map (fun e -> (e, true)) (Lwt_stream.get_available s.reqs) in
	msg_events @ req_events

let rec main_loop s =
	let* () = if !(s.alive) then (
		s.iter_count := !(s.iter_count) + 1;
		(* take an event from the incoming stream, prioritise internal messages  *)
		let events = get_events s in
		Lwt_list.iter_s (fun ((event, queue_time), is_req) ->
			let t1 = Time_now.nanoseconds_since_unix_epoch () in
			(* advance consensus state machine by delivering the new event *)
			let (state_machine', actions) = Consensus.advance !(s.state_machine) event in
			s.state_machine := state_machine';
			let t2 = Time_now.nanoseconds_since_unix_epoch () in
			(* carry out actions returned by state machine *)
			let* () = do_actions s actions in
			let t3 = Time_now.nanoseconds_since_unix_epoch () in
			(* record stats / output events *)
			let () = if s.verbose then (
			Consensus.print_state state_machine';
			List.iter (Consensus.print_action) actions;
			) in
			(if is_req then
			s.stats.req_queue_times := (delta queue_time t1) :: !(s.stats.req_queue_times)
			else
			s.stats.msg_queue_times := (delta queue_time t1) :: !(s.stats.msg_queue_times)
			);
			s.stats.advance_times := (delta t1 t2) :: !(s.stats.advance_times);
			s.stats.action_times := (delta t2 t3) :: !(s.stats.action_times);
			Lwt.return_unit
		) events
  	)
	else
		Lwt.return_unit
	in
	let* () = if (!(s.iter_count) mod 10000) = 0 then Lwt.pause () else Lwt.return_unit in
	main_loop s