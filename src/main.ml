open Cmdliner
open Lwt.Syntax
open Types

let secret_key = `Ephemeral

(* intialise internal state *)
let init id nodes timeout batch_size verbose =
	let _sk, _pks = Util.gen_keys id nodes in (* generate public and private keys *)
	let crypto = Some ({sk = _sk; pks = _pks} : Consensus.crypto) in
        (*let crypto = None in*)
	let initial_state, new_view_actions = Consensus.create_state_machine id nodes batch_size ~crypto in (* initialise state machine *)
	let conns = Net.open_conns nodes in (* connect to other nodes *)
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

(* main loop delivers events to consensus state machine *)
let rec main_loop s =
	let* () = if !(s.alive) then (
		s.iter_count := !(s.iter_count) + 1;
		(* take an event from the incoming stream, prioritise internal messages  *)
		let msg_events = List.map (fun e -> (e, false)) (Lwt_stream.get_available s.msgs) in
		let req_events = List.map (fun e -> (e, true)) (Lwt_stream.get_available s.reqs) in
		let events = msg_events @ req_events in
		(* handle events *)
		Lwt_list.iter_s (fun ((event, queue_time), is_req) ->
			let t1 = Time_now.nanoseconds_since_unix_epoch () in
			(* advance consensus state machine by delivering the new event *)
			let (state_machine', actions) = Consensus.advance !(s.state_machine) event in
			s.state_machine := state_machine';
			let t2 = Time_now.nanoseconds_since_unix_epoch () in
			(* carry out actions returned by state machine *)
			let* () = Action_handler.do_actions s actions in
			let t3 = Time_now.nanoseconds_since_unix_epoch () in
			(* record stats / output events *)
			if s.verbose then (
				Consensus.print_state state_machine';
				List.iter (Consensus.print_action) actions;
			);
			(* record statistics *)
			(if is_req then
				s.stats.req_queue_times := (Util.delta queue_time t1) :: !(s.stats.req_queue_times)
			else
				s.stats.msg_queue_times := (Util.delta queue_time t1) :: !(s.stats.msg_queue_times)
			);
			s.stats.advance_times := (Util.delta t1 t2) :: !(s.stats.advance_times);
			s.stats.action_times := (Util.delta t2 t3) :: !(s.stats.action_times);
			Lwt.return_unit
		) events
  	)
	else
		Lwt.return_unit
	in
	let* () = if (!(s.iter_count) mod 5) = 0 then Lwt.pause () else Lwt.return_unit in
	main_loop s

let start_node id nodes batch_size timeout verbose =
	Lwt_main.run begin
		(* let listen_address = `TCP ("0.0.0.0", 9000) in *)
		let listen_address = `TCP ("127.0.0.1", 9000 + id) in
		let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
		let service_id = Capnp_rpc_net.Restorer.Id.public "" in
    let node_state = init id nodes timeout batch_size verbose in
		let restore = Capnp_rpc_net.Restorer.single service_id (Server.local node_state) in
		let* vat = Capnp_rpc_unix.serve config ~restore in
		let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
		Fmt.pr "Server ID=%s running. Connect to URI %S.@." (Int.to_string id) (Uri.to_string uri);
		main_loop node_state
	end

let id =
	let doc = "Node ID. Server will run on port 9000 + ID" in
	Arg.(value & opt int 0 & info ["i"; "id"] ~docv:"ID" ~doc)

let nodes =
	let doc = "Node count." in
	Arg.(value & opt int 0 & info ["n"; "nodes"] ~docv:"NODES" ~doc)

let verbose =
	let doc = "Output info about messages and state machine." in
	Arg.(value & flag & info ["v"; "verbose"] ~docv:"VERBOSE" ~doc)

let batch_size =
	let doc = "Batch size." in
	Arg.(value & opt int 300 & info ["b"; "batch"] ~docv:"BATCH" ~doc)

let timeout =
	let doc = "View timeout." in
	Arg.(value & opt float 1000. & info ["t"; "timeout"] ~docv:"TIMEOUT" ~doc)

let cmd =
	let doc = "run a hotstuff node" in
	let info = Cmd.info "hs" ~version:"%‌%VERSION%%" ~doc in
	Cmd.v info Term.(const start_node $ id $ nodes $ batch_size $ timeout $ verbose)

let () =
	(* Util.init_logging () *)
	(* Memtrace.trace_if_requested (); *)
	Random.self_init ();
	exit (Cmd.eval cmd);
