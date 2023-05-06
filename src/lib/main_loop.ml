open Lwt.Syntax
open Types

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