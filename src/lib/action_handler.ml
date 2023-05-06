open Lwt.Syntax
open Types

let rec do_actions s = function
	| [] -> Lwt.return_unit
	| a::actions ->
		let _t1 = Time_now.nanoseconds_since_unix_epoch () in
		let* () = (match a with
			| Consensus.Broadcast m ->
				Lwt_list.iter_s (fun conn -> Net.send_msg conn m s.stats) s.conns
			| Consensus.SendLeader m ->
				let leader = Consensus.leader_id m.view !(s.state_machine).node_count in
				let conn = List.nth s.conns leader in
				Net.send_msg conn m s.stats
			| Consensus.SendNextLeader m ->
				let leader = Consensus.leader_id (m.view + 1) !(s.state_machine).node_count in
				let conn = List.nth s.conns leader in
				Net.send_msg conn m s.stats
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
				let elapsed = Util.delta !(s.stats.last_view) t_n in
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