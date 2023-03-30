open Types
open Util
open Crypto

type state = {v: (int, event list) Hashtbl.t; vheight: int; b_lock: node; b_exec: node; b_leaf: node; qc_high: qc}
type t = {view: int; id: int; node_count: int; cmds: Cmd_set.t;  commited: Cmd_set.t; crypto: crypto option; complain: (int, event list) Hashtbl.t; s: state}

let b_0_justify = {node_offset = 0; view = 0; signature = None; msg_type = GenericAck; ids = []}
let b_0 = make_node Cmd_set.empty None (Some {justify = b_0_justify; height = 1})
let qc_0 = {node = Some b_0; view = 0; signature = None; msg_type = GenericAck; ids = []}

let get_qc_0 () = Some qc_0
let get_b_0 () = Some b_0

let print_state state = Fmt.pr "state id=%d view=%d qc_high=(%s) cmd=(%s) v=[%s]@."
	state.id
	state.view
	(qc_to_string (Some state.s.qc_high))
	(Cmd_set.fold (fun cmd acc -> acc ^ cmd.data ^ ",") state.cmds "")
	(List.fold_left (fun acc e -> get_event_type e ^ ", " ^ acc) "" (match Hashtbl.find_opt state.s.v state.view with Some l -> l | None -> []))

let get_node_height n = match n.i with
	| Some i -> i.height
	| None -> raise NodeInternalException

let get_node_justify n = match n.i with
	| Some i -> i.justify
	| None -> raise NodeInternalException

let get_node_from_qc (qc : qc) =
	match qc.node with
		| Some n -> n
		| None -> raise MissingNodeException

let qc_from_node_justify n =
	let justify = get_node_justify n in
	{node = Some (node_nth justify.node_offset n); view = justify.view; signature = justify.signature; msg_type = justify.msg_type; ids = justify.ids}

let update_qc_high state (qc'_high : qc) =
	let n' = get_node_from_qc qc'_high in
	let n = get_node_from_qc state.s.qc_high in
	if (get_node_height n') > (get_node_height n) then
		{state with s = {state.s with qc_high = qc'_high; b_leaf = n'}}
	else
		state

let rec add_dummy_nodes (n : node) = function
	| 0 -> n
	| x -> add_dummy_nodes (make_node Cmd_set.empty (Some n) (Some {height = get_node_height n; justify = get_node_justify n})) (x - 1)

let rec trim_node (n : node) x = match n.parent, x with
	| _, 0 -> {n with parent = None}
	| Some p, x -> {n with parent = Some (trim_node p (x - 1))}
	| None, _ -> n

let _create_leaf state (parent : node) (cmds : Cmd_set.t) (qc : qc) (height : int) =
	let b'' = get_node_from_qc qc in
	let b' = get_node_from_qc (qc_from_node_justify b'') in
	let b = get_node_from_qc (qc_from_node_justify b') in
	let cutoff = min (get_node_height b) ((get_node_height b') - 1) in (* calculate cutoff of how much history to send *)
	let offset = state.view - height + 1 in
	(* Fmt.pr "%d: create leaf view = %d parent = (%s) cmd = (%s) qc = (%s) height = %d offset = %d@." state.id state.view (node_to_string (Some parent)) cmd.data (qc_to_string (Some qc)) height offset; *)
	let parent = add_dummy_nodes parent offset in
	(* Fmt.pr "%d: parent with dummys = (%s)@." state.id (node_to_string (Some parent)); *)
	let justify = {node_offset = offset + 1; view = qc.view; signature = qc.signature; msg_type = qc.msg_type; ids = qc.ids} in (* ??? change offset if skipped? *)
	let n = make_node cmds (Some parent) (Some {justify = justify; height = state.view + 1}) in
	trim_node n (state.view + 1 - cutoff) (* only send nodes that will be used*)

let create_leaf _state (parent : node) (cmds : Cmd_set.t) (qc : qc) (height : int) =
	let justify = {node_offset = 1; view = qc.view; signature = qc.signature; msg_type = qc.msg_type; ids = qc.ids} in (* ??? change offset if skipped? *)
	make_node cmds (Some parent) (Some {justify = justify; height = height})

let rec on_commit (state : t) = function
	| Some b ->
		if (get_node_height b) > (get_node_height state.s.b_exec) then (
			(*let state, actions = (match Queue.peek_opt state.exec with
				| Some cmd when (cmd.callback_id = b.cmd.callback_id) ->
					let _ = Queue.pop state.exec in
					(state, [Execute {id = state.id; node = b}; SendClient {id = state.id; callback_id = cmd.callback_id; success = true}])
				| _ -> (state, [Execute {id = state.id; node = b}])
			) in*)
			(* List.filter (fun i -> Set.mem committed i) cmds *)
			(* Fmt.pr "commit!@."; *)
			let cmds = Cmd_set.diff b.cmds state.commited in
			let actions = (Execute {id = state.id; node = b}) :: (Cmd_set.fold (fun cmd acc ->
				if cmd.callback_id = "" then
					acc
				else
					(Fmt.pr "%s@." cmd.data;
					(SendClient {id = state.id; callback_id = cmd.callback_id; success = true})::acc)
			) cmds []) in
			let state = {state with commited = (Cmd_set.union state.commited cmds)} in
			let state, actions' = (on_commit state b.parent) in
			(state, actions' @ actions)
		)
		else (on_commit state b.parent)
	| _ -> (state, [])

let update (state: t) (b_star : node) =
	let qc_high = qc_from_node_justify b_star in (* convert offset into actual node for storage *)
	let b'' = get_node_from_qc qc_high in
	let b' = get_node_from_qc (qc_from_node_justify b'') in
	let b = get_node_from_qc (qc_from_node_justify b') in
	let state = update_qc_high state qc_high in
	let state = if (get_node_height b') > (get_node_height state.s.b_lock) then
		{state with s = {state.s with b_lock = b'}}
	else state in
	if (equal_nodes b''.parent (Some b')) && (equal_nodes b'.parent (Some b)) then
		let state, actions = on_commit state (Some b) in
		({state with s = {state.s with b_exec = b}}, actions)
  	else (state, [])

let on_propose state cmds =
	let b_new = create_leaf state state.s.b_leaf cmds state.s.qc_high ((get_node_height state.s.b_leaf) + 1) in
	let broadcast_msg = sign state.crypto ({id = state.id; view = state.view; msg_type = Generic; node = Some b_new; justify = Some state.s.qc_high; partial_signature = None}) in
	(b_new, [Broadcast broadcast_msg])

let on_beat state cmds =
	let (b_new, actions) = on_propose state cmds in
	({state with s = {state.s with b_leaf = b_new}}, actions)

(* transition to view v *)
let on_next_sync_view state view =
	let state = {state with view = view} in
	let new_view_msg = sign state.crypto ({id = state.id; view = state.view; msg_type = NewView; node = None; justify = Some state.s.qc_high; partial_signature = None}) in
	(state, [ResetTimer {id = state.id; view = view}; SendNextLeader new_view_msg])

let delta x y =
	let open Base.Int63 in
	(to_float (y - x)) /. 1_000_000_000.

let on_recieve_proposal state msg =
	let b_new = (match msg.node with Some node -> node | None -> raise MissingNodeException) in
	let n = get_node_from_qc (qc_from_node_justify b_new) in
	let (state, actions1) = if ((get_node_height b_new) > state.s.vheight) && ((extends (Some b_new) (Some state.s.b_lock)) || (get_node_height n) > (get_node_height state.s.b_lock)) then
		let vote_msg = sign state.crypto ({id = state.id; view = state.view; msg_type = GenericAck; node = Some b_new; justify = Some state.s.qc_high; partial_signature = None}) in
		({state with s = {state.s with vheight = (get_node_height b_new)}}, [SendNextLeader vote_msg])
	else (state, []) in
	let (state, actions2) = update state b_new in
	(state, actions1 @ actions2)

let on_recieve_new_view state msg =
	match msg.justify with
		| Some qc ->
			(*let state = update_qc_high state qc in
			let state, actions = if msg.view > state.view then (
				on_next_sync_view state msg.view
			) else
				(state, [])
			in
			(state, actions)*)
			(update_qc_high state qc, [])
		| None ->
			raise MissingQcException

let on_recieve_vote state event view =
	match (add_event state.s.v event view state.node_count) with
		| Some q ->
			let qc = threshold_qc state.crypto q in
			(* set new qc as votes as highest *)
			(* let state = update_qc_high state qc in *)
			(* transition to next state*)
			(* on_next_sync_view state (view + 1) *)
			(update_qc_high state qc, [])
		| None ->
			(state, [])

let create_state_machine ?(crypto = None) id node_count =
	let s = {v = (Hashtbl.create 10000); vheight = 1; b_lock = b_0; b_exec = b_0; b_leaf = b_0; qc_high = qc_0} in
	let state = {view = 1; id = id; node_count = node_count; crypto = crypto; cmds = Cmd_set.empty; commited = Cmd_set.empty; complain = (Hashtbl.create 1000); s = s} in
	if (is_leader 1 id node_count) then
		on_next_sync_view state 1
	else
		(state, [])

let advance (state : t) (event : event) =
	(* let is_next_leader = is_leader (state.view + 1) state.id state.node_count in *)
	let is_signed = verify_partial_signature state.crypto event in
	let qc = (
		match get_msg_from_event event with
		| Some msg -> if verify_threshold_qc state.crypto state.node_count (Some qc_0) msg.justify then msg.justify else None
		| None -> None
	) in
	let state, actions = (match qc with
		| Some qc when (qc.view + 1) > state.view ->
				Fmt.pr "catching up!@.";
				(* let actions = if qc.msg_type = Complain then (
						nextview mesage clears pipeline
						let fail_messages = Queue.fold (fun acc cmd ->
							acc @ [SendClient {id = state.id; callback_id = cmd.callback_id; success = false}]
						) [] state.exec in
						Queue.clear state.exec;
						fail_messages
				) else [] in
				*)
				(* qc proves we should transition to state qc.view + 1 *)
				let state, actions' = on_next_sync_view state (qc.view + 1) in
				(state, actions')
		| _ ->
			(state, [])
	)
	in
	let state, actions' = if is_signed then
		(match event with
			| GenericAck msg when (is_leader (msg.view + 1) state.id state.node_count) && msg.view >= state.view -> on_recieve_vote state event msg.view
			| Generic msg when msg.view = state.view -> on_recieve_proposal state msg
			| NewView msg when (Option.is_some qc) && msg.view >= state.view -> on_recieve_new_view state msg
			| ClientCmd cmd ->
				({state with cmds = Cmd_set.add cmd state.cmds}, [])
			| Timeout x ->
				let complain_msg = sign state.crypto ({id = state.id; view = x.view; msg_type = Complain; node = None; justify = Some state.s.qc_high; partial_signature = None}) in
				(state, [
					ResetTimer {id = state.id; view = (x.view + 1)}; (* start timeout for next view *)
					SendNextLeader complain_msg (* complain to next leader *)
					])
			| Complain msg when msg.view >= state.view && (is_leader (msg.view + 1) state.id state.node_count) ->
					(match (add_event state.complain event msg.view state.node_count) with
						| Some q ->
							let complainQC = Some (threshold_qc state.crypto q) in
							let broadcast_msg = sign state.crypto ({id = state.id; view = msg.view; msg_type = NextView; node = None; justify = complainQC; partial_signature = None}) in
							(
								state,
								[Broadcast broadcast_msg]
							)
						| None -> (state, [])
					)
			| Beat ->
				if (is_leader state.view state.id state.node_count) then (
					(* Fmt.pr "%d: beat!@." state.id; *)
					let cmds = Cmd_set.diff state.cmds state.commited in
					let state = {state with cmds = Cmd_set.empty} in
					on_beat state cmds
				)
				else
					(state, [])
			| NextSyncView ->
				on_next_sync_view state (state.view + 1)
			| _ -> (state, [])
		)
	else
		(state, [])
	in
	(state, actions @ actions')

let create_node_internal height justify =
	match height, justify with
		| Some height, Some justify -> Some {height = height; justify = justify}
		| _ -> raise NodeInternalException