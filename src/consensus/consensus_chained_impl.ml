open Types
open Util
open Crypto

type state = {v: (int, event list) Hashtbl.t; vheight: int; b_lock: node; b_exec: node; b_leaf: node; qc_high: qc; tcp_lens: int list}
type t = {view: int; id: int; node_count: int; batch_size: int; cmds: Cmd_set.t;  seen: Cmd_set.t; crypto: crypto option; complain: (int, event list) Hashtbl.t; s: state}

let b_0_justify = {node_offset = 0; view = 0; signature = None; msg_type = GenericAck; ids = []}
let b_0 = make_node Cmd_set.empty None (Some {justify = b_0_justify; height = 1})
let qc_0 = {node = Some b_0; view = 0; signature = None; msg_type = GenericAck; ids = []}

let get_qc_0 () = Some qc_0
let get_b_0 () = Some b_0

let print_state state = Fmt.pr "state id=%d view=%d qc_high=(%s) cmd=(%s)@."
	state.id
	state.view
	(qc_to_string (Some state.s.qc_high))
	(Cmd_set.fold (fun cmd acc -> acc ^ cmd.data ^ ",") state.cmds "")

let update_qc_high state (qc'_high : qc) =
	let n' = get_node_from_qc qc'_high in
	let n = get_node_from_qc state.s.qc_high in
	if (get_node_height n') > (get_node_height n) then
		{state with s = {state.s with qc_high = qc'_high; b_leaf = n'}}
	else
		state

let update_tcp_lens state (tcp_lens' : int list) =
	let tcp_lens = List.mapi (fun i l ->
		let l' = List.nth tcp_lens' i in
		let l' = if i = state.id then max l' (get_node_height state.s.b_exec) else l' in
		max l l') state.s.tcp_lens in
    {state with s = {state.s with tcp_lens = tcp_lens}}

let create_leaf state (parent : node) (cmds : Cmd_set.t) (qc : qc) =
	let _cutoff = List.fold_left min Int.max_int state.s.tcp_lens in
	let offset = state.view - (get_node_height parent) in
	let parent = add_dummy_nodes parent offset in
	let justify = {node_offset = offset + 1; view = qc.view; signature = qc.signature; msg_type = qc.msg_type; ids = qc.ids} in (* ??? change offset if skipped? *)
	let n = make_node cmds (Some parent) (Some {justify = justify; height = state.view + 1}) in
	trim_node n (state.view + 1 - _cutoff) (* only send nodes that will be used *)

let rec on_commit (state : t) = function
	| Some b ->
		if (get_node_height b) > (get_node_height state.s.b_exec) then (
			let actions = (Execute {id = state.id; node = b}) :: (Cmd_set.fold (fun cmd acc ->
				if (cmd.callback_id = 0) then
					acc
				else
					(SendClient {id = state.id; callback_id = cmd.callback_id; success = true})::acc
			) b.cmds []) in
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
	let state = if (get_node_height b') > (get_node_height state.s.b_lock) then (
		{state with s = {state.s with b_lock = b'}}
  )
	else state in
	if (equal_nodes b''.parent (Some b')) && (equal_nodes b'.parent (Some b)) then (
		let state, actions = on_commit state (Some b) in
		let state = {state with s = {state.s with b_exec = b}} in
		let state = update_tcp_lens state state.s.tcp_lens in
		(state, actions)
  )
  else (state, [])

let on_propose state cmds =
	let b_new = create_leaf state state.s.b_leaf cmds state.s.qc_high in
	let broadcast_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = state.s.tcp_lens; msg_type = Generic; node = Some b_new; justify = Some state.s.qc_high; partial_signature = None}) in
	(b_new, [Broadcast broadcast_msg])

let on_beat state cmds =
	let (b_new, actions) = on_propose state cmds in
	({state with s = {state.s with b_leaf = b_new}}, actions)

(* transition to view v *)
let on_next_sync_view state view =
	let state = {state with view = view} in
	let (state, actions) = if (is_leader state.view state.id state.node_count) then (
		let filtered = Cmd_set.diff state.cmds state.seen in
        (* let filtered = state.cmds in *)
		let i = ref 0 in
    	(* limit batch size *)
		let cmds, rest = Cmd_set.partition (fun _ -> i := !i + 1; !i <= state.batch_size) filtered in
		let state = {state with cmds = rest; seen = Cmd_set.empty} in
		on_beat state cmds
	)
  else (
	  (* send new leader new-view message *)
	  let new_view_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = state.s.tcp_lens; msg_type = NewView; node = None; justify = Some state.s.qc_high; partial_signature = None}) in
    (state, [SendLeader new_view_msg])
  ) in
	(state, ResetTimer {id = state.id; view = view} :: actions)

let safe_node state b_new n = ((get_node_height b_new) > state.s.vheight) && ((extends (Some b_new) (Some state.s.b_lock)) || ((get_node_height n) > (get_node_height state.s.b_lock)))

let on_recieve_proposal state (msg : msg) =
	let state = update_tcp_lens state msg.tcp_lens in
	let x = combine_nodes msg.node (Some state.s.b_exec) in
	let b_new = (match x with Some node -> node | None -> raise MissingNodeException) in
	let n = get_node_from_qc (qc_from_node_justify b_new) in
	let state = {state with seen = (Cmd_set.union state.seen n.cmds)} in (* keep track of seen commands *)
	let (state, actions1) = if safe_node state b_new n then (
		let vote_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = GenericAck; node = msg.node; justify = None; partial_signature = None}) in
		({state with s = {state.s with vheight = (get_node_height b_new)}}, [SendNextLeader vote_msg])
	)
	else (state, []) in
	let (state, actions2) = update state b_new in
	(* transition to next state if not next leader as next leader has to collect ACKs first *)
	let (state, actions3) =
		if (is_leader (state.view + 1) state.id state.node_count) then
			(state, [])
		else
			on_next_sync_view state (state.view + 1)
	in
	(state,  actions1 @ actions2 @ actions3)

let on_recieve_new_view state (msg : msg) qc =
	let state = update_qc_high state qc in
	(* update tcp length for source node *)
	let state = update_tcp_lens state msg.tcp_lens in
	(state, [])

let on_recieve_vote state event view =
	match (add_event state.s.v event view state.node_count) with
		| Some q ->
			let qc = threshold_qc state.crypto q in
			(* set new qc as votes as highest *)
			let state = update_qc_high state qc in
			(* transition to next state*)
			on_next_sync_view state (view + 1)
		| None ->
			(state, [])

let create_state_machine ?(crypto = None) id node_count batch_size =
	let s = {v = (Hashtbl.create 10000); vheight = 1; b_lock = b_0; b_exec = b_0; b_leaf = b_0; qc_high = qc_0; tcp_lens = (List.init node_count (fun _ -> 1))} in
	let state = {view = 1; id = id; node_count = node_count; batch_size = batch_size; crypto = crypto; cmds = Cmd_set.empty; seen = Cmd_set.empty; complain = (Hashtbl.create 1000); s = s} in
	if (is_leader 1 id node_count) then
		on_next_sync_view state 1
	else
		(state, [])

let advance (state : t) (event : event) =
	let is_signed = verify_partial_signature state.crypto event in
	let qc = (
		match get_msg_from_event event with
		| Some msg -> if verify_threshold_qc state.crypto state.node_count (Some qc_0) msg.justify then msg.justify else None
		| None -> None
	) in
	let state, actions = if is_signed then (match qc with
		| Some qc when (qc.view + 1) > state.view ->
				(* qc proves we should transition to state qc.view + 1 *)
				let state, actions' = on_next_sync_view state (qc.view + 1) in
				(state, actions')
		| _ ->
			(state, [])
	) else (state, [])
	in
	let state, actions' = if is_signed then
		(match event with
			| GenericAck msg when (is_leader (msg.view + 1) state.id state.node_count) && msg.view >= state.view -> on_recieve_vote state event msg.view
			| Generic msg when msg.view = state.view && (is_leader msg.view msg.id state.node_count) -> on_recieve_proposal state msg
			| NewView msg -> (match qc with
				| Some qc -> on_recieve_new_view state msg qc
				| None -> (state, [])	
			)
			| ClientCmd cmd ->
				({state with cmds = Cmd_set.add cmd state.cmds}, [])
			| Timeout x ->
				let complain_msg = sign state.crypto ({id = state.id; view = x.view; tcp_lens = state.s.tcp_lens; msg_type = Complain; node = None; justify = Some state.s.qc_high; partial_signature = None}) in
				(state, [
					ResetTimer {id = state.id; view = (x.view + 1)}; (* start timeout for next view *)
					SendNextLeader complain_msg (* complain to next leader *)
					])
			| Complain msg when msg.view >= state.view && (is_leader (msg.view + 1) state.id state.node_count) ->
					let state = update_tcp_lens state msg.tcp_lens in
					(match (add_event state.complain event msg.view state.node_count) with
						| Some q ->
							let complainQC = Some (threshold_qc state.crypto q) in
							let broadcast_msg = sign state.crypto ({id = state.id; view = msg.view; tcp_lens = []; msg_type = NextView; node = None; justify = complainQC; partial_signature = None}) in
							(
								state,
								[Broadcast broadcast_msg]
							)
						| None -> (state, [])
					)
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
