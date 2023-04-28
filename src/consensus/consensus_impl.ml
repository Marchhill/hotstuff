open Types
open Util
open Crypto

(* phase only used for debugging purposes *)
type phase = Prepare | PreCommit | Commit | Decide

(* Leader contains own local variables *)
type role = Leader of {m: event list; vpc: event list; vc: event list; vd: event list; has_proposed: bool} | Replica

type state = {phase: phase; role: role; locked_qc: qc option; prepare_qc: qc option; nv: event list}
type t = {view: int; id: int; node_count: int; batch_size: int; cmds: Cmd_set.t;  seen: Cmd_set.t; crypto: crypto option; complain: (int, event list) Hashtbl.t; s: state}

let b_0 = make_node Cmd_set.empty None None
let qc_0 = {node = Some b_0; view = 0; signature = None; msg_type = PrepareAck; ids = []}

let get_qc_0 () = (None : qc option)
let get_b_0 () = (None : node option)

(* utils *)
let phase_to_string : (phase -> string)= function
	| Prepare -> "prepare"
	| PreCommit -> "pre_commit"
	| Commit -> "commit"
	| Decide -> "decide"

let role_to_string = function
	| Leader _ -> "leader"
	| Replica -> "replica"

let print_state state = Fmt.pr "state id=%d phase=%s role=%s view=%d@." state.id (phase_to_string state.s.phase) (role_to_string state.s.role) state.view

let get_role view id node_count nv =
	if is_leader view id node_count then
		Leader {m = nv; vpc = []; vc = []; vd = []; has_proposed = false}
	else
		Replica

let max_qc (qc1 : qc option) (qc2 : qc option) =
	match qc1, qc2 with
		| Some qc1, Some qc2 -> if qc1.view > qc2.view then Some qc1 else Some qc2
		| Some qc, None | None, Some qc -> Some qc
		| None, None -> None

let get_high_qc = List.fold_left (fun acc e2 ->
	let msg = (match get_msg_from_event e2 with Some msg -> msg | None -> raise EventTypeNotFoundException) in
	max_qc acc msg.justify) None

let create_leaf (n : node option) (c : Cmd_set.t) = Some (make_node c n None)

let safe_node (n : node option) (qc : qc option) (lockedQC : qc option) =
	match qc, lockedQC with
		| Some qc, Some lockedQC -> 
			extends n lockedQC.node || (* safety rule *)
			qc.view > lockedQC.view (* liveness rule *)
		| _, None -> true
		| None, Some _ -> false

(* --- *)

let as_leader state event =
	match state.s.role with
		| Leader l ->
			(match event with
				| NewView msg when msg.view = state.view - 1 && not(l.has_proposed) ->
					let m' = event::(l.m) in
					let q = is_quorum m' state.node_count in
					let state = {state with s = {state.s with role = Leader {l with m = m'}}} in
					if q then
						let highQC = get_high_qc m' in
						let i = ref 0 in
    					(* limit batch size *)
						let cmds, rest = Cmd_set.partition (fun _ -> i := !i + 1; !i <= state.batch_size) state.cmds in
						let curProposal = match highQC with
							| Some qc -> create_leaf qc.node cmds (* extend log *)
							| None -> create_leaf None cmds (* should only happen in view 1 *)
						in
						let broadcast_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = Prepare; node = curProposal; justify = highQC; partial_signature = None}) in
						(
							{state with s = {state.s with role = Leader {l with has_proposed = true}}; cmds = rest},
							[Broadcast broadcast_msg]
						)
					else
						(state, [])
				| PrepareAck msg when msg.view = state.view && not(is_quorum l.vpc state.node_count) ->
					let vpc' = event::(l.vpc) in
					let q = get_quorum vpc' state.node_count in
					let state = {state with s = {state.s with role = Leader {l with vpc = vpc'}}}in
					(match q with
						| Some q ->
							let prepareQC = Some (threshold_qc state.crypto q) in
							let broadcast_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = PreCommit; node = None; justify = prepareQC; partial_signature = None}) in
							(
								state,
								[Broadcast broadcast_msg]
							)
						| None -> (state, [])
					)
				| PreCommitAck msg when msg.view = state.view && not(is_quorum l.vc state.node_count) ->
					let vc' = event::(l.vc) in
					let q = get_quorum vc' state.node_count in
					let state = {state with s = {state.s with role = Leader {l with vc = vc'}}} in
					(match q with
						| Some q ->
							let precommitQC = Some (threshold_qc state.crypto q) in
							let broadcast_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = Commit; node = None; justify = precommitQC; partial_signature = None}) in
							(
								state,
								[Broadcast broadcast_msg]
							)
						| None -> (state, [])
					)
				| CommitAck msg when msg.view = state.view && not(is_quorum l.vd state.node_count) ->
					let vd' = event::(l.vd) in
					let q = get_quorum vd' state.node_count in
					let state = {state with s = {state.s with role = Leader {l with vd = vd'}}} in
					(match q with
						| Some q ->
							let commitQC = Some (threshold_qc state.crypto q) in
							let broadcast_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = Decide; node = None; justify = commitQC; partial_signature = None}) in
							let cmds = match msg.node with Some n -> n.cmds | None -> Cmd_set.empty in
							let actions = (Broadcast broadcast_msg) :: (Cmd_set.fold (fun cmd acc ->
								if cmd.callback_id = 0 then
									acc
								else
									(SendClient {id = state.id; callback_id = cmd.callback_id; success = true})::acc
							) cmds []) in
							(state, actions)
						| None -> (state, [])
					)
				| _ -> (state, [])
			)
		| Replica -> (state, [])

(* reset state and assume phase is over, go to view 'view' *)
let finally state view =
	let role = get_role view state.id state.node_count state.s.nv in
	let state' = {state with
		view = view;
		s = {state.s with
			role = role;
			phase = Prepare;
			nv = []
		}
	} in
	let msg = sign state.crypto ({id = state.id; view = (view - 1); tcp_lens = []; msg_type = NewView; node = None; justify = state.s.prepare_qc; partial_signature = None}) in
	let actions = [SendNextLeader msg; ResetTimer {id = state.id; view = view}] in
	(state', actions)

let execute state = function
	| Some n ->
		let state, actions = finally state (state.view + 1) in
		(state, (Execute {id = state.id; node = n}) :: actions)
	| None -> (state, [])

let as_replica state (event : event) =
	let qc = (match get_msg_from_event event with
    	| Some msg -> if (verify_threshold_qc state.crypto state.node_count (Some qc_0) msg.justify) then msg.justify else None
		| None -> None
	) in
	match event with
		| Prepare msg when state.view = msg.view ->
			let is_safe = (match qc with
				| Some qc -> (extends msg.node qc.node) && (safe_node msg.node msg.justify state.s.locked_qc)
				| None -> false
			) in
			if is_safe then
				(
			    let vote_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = PrepareAck; node = msg.node; justify = None; partial_signature = None}) in
					{state with s = {state.s with phase = PreCommit}},
					[SendLeader vote_msg]
				)
			else
				(state, [])
		| PreCommit msg when state.view = msg.view ->
			(match qc with
				| Some qc when (matching_qc qc PrepareAck state.view) ->
					let vote_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = PreCommitAck; node = qc.node; justify = None; partial_signature = None}) in
					(
						{state with s = {state.s with prepare_qc = msg.justify; phase = Commit}},
						[SendLeader vote_msg]
					)
				| _ -> (state, [])
			)
		| Commit msg when state.view = msg.view ->
			(match qc with
				| Some qc when (matching_qc qc PreCommitAck state.view) ->
					let vote_msg = sign state.crypto ({id = state.id; view = state.view; tcp_lens = []; msg_type = CommitAck; node = qc.node; justify = None; partial_signature = None}) in
					(
						{state with s = {state.s with locked_qc = msg.justify; phase = Decide}},
						[SendLeader vote_msg]
					)
				| _ -> (state, [])
			)
		| Decide msg when state.view = msg.view ->
			(match qc with
				| Some qc when (matching_qc qc CommitAck state.view) -> execute state qc.node
				| _ -> (state, [])
			)
		| ClientCmd cmd ->
			({state with cmds = Cmd_set.add cmd state.cmds}, [])
		| NextView msg ->
			(match qc with
				| Some qc when qc.msg_type = Complain && msg.view >= state.view && msg.view = qc.view ->
					finally state (msg.view + 1)
				| _ -> (state, [])
			)
		| Timeout x ->
			let complain_msg = sign state.crypto ({id = state.id; view = x.view; tcp_lens = []; msg_type = Complain; node = None; justify = None; partial_signature = None}) in
			(state, [
				ResetTimer {id = state.id; view = (x.view + 1)}; (* start timeout fot next view *)
				SendNextLeader complain_msg (* complain to next leader *)
				])
		| Complain msg when (is_leader (msg.view + 1) state.id state.node_count) ->
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
		| NewView msg when msg.view = state.view && (is_leader (state.view + 1) state.id state.node_count) ->
			(* store new view messages if we are leader for next round and have not yet transitioned (prevents deadlock ??? race condition accessing nv??? ) *)
			({state with s = {state.s with nv = event::state.s.nv}}, [])
		| _ -> (state, [])

let create_state_machine ?(crypto = None) id node_count _ =
	let r = get_role 1 id node_count [] in
	let s = {phase = Prepare; role = r; locked_qc = Some qc_0; prepare_qc = Some qc_0; nv = []} in
	let state = {view = 1; id = id; node_count = node_count; batch_size = 0; crypto = crypto; cmds = Cmd_set.empty; seen = Cmd_set.empty; complain = (Hashtbl.create 1000); s = s} in
	let new_view_msg = sign state.crypto ({id = state.id; view = 0; tcp_lens = []; msg_type = NewView; node = None; justify = Some qc_0; partial_signature = None}) in
	let new_view_action = SendNextLeader new_view_msg in
	(state, [new_view_action])

let advance (state : t) (event : event) =
	if verify_partial_signature state.crypto event then
		let (state, asLeaderActions) = as_leader state event in
		let (state, asReplicaActions) =	as_replica state event in
		(state, asLeaderActions @ asReplicaActions)
	else
		(state, [])

let create_node_internal (_ : int option) (_ : node_justify option) = (None : node_internal option)