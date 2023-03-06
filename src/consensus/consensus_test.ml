open Types
open Util
open Crypto
open Consensus_impl

exception TestException

let msg_to_event (msg : msg) =
	match msg.msg_type with
		| NewView -> NewView msg
		| Prepare -> Prepare msg
		| PrepareAck -> PrepareAck msg
		| PreCommit -> PreCommit msg
		| PreCommitAck -> PreCommitAck msg
		| Commit -> Commit msg
		| CommitAck -> CommitAck msg
		| Decide -> Decide msg
		| Complain -> Complain msg
		| NextView -> NextView msg
		| Generic -> raise TestException
		| GenericAck -> raise TestException

(* create state machines and new view actions *)
let create_nodes ?(use_crypto = false) n =
	(* create list of state machines*)
	(
		if use_crypto then
			let sks, pks = gen_keys n in
			List.init n (fun i ->
				let crypto = Some {sk = (List.nth sks i); pks = pks} in
				create_state_machine i n ~crypto)
		else
			List.init n (fun i -> create_state_machine i n)
	)
	(* flatten actions *)
	|> List.fold_left (fun (states, actions) (state', actions') -> (state'::states, actions @ actions')) ([], [])

(* deliver multiple events to a node *)
let advance_multiple state events =
	List.fold_left (fun (s, a) e ->
		let (s', a') = advance s e in
		(s', a @ a')
	) (state, []) events

(* deliver multiple events to multiple nodes *)
let advance_all states events =
	let r = List.map (fun s -> advance_multiple s events) states in
	let (states, actions) = List.split r in
	(states, List.flatten actions)

let advance_leader view states events =
	let r = List.map (fun s ->
		if Util.is_leader view s.id s.node_count then
			advance_multiple s events
		else (s, [])) states in
	let (states, actions) = List.split r in
	(states, List.flatten actions)

let rec do_actions nodes = function
	| (Broadcast m) :: xs ->
		let event = msg_to_event m in
		let (nodes, actions) = advance_all nodes [event] in
		let (nodes, actions') = do_actions nodes xs in
		(nodes, actions @ actions')
	| (SendLeader m) :: xs ->
		let event = msg_to_event m in
		let (nodes, actions) = advance_leader m.view nodes [event] in
		let (nodes, actions') = do_actions nodes xs in
		(nodes, actions @ actions')
	| (SendNextLeader m) :: xs ->
		let event = msg_to_event m in
		let (nodes, actions) = advance_leader (m.view + 1) nodes [event] in
		let (nodes, actions') = do_actions nodes xs in
		(nodes, actions @ actions')
	| (SendClient _) :: xs -> do_actions nodes xs
	| (Execute _) :: xs -> do_actions nodes xs
	| (ResetTimer _) :: xs -> do_actions nodes xs
	| [] -> (nodes, [])

(* run a view *)
let view nodes new_view_actions =
	let (nodes, prepare_actions) = do_actions nodes new_view_actions in
	let (nodes, prepare_ack_actions) = do_actions nodes prepare_actions in
	let (nodes, pre_commit_actions) = do_actions nodes prepare_ack_actions in
	let (nodes, pre_commit_ack_actions) = do_actions nodes pre_commit_actions in
	let (nodes, commit_actions) = do_actions nodes pre_commit_ack_actions in
	let (nodes, commit_ack_actions) = do_actions nodes commit_actions in
	let (nodes, decide_actions) = do_actions nodes commit_ack_actions in
	do_actions nodes decide_actions

(* run several views *)
let rec views nodes new_view_actions = function
	| 0 -> (nodes, [])
	| x ->
		let nodes, actions = view nodes new_view_actions in
		views nodes actions (x - 1)

(* send a command to be commited to some node *)
let sent = ref 0
let deliver_command id nodes =
	let nodes, _ = advance_leader id nodes [ClientCmd {data = (Fmt.str "hello%d#%d" id (!sent)); callback_id = (Fmt.str "testid%d#%d" id (!sent))}] in
	sent := !sent + 1;
	nodes

let%expect_test "leader can commit from view 1" =
	let s, _ = create_state_machine 0 4 in
	let new_view_events = List.init 4 (fun id -> NewView {id = id; view = 0; msg_type = NewView; node = None; justify = None; partial_signature = None}) in
	let (s, a) = advance_multiple s new_view_events in
	let s = {s with s = {s.s with phase = PreCommit}} in
	List.iter print_action a;
	[%expect {| 0: broadcast view=1 src=0 type=prepare node=([]-⊥) justify=() sig=(⊥) |}];
	let n = (match List.hd a with
		| Broadcast x -> x.node
		| _ -> raise TestException 
	) in
	let prepare_events = List.init 4 (fun id -> PrepareAck {id = id; view = 1; msg_type = PrepareAck; node = n; justify = None; partial_signature = None}) in
	let (s, a) = advance_multiple s prepare_events in
	let s = {s with s = {s.s with phase = Commit}} in
	List.iter print_action a;
	[%expect {| 0: broadcast view=1 src=0 type=pre_commit node=(⊥) justify=(view=1 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(⊥) |}];
	let n = (match List.hd a with
		| Broadcast x ->
			(match x.justify with
			| Some qc -> qc.node
			| None -> None
			)
		| _ -> raise TestException
	) in
	let pre_commit_events = List.init 4 (fun id -> PreCommitAck {id = id; view = 1; msg_type = PreCommitAck; node = n; justify = None; partial_signature = None}) in
	let (s, a) = advance_multiple s pre_commit_events in
	let s = {s with s = {s.s with phase = Decide}} in
	List.iter print_action a;
	[%expect {| 0: broadcast view=1 src=0 type=commit node=(⊥) justify=(view=1 type=pre_commit_ack node=([]-⊥) sig=(⊥)) sig=(⊥) |}];
	let n = (match List.hd a with
		| Broadcast x ->
			(match x.justify with
			| Some qc -> qc.node
			| None -> None
			)
		| _ -> raise TestException
	) in
	let commit_events = List.init 4 (fun id -> CommitAck{id = id; view = 1; msg_type = CommitAck; node = n; justify = None; partial_signature = None}) in
	let (_, a) = advance_multiple s commit_events in
	List.iter print_action a;
	[%expect {| 0: broadcast view=1 src=0 type=decide node=(⊥) justify=(view=1 type=commit_ack node=([]-⊥) sig=(⊥)) sig=(⊥) |}]

let%expect_test "leader can commit from later view" =
	let s = {(fst (create_state_machine 0 4)) with view = 100} in
	let new_view_events = [
		NewView {id = 0; view = 99; msg_type = NewView; node = None; justify = (Some {msg_type = PrepareAck; view = 70; node = (create_leaf None (Cmd_set.singleton {data = "woo"; callback_id = ""})); signature = None; ids = []}); partial_signature = None};
		NewView {id = 1; view = 99; msg_type = NewView; node = None; justify = (Some {msg_type = PrepareAck; view = 65; node = None; signature = None; ids = []}); partial_signature = None};
		NewView {id = 2; view = 99; msg_type = NewView; node = None; justify = (Some {msg_type = PrepareAck; view = 65; node = None; signature = None; ids = []}); partial_signature = None};
		NewView {id = 3; view = 99; msg_type = NewView; node = None; justify = (Some {msg_type = PrepareAck; view = 67; node = (create_leaf None (Cmd_set.singleton {data = "woo"; callback_id = ""})); signature = None; ids = []}); partial_signature = None}
	] in
	let (s, a) = advance_multiple s new_view_events in
	let s = {s with s = {s.s with phase = PreCommit}} in
	List.iter print_action a;
	[%expect {| 0: broadcast view=100 src=0 type=prepare node=([]-["woo",]-⊥) justify=(view=70 type=prepare_ack node=(["woo",]-⊥) sig=(⊥)) sig=(⊥) |}];
	let n = (match List.hd a with
		| Broadcast x -> x.node
		| _ -> raise TestException 
	) in
	let prepare_events = List.init 4 (fun id -> PrepareAck {id = id; view = 100; msg_type = PrepareAck; node = n; justify = None; partial_signature = None}) in
	let (s, a) = advance_multiple s prepare_events in
	let s = {s with s = {s.s with phase = Commit}} in
	List.iter print_action a;
	[%expect {| 0: broadcast view=100 src=0 type=pre_commit node=(⊥) justify=(view=100 type=prepare_ack node=([]-["woo",]-⊥) sig=(⊥)) sig=(⊥) |}];
	let n = (match List.hd a with
		| Broadcast x ->
			(match x.justify with
			| Some qc -> qc.node
			| None -> None
			)
		| _ -> raise TestException
	) in
	let pre_commit_events = List.init 4 (fun id -> PreCommitAck {id = id; view = 100; msg_type = PreCommitAck; node = n; justify = None; partial_signature = None}) in
	let (s, a) = advance_multiple s pre_commit_events in
	let s = {s with s = {s.s with phase = Decide}} in
	List.iter print_action a;
	[%expect {| 0: broadcast view=100 src=0 type=commit node=(⊥) justify=(view=100 type=pre_commit_ack node=([]-["woo",]-⊥) sig=(⊥)) sig=(⊥) |}];
	let n = (match List.hd a with
		| Broadcast x ->
			(match x.justify with
			| Some qc -> qc.node
			| None -> None
			)
		| _ -> raise TestException
	) in
	let commit_events = List.init 4 (fun id -> CommitAck {id = id; view = 100; msg_type = CommitAck; node = n; justify = None; partial_signature = None}) in
	let (_, a) = advance_multiple s commit_events in
	List.iter print_action a;
	[%expect {| 0: broadcast view=100 src=0 type=decide node=(⊥) justify=(view=100 type=commit_ack node=([]-["woo",]-⊥) sig=(⊥)) sig=(⊥) |}]

let%expect_test "leader + replicas can commit from view 1" =
	let nodes, new_view_actions = create_nodes 4 in
	List.iter print_state nodes;
	List.iter print_action new_view_actions;
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  0: send_next_leader view=0 src=0 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(⊥)
  1: send_next_leader view=0 src=1 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(⊥)
  2: send_next_leader view=0 src=2 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(⊥)
  3: send_next_leader view=0 src=3 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(⊥) |}];
	let (nodes, prepare_actions) = do_actions nodes new_view_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_actions;
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  0: broadcast view=1 src=0 type=prepare node=([]-[]-⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(⊥) |}];
	let (nodes, prepare_ack_actions) = do_actions nodes prepare_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_ack_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=1
  state id=2 phase=pre_commit role=replica view=1
  state id=1 phase=pre_commit role=replica view=1
  state id=0 phase=pre_commit role=leader view=1
  3: send_leader view=1 src=3 type=prepare_ack node=([]-[]-⊥) justify=() sig=(⊥)
  2: send_leader view=1 src=2 type=prepare_ack node=([]-[]-⊥) justify=() sig=(⊥)
  1: send_leader view=1 src=1 type=prepare_ack node=([]-[]-⊥) justify=() sig=(⊥)
  0: send_leader view=1 src=0 type=prepare_ack node=([]-[]-⊥) justify=() sig=(⊥) |}];
	let (nodes, pre_commit_actions) = do_actions nodes prepare_ack_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=1
  state id=2 phase=pre_commit role=replica view=1
  state id=1 phase=pre_commit role=replica view=1
  state id=0 phase=pre_commit role=leader view=1
  0: broadcast view=1 src=0 type=pre_commit node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥) |}];
	let (nodes, pre_commit_ack_actions) = do_actions nodes pre_commit_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_ack_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=1
  state id=2 phase=commit role=replica view=1
  state id=1 phase=commit role=replica view=1
  state id=0 phase=commit role=leader view=1
  3: send_leader view=1 src=3 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(⊥)
  2: send_leader view=1 src=2 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(⊥)
  1: send_leader view=1 src=1 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(⊥)
  0: send_leader view=1 src=0 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(⊥) |}];
	let (nodes, commit_actions) = do_actions nodes pre_commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action commit_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=1
  state id=2 phase=commit role=replica view=1
  state id=1 phase=commit role=replica view=1
  state id=0 phase=commit role=leader view=1
  0: broadcast view=1 src=0 type=commit node=(⊥) justify=(view=1 type=pre_commit_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥) |}];
	let (nodes, commit_ack_actions) = do_actions nodes commit_actions in
	List.iter print_state nodes;
	List.iter print_action commit_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  3: send_leader view=1 src=3 type=commit_ack node=([]-[]-⊥) justify=() sig=(⊥)
  2: send_leader view=1 src=2 type=commit_ack node=([]-[]-⊥) justify=() sig=(⊥)
  1: send_leader view=1 src=1 type=commit_ack node=([]-[]-⊥) justify=() sig=(⊥)
  0: send_leader view=1 src=0 type=commit_ack node=([]-[]-⊥) justify=() sig=(⊥) |}];
	let (nodes, decide_actions) = do_actions nodes commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action decide_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  0: broadcast view=1 src=0 type=decide node=(⊥) justify=(view=1 type=commit_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥) |}];
	let (_, decide_ack_actions) = do_actions nodes decide_actions in
	List.iter print_state nodes;
	List.iter print_action decide_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  3: execute node=[]-[]-⊥
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
  3: reset_timer view=2
  2: execute node=[]-[]-⊥
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
  2: reset_timer view=2
  1: execute node=[]-[]-⊥
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
  1: reset_timer view=2
  0: execute node=[]-[]-⊥
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
  0: reset_timer view=2 |}]

let%expect_test "leader + replicas can commit 5 rounds" =
  let nodes, new_view_actions = create_nodes 4 in
  let (nodes, a) = view nodes new_view_actions in
  List.iter print_state nodes;
  List.iter print_action a;
  [%expect {|
state id=3 phase=prepare role=replica view=2
state id=2 phase=prepare role=replica view=2
state id=1 phase=prepare role=leader view=2
state id=0 phase=prepare role=replica view=2
3: execute node=[]-[]-⊥
3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
3: reset_timer view=2
2: execute node=[]-[]-⊥
2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
2: reset_timer view=2
1: execute node=[]-[]-⊥
1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
1: reset_timer view=2
0: execute node=[]-[]-⊥
0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(⊥)) sig=(⊥)
0: reset_timer view=2 |}];
  let (nodes, a) = view nodes a in
  List.iter print_state nodes;
  List.iter print_action a;
  [%expect {|
state id=3 phase=prepare role=replica view=3
state id=2 phase=prepare role=leader view=3
state id=1 phase=prepare role=replica view=3
state id=0 phase=prepare role=replica view=3
3: execute node=[]-[]-[]-⊥
3: send_next_leader view=2 src=3 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
3: reset_timer view=3
2: execute node=[]-[]-[]-⊥
2: send_next_leader view=2 src=2 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
2: reset_timer view=3
1: execute node=[]-[]-[]-⊥
1: send_next_leader view=2 src=1 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
1: reset_timer view=3
0: execute node=[]-[]-[]-⊥
0: send_next_leader view=2 src=0 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
0: reset_timer view=3 |}];
  let (nodes, a) = view nodes a in
  List.iter print_state nodes;
  List.iter print_action a;
  [%expect {|
state id=3 phase=prepare role=leader view=4
state id=2 phase=prepare role=replica view=4
state id=1 phase=prepare role=replica view=4
state id=0 phase=prepare role=replica view=4
3: execute node=[]-[]-[]-[]-⊥
3: send_next_leader view=3 src=3 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
3: reset_timer view=4
2: execute node=[]-[]-[]-[]-⊥
2: send_next_leader view=3 src=2 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
2: reset_timer view=4
1: execute node=[]-[]-[]-[]-⊥
1: send_next_leader view=3 src=1 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
1: reset_timer view=4
0: execute node=[]-[]-[]-[]-⊥
0: send_next_leader view=3 src=0 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
0: reset_timer view=4 |}];
  let (nodes, a) = view nodes a in
  List.iter print_state nodes;
  List.iter print_action a;
  [%expect {|
state id=3 phase=prepare role=replica view=5
state id=2 phase=prepare role=replica view=5
state id=1 phase=prepare role=replica view=5
state id=0 phase=prepare role=leader view=5
3: execute node=[]-[]-[]-[]-[]-⊥
3: send_next_leader view=4 src=3 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
3: reset_timer view=5
2: execute node=[]-[]-[]-[]-[]-⊥
2: send_next_leader view=4 src=2 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
2: reset_timer view=5
1: execute node=[]-[]-[]-[]-[]-⊥
1: send_next_leader view=4 src=1 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
1: reset_timer view=5
0: execute node=[]-[]-[]-[]-[]-⊥
0: send_next_leader view=4 src=0 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
0: reset_timer view=5 |}];
  let (nodes, a) = view nodes a in
  List.iter print_state nodes;
  List.iter print_action a;
  [%expect {|
state id=3 phase=prepare role=replica view=6
state id=2 phase=prepare role=replica view=6
state id=1 phase=prepare role=leader view=6
state id=0 phase=prepare role=replica view=6
3: execute node=[]-[]-[]-[]-[]-...
3: send_next_leader view=5 src=3 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
3: reset_timer view=6
2: execute node=[]-[]-[]-[]-[]-...
2: send_next_leader view=5 src=2 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
2: reset_timer view=6
1: execute node=[]-[]-[]-[]-[]-...
1: send_next_leader view=5 src=1 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
1: reset_timer view=6
0: execute node=[]-[]-[]-[]-[]-...
0: send_next_leader view=5 src=0 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(⊥)) sig=(⊥)
0: reset_timer view=6 |}]

let%expect_test "leader + replicas can commit from view 1 w/crypto" =
	let nodes, new_view_actions = create_nodes 4 ~use_crypto:true in
	List.iter print_state nodes;
	List.iter print_action new_view_actions;
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  0: send_next_leader view=0 src=0 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#0)
  1: send_next_leader view=0 src=1 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#1)
  2: send_next_leader view=0 src=2 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#2)
  3: send_next_leader view=0 src=3 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#3) |}];
	let (nodes, prepare_actions) = do_actions nodes new_view_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_actions;
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  0: broadcast view=1 src=0 type=prepare node=([]-[]-⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#0) |}];
	let (nodes, prepare_ack_actions) = do_actions nodes prepare_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_ack_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=1
  state id=2 phase=pre_commit role=replica view=1
  state id=1 phase=pre_commit role=replica view=1
  state id=0 phase=pre_commit role=leader view=1
  3: send_leader view=1 src=3 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=1 src=2 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=1 src=1 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=1 src=0 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, pre_commit_actions) = do_actions nodes prepare_ack_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=1
  state id=2 phase=pre_commit role=replica view=1
  state id=1 phase=pre_commit role=replica view=1
  state id=0 phase=pre_commit role=leader view=1
  0: broadcast view=1 src=0 type=pre_commit node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let (nodes, pre_commit_ack_actions) = do_actions nodes pre_commit_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_ack_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=1
  state id=2 phase=commit role=replica view=1
  state id=1 phase=commit role=replica view=1
  state id=0 phase=commit role=leader view=1
  3: send_leader view=1 src=3 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=1 src=2 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=1 src=1 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=1 src=0 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, commit_actions) = do_actions nodes pre_commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action commit_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=1
  state id=2 phase=commit role=replica view=1
  state id=1 phase=commit role=replica view=1
  state id=0 phase=commit role=leader view=1
  0: broadcast view=1 src=0 type=commit node=(⊥) justify=(view=1 type=pre_commit_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let (nodes, commit_ack_actions) = do_actions nodes commit_actions in
	List.iter print_state nodes;
	List.iter print_action commit_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  3: send_leader view=1 src=3 type=commit_ack node=([]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=1 src=2 type=commit_ack node=([]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=1 src=1 type=commit_ack node=([]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=1 src=0 type=commit_ack node=([]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, decide_actions) = do_actions nodes commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action decide_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  0: broadcast view=1 src=0 type=decide node=(⊥) justify=(view=1 type=commit_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let (_, decide_ack_actions) = do_actions nodes decide_actions in
	List.iter print_state nodes;
	List.iter print_action decide_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  3: execute node=[]-[]-⊥
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=2
  2: execute node=[]-[]-⊥
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=2
  1: execute node=[]-[]-⊥
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=2
  0: execute node=[]-[]-⊥
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=2 |}]

let%expect_test "leader + replicas can commit 5 rounds w/crypto" =
	let nodes, new_view_actions = create_nodes 4 ~use_crypto:true in
	let (nodes, a) = view nodes new_view_actions in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=2
  state id=2 phase=prepare role=replica view=2
  state id=1 phase=prepare role=leader view=2
  state id=0 phase=prepare role=replica view=2
  3: execute node=[]-[]-⊥
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=2
  2: execute node=[]-[]-⊥
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=2
  1: execute node=[]-[]-⊥
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=2
  0: execute node=[]-[]-⊥
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=2 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=3
  state id=2 phase=prepare role=leader view=3
  state id=1 phase=prepare role=replica view=3
  state id=0 phase=prepare role=replica view=3
  3: execute node=[]-[]-[]-⊥
  3: send_next_leader view=2 src=3 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=3
  2: execute node=[]-[]-[]-⊥
  2: send_next_leader view=2 src=2 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=3
  1: execute node=[]-[]-[]-⊥
  1: send_next_leader view=2 src=1 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=3
  0: execute node=[]-[]-[]-⊥
  0: send_next_leader view=2 src=0 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=3 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=leader view=4
  state id=2 phase=prepare role=replica view=4
  state id=1 phase=prepare role=replica view=4
  state id=0 phase=prepare role=replica view=4
  3: execute node=[]-[]-[]-[]-⊥
  3: send_next_leader view=3 src=3 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=4
  2: execute node=[]-[]-[]-[]-⊥
  2: send_next_leader view=3 src=2 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=4
  1: execute node=[]-[]-[]-[]-⊥
  1: send_next_leader view=3 src=1 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=4
  0: execute node=[]-[]-[]-[]-⊥
  0: send_next_leader view=3 src=0 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=([]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=4 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=5
  state id=2 phase=prepare role=replica view=5
  state id=1 phase=prepare role=replica view=5
  state id=0 phase=prepare role=leader view=5
  3: execute node=[]-[]-[]-[]-[]-⊥
  3: send_next_leader view=4 src=3 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=5
  2: execute node=[]-[]-[]-[]-[]-⊥
  2: send_next_leader view=4 src=2 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=5
  1: execute node=[]-[]-[]-[]-[]-⊥
  1: send_next_leader view=4 src=1 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=5
  0: execute node=[]-[]-[]-[]-[]-⊥
  0: send_next_leader view=4 src=0 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=([]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=5 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=6
  state id=2 phase=prepare role=replica view=6
  state id=1 phase=prepare role=leader view=6
  state id=0 phase=prepare role=replica view=6
  3: execute node=[]-[]-[]-[]-[]-...
  3: send_next_leader view=5 src=3 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=6
  2: execute node=[]-[]-[]-[]-[]-...
  2: send_next_leader view=5 src=2 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=6
  1: execute node=[]-[]-[]-[]-[]-...
  1: send_next_leader view=5 src=1 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=6
  0: execute node=[]-[]-[]-[]-[]-...
  0: send_next_leader view=5 src=0 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=6 |}]

let%expect_test "leader + replicas can commit and send response to client" =
	let nodes, new_view_actions = create_nodes 4 ~use_crypto:true in
	let nodes = deliver_command 1 nodes in
	let (nodes, prepare_actions) = do_actions nodes new_view_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_actions;
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  0: broadcast view=1 src=0 type=prepare node=(["hello1#0",]-[]-⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#0) |}];
	let (nodes, prepare_ack_actions) = do_actions nodes prepare_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_ack_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=1
  state id=2 phase=pre_commit role=replica view=1
  state id=1 phase=pre_commit role=replica view=1
  state id=0 phase=pre_commit role=leader view=1
  3: send_leader view=1 src=3 type=prepare_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=1 src=2 type=prepare_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=1 src=1 type=prepare_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=1 src=0 type=prepare_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, pre_commit_actions) = do_actions nodes prepare_ack_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=1
  state id=2 phase=pre_commit role=replica view=1
  state id=1 phase=pre_commit role=replica view=1
  state id=0 phase=pre_commit role=leader view=1
  0: broadcast view=1 src=0 type=pre_commit node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let (nodes, pre_commit_ack_actions) = do_actions nodes pre_commit_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_ack_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=1
  state id=2 phase=commit role=replica view=1
  state id=1 phase=commit role=replica view=1
  state id=0 phase=commit role=leader view=1
  3: send_leader view=1 src=3 type=pre_commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=1 src=2 type=pre_commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=1 src=1 type=pre_commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=1 src=0 type=pre_commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, commit_actions) = do_actions nodes pre_commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action commit_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=1
  state id=2 phase=commit role=replica view=1
  state id=1 phase=commit role=replica view=1
  state id=0 phase=commit role=leader view=1
  0: broadcast view=1 src=0 type=commit node=(⊥) justify=(view=1 type=pre_commit_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let (nodes, commit_ack_actions) = do_actions nodes commit_actions in
	List.iter print_state nodes;
	List.iter print_action commit_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  3: send_leader view=1 src=3 type=commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=1 src=2 type=commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=1 src=1 type=commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=1 src=0 type=commit_ack node=(["hello1#0",]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, decide_actions) = do_actions nodes commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action decide_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  0: broadcast view=1 src=0 type=decide node=(⊥) justify=(view=1 type=commit_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: send_client success=true callback_id="testid1#0" |}];
	let (_, decide_ack_actions) = do_actions nodes decide_actions in
	List.iter print_state nodes;
	List.iter print_action decide_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=1
  state id=2 phase=decide role=replica view=1
  state id=1 phase=decide role=replica view=1
  state id=0 phase=decide role=leader view=1
  3: execute node=["hello1#0",]-[]-⊥
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=2
  2: execute node=["hello1#0",]-[]-⊥
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=2
  1: execute node=["hello1#0",]-[]-⊥
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=2
  0: execute node=["hello1#0",]-[]-⊥
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#0",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=2 |}]

let%expect_test "leader + replicas commit client command if present or noop otherwise" =
	let nodes, new_view_actions = create_nodes 4 ~use_crypto:true in
	let nodes = deliver_command 1 nodes in
	let nodes = deliver_command 2 nodes in
	let nodes = deliver_command 3 nodes in
	let nodes = deliver_command 4 nodes in
	let (nodes, a) = view nodes new_view_actions in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=2
  state id=2 phase=prepare role=replica view=2
  state id=1 phase=prepare role=leader view=2
  state id=0 phase=prepare role=replica view=2
  3: execute node=["hello1#1",]-[]-⊥
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=2
  2: execute node=["hello1#1",]-[]-⊥
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=2
  1: execute node=["hello1#1",]-[]-⊥
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=2
  0: execute node=["hello1#1",]-[]-⊥
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=1 type=prepare_ack node=(["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=2 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=3
  state id=2 phase=prepare role=leader view=3
  state id=1 phase=prepare role=replica view=3
  state id=0 phase=prepare role=replica view=3
  3: execute node=["hello2#2",]-["hello1#1",]-[]-⊥
  3: send_next_leader view=2 src=3 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=(["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=3
  2: execute node=["hello2#2",]-["hello1#1",]-[]-⊥
  2: send_next_leader view=2 src=2 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=(["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=3
  1: execute node=["hello2#2",]-["hello1#1",]-[]-⊥
  1: send_next_leader view=2 src=1 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=(["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=3
  0: execute node=["hello2#2",]-["hello1#1",]-[]-⊥
  0: send_next_leader view=2 src=0 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=(["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=3 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=leader view=4
  state id=2 phase=prepare role=replica view=4
  state id=1 phase=prepare role=replica view=4
  state id=0 phase=prepare role=replica view=4
  3: execute node=["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  3: send_next_leader view=3 src=3 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=(["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=4
  2: execute node=["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  2: send_next_leader view=3 src=2 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=(["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=4
  1: execute node=["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  1: send_next_leader view=3 src=1 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=(["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=4
  0: execute node=["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  0: send_next_leader view=3 src=0 type=new_view node=(⊥) justify=(view=3 type=prepare_ack node=(["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=4 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=5
  state id=2 phase=prepare role=replica view=5
  state id=1 phase=prepare role=replica view=5
  state id=0 phase=prepare role=leader view=5
  3: execute node=["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  3: send_next_leader view=4 src=3 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=(["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=5
  2: execute node=["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  2: send_next_leader view=4 src=2 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=(["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=5
  1: execute node=["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  1: send_next_leader view=4 src=1 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=(["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=5
  0: execute node=["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥
  0: send_next_leader view=4 src=0 type=new_view node=(⊥) justify=(view=4 type=prepare_ack node=(["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=5 |}];
	let (nodes, a) = view nodes a in
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 phase=prepare role=replica view=6
  state id=2 phase=prepare role=replica view=6
  state id=1 phase=prepare role=leader view=6
  state id=0 phase=prepare role=replica view=6
  3: execute node=[]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-...
  3: send_next_leader view=5 src=3 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=6
  2: execute node=[]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-...
  2: send_next_leader view=5 src=2 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=6
  1: execute node=[]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-...
  1: send_next_leader view=5 src=1 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=6
  0: execute node=[]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-...
  0: send_next_leader view=5 src=0 type=new_view node=(⊥) justify=(view=5 type=prepare_ack node=([]-["hello4#4",]-["hello3#3",]-["hello2#2",]-["hello1#1",]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=6 |}]

let%expect_test "leader + replicas can view change from view 1" =
	let nodes, _ = create_nodes 4 ~use_crypto:true in
	let timeout = Timeout {view = 1} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  3: reset_timer view=2
  3: send_next_leader view=1 src=3 type=complain node=(⊥) justify=() sig=(#3)
  2: reset_timer view=2
  2: send_next_leader view=1 src=2 type=complain node=(⊥) justify=() sig=(#2)
  1: reset_timer view=2
  1: send_next_leader view=1 src=1 type=complain node=(⊥) justify=() sig=(#1)
  0: reset_timer view=2
  0: send_next_leader view=1 src=0 type=complain node=(⊥) justify=() sig=(#0) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should broadcast next-view *)
	[%expect {|
  state id=3 phase=prepare role=replica view=1
  state id=2 phase=prepare role=replica view=1
  state id=1 phase=prepare role=replica view=1
  state id=0 phase=prepare role=leader view=1
  1: broadcast view=1 src=1 type=next_view node=(⊥) justify=(view=1 type=complain node=(⊥) sig=(#3,#2,#1)) sig=(#1) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send new view messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=2
  state id=2 phase=prepare role=replica view=2
  state id=1 phase=prepare role=leader view=2
  state id=0 phase=prepare role=replica view=2
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#3)
  3: reset_timer view=2
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#2)
  2: reset_timer view=2
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#1)
  1: reset_timer view=2
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#0)
  0: reset_timer view=2 |}];
	let (nodes, prepare_actions) = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action prepare_actions;
	[%expect {|
  state id=3 phase=prepare role=replica view=2
  state id=2 phase=prepare role=replica view=2
  state id=1 phase=prepare role=leader view=2
  state id=0 phase=prepare role=replica view=2
  1: broadcast view=2 src=1 type=prepare node=([]-[]-⊥) justify=(view=0 type=prepare_ack node=([]-⊥) sig=(⊥)) sig=(#1) |}];
	let (nodes, prepare_ack_actions) = do_actions nodes prepare_actions in
	List.iter print_state nodes;
	List.iter print_action prepare_ack_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=2
  state id=2 phase=pre_commit role=replica view=2
  state id=1 phase=pre_commit role=leader view=2
  state id=0 phase=pre_commit role=replica view=2
  3: send_leader view=2 src=3 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=2 src=2 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=2 src=1 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=2 src=0 type=prepare_ack node=([]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, pre_commit_actions) = do_actions nodes prepare_ack_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_actions;
	[%expect {|
  state id=3 phase=pre_commit role=replica view=2
  state id=2 phase=pre_commit role=replica view=2
  state id=1 phase=pre_commit role=leader view=2
  state id=0 phase=pre_commit role=replica view=2
  1: broadcast view=2 src=1 type=pre_commit node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#1) |}];
	let (nodes, pre_commit_ack_actions) = do_actions nodes pre_commit_actions in
	List.iter print_state nodes;
	List.iter print_action pre_commit_ack_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=2
  state id=2 phase=commit role=replica view=2
  state id=1 phase=commit role=leader view=2
  state id=0 phase=commit role=replica view=2
  3: send_leader view=2 src=3 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=2 src=2 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=2 src=1 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=2 src=0 type=pre_commit_ack node=([]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, commit_actions) = do_actions nodes pre_commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action commit_actions;
	[%expect {|
  state id=3 phase=commit role=replica view=2
  state id=2 phase=commit role=replica view=2
  state id=1 phase=commit role=leader view=2
  state id=0 phase=commit role=replica view=2
  1: broadcast view=2 src=1 type=commit node=(⊥) justify=(view=2 type=pre_commit_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#1) |}];
	let (nodes, commit_ack_actions) = do_actions nodes commit_actions in
	List.iter print_state nodes;
	List.iter print_action commit_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=2
  state id=2 phase=decide role=replica view=2
  state id=1 phase=decide role=leader view=2
  state id=0 phase=decide role=replica view=2
  3: send_leader view=2 src=3 type=commit_ack node=([]-[]-⊥) justify=() sig=(#3)
  2: send_leader view=2 src=2 type=commit_ack node=([]-[]-⊥) justify=() sig=(#2)
  1: send_leader view=2 src=1 type=commit_ack node=([]-[]-⊥) justify=() sig=(#1)
  0: send_leader view=2 src=0 type=commit_ack node=([]-[]-⊥) justify=() sig=(#0) |}];
	let (nodes, decide_actions) = do_actions nodes commit_ack_actions in
	List.iter print_state nodes;
	List.iter print_action decide_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=2
  state id=2 phase=decide role=replica view=2
  state id=1 phase=decide role=leader view=2
  state id=0 phase=decide role=replica view=2
  1: broadcast view=2 src=1 type=decide node=(⊥) justify=(view=2 type=commit_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#1) |}];
	let (_, decide_ack_actions) = do_actions nodes decide_actions in
	List.iter print_state nodes;
	List.iter print_action decide_ack_actions;
	[%expect {|
  state id=3 phase=decide role=replica view=2
  state id=2 phase=decide role=replica view=2
  state id=1 phase=decide role=leader view=2
  state id=0 phase=decide role=replica view=2
  3: execute node=[]-[]-⊥
  3: send_next_leader view=2 src=3 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=3
  2: execute node=[]-[]-⊥
  2: send_next_leader view=2 src=2 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=3
  1: execute node=[]-[]-⊥
  1: send_next_leader view=2 src=1 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=3
  0: execute node=[]-[]-⊥
  0: send_next_leader view=2 src=0 type=new_view node=(⊥) justify=(view=2 type=prepare_ack node=([]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=3 |}]

let%expect_test "leader + replicas can view change from view 10 to 11" =
	let nodes, new_view_actions = create_nodes 4 ~use_crypto:true in
	let nodes, actions = views nodes new_view_actions 9 in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should be in view 50 *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10 |}];
	let timeout = Timeout {view = 10} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10
  3: reset_timer view=11
  3: send_next_leader view=10 src=3 type=complain node=(⊥) justify=() sig=(#3)
  2: reset_timer view=11
  2: send_next_leader view=10 src=2 type=complain node=(⊥) justify=() sig=(#2)
  1: reset_timer view=11
  1: send_next_leader view=10 src=1 type=complain node=(⊥) justify=() sig=(#1)
  0: reset_timer view=11
  0: send_next_leader view=10 src=0 type=complain node=(⊥) justify=() sig=(#0) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should broadcast next-view *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10
  2: broadcast view=10 src=2 type=next_view node=(⊥) justify=(view=10 type=complain node=(⊥) sig=(#3,#2,#1)) sig=(#2) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send new view messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=11
  state id=2 phase=prepare role=leader view=11
  state id=1 phase=prepare role=replica view=11
  state id=0 phase=prepare role=replica view=11
  3: send_next_leader view=10 src=3 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=11
  2: send_next_leader view=10 src=2 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=11
  1: send_next_leader view=10 src=1 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=11
  0: send_next_leader view=10 src=0 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=11 |}];
	let nodes, actions = view nodes actions in 
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should carry out the next round *)
	[%expect {|
  state id=3 phase=prepare role=leader view=12
  state id=2 phase=prepare role=replica view=12
  state id=1 phase=prepare role=replica view=12
  state id=0 phase=prepare role=replica view=12
  3: execute node=[]-[]-[]-[]-[]-...
  3: send_next_leader view=11 src=3 type=new_view node=(⊥) justify=(view=11 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=12
  2: execute node=[]-[]-[]-[]-[]-...
  2: send_next_leader view=11 src=2 type=new_view node=(⊥) justify=(view=11 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=12
  1: execute node=[]-[]-[]-[]-[]-...
  1: send_next_leader view=11 src=1 type=new_view node=(⊥) justify=(view=11 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=12
  0: execute node=[]-[]-[]-[]-[]-...
  0: send_next_leader view=11 src=0 type=new_view node=(⊥) justify=(view=11 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=12 |}]

let%expect_test "leader + replicas can view change from view 10 to 13" =
	let nodes, new_view_actions = create_nodes 4 ~use_crypto:true in
	let nodes, actions = views nodes new_view_actions 9 in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should be in view 50 *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10 |}];
	let timeout = Timeout {view = 10} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10
  3: reset_timer view=11
  3: send_next_leader view=10 src=3 type=complain node=(⊥) justify=() sig=(#3)
  2: reset_timer view=11
  2: send_next_leader view=10 src=2 type=complain node=(⊥) justify=() sig=(#2)
  1: reset_timer view=11
  1: send_next_leader view=10 src=1 type=complain node=(⊥) justify=() sig=(#1)
  0: reset_timer view=11
  0: send_next_leader view=10 src=0 type=complain node=(⊥) justify=() sig=(#0) |}];
	let timeout = Timeout {view = 11} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	let timeout = Timeout {view = 12} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10
  3: reset_timer view=12
  3: send_next_leader view=11 src=3 type=complain node=(⊥) justify=() sig=(#3)
  2: reset_timer view=12
  2: send_next_leader view=11 src=2 type=complain node=(⊥) justify=() sig=(#2)
  1: reset_timer view=12
  1: send_next_leader view=11 src=1 type=complain node=(⊥) justify=() sig=(#1)
  0: reset_timer view=12
  0: send_next_leader view=11 src=0 type=complain node=(⊥) justify=() sig=(#0)
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10
  3: reset_timer view=13
  3: send_next_leader view=12 src=3 type=complain node=(⊥) justify=() sig=(#3)
  2: reset_timer view=13
  2: send_next_leader view=12 src=2 type=complain node=(⊥) justify=() sig=(#2)
  1: reset_timer view=13
  1: send_next_leader view=12 src=1 type=complain node=(⊥) justify=() sig=(#1)
  0: reset_timer view=13
  0: send_next_leader view=12 src=0 type=complain node=(⊥) justify=() sig=(#0) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should broadcast next-view *)
	[%expect {|
  state id=3 phase=prepare role=replica view=10
  state id=2 phase=prepare role=replica view=10
  state id=1 phase=prepare role=leader view=10
  state id=0 phase=prepare role=replica view=10
  0: broadcast view=12 src=0 type=next_view node=(⊥) justify=(view=12 type=complain node=(⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send new view messages *)
	[%expect {|
  state id=3 phase=prepare role=replica view=13
  state id=2 phase=prepare role=replica view=13
  state id=1 phase=prepare role=replica view=13
  state id=0 phase=prepare role=leader view=13
  3: send_next_leader view=12 src=3 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=13
  2: send_next_leader view=12 src=2 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=13
  1: send_next_leader view=12 src=1 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=13
  0: send_next_leader view=12 src=0 type=new_view node=(⊥) justify=(view=9 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=13 |}];
	let nodes, actions = view nodes actions in 
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should carry out the next round *)
	[%expect {|
  state id=3 phase=prepare role=replica view=14
  state id=2 phase=prepare role=replica view=14
  state id=1 phase=prepare role=leader view=14
  state id=0 phase=prepare role=replica view=14
  3: execute node=[]-[]-[]-[]-[]-...
  3: send_next_leader view=13 src=3 type=new_view node=(⊥) justify=(view=13 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#3)
  3: reset_timer view=14
  2: execute node=[]-[]-[]-[]-[]-...
  2: send_next_leader view=13 src=2 type=new_view node=(⊥) justify=(view=13 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#2)
  2: reset_timer view=14
  1: execute node=[]-[]-[]-[]-[]-...
  1: send_next_leader view=13 src=1 type=new_view node=(⊥) justify=(view=13 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#1)
  1: reset_timer view=14
  0: execute node=[]-[]-[]-[]-[]-...
  0: send_next_leader view=13 src=0 type=new_view node=(⊥) justify=(view=13 type=prepare_ack node=([]-[]-[]-[]-[]-[]-[]-[]-[]-[]-[]-⊥) sig=(#3,#2,#1)) sig=(#0)
  0: reset_timer view=14 |}]