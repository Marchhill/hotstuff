open Types
open Util
open Crypto
open Consensus_chained_impl

exception TestException

let msg_to_event (msg : msg) =
	match msg.msg_type with
		| NewView -> NewView msg
		| Generic -> Generic msg
		| GenericAck -> GenericAck msg
		| NextView -> NextView msg 
		| Complain -> Complain msg
		| Prepare -> raise TestException
		| PrepareAck -> raise TestException
		| PreCommit -> raise TestException
		| PreCommitAck -> raise TestException
		| Commit -> raise TestException
		| CommitAck -> raise TestException
		| Decide -> raise TestException

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

let advance_with_log state event =
	(* Fmt.pr "%d: recv " state.id; *)
	(* print_event event; *)
	advance state event

(* deliver multiple events to a node *)
let advance_multiple state events =
	List.fold_left (fun (s, a) e ->
		let (s', a') = advance_with_log s e in
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
	| (SendClient m) :: xs ->
		Fmt.pr "send_client id=\"%s\"@." m.callback_id;
		do_actions nodes xs
	| (Execute m) :: xs ->
		Fmt.pr "exec (%s)@." (node_to_string (Some m.node));
		do_actions nodes xs
	| (ResetTimer _) :: xs ->
		do_actions nodes xs
	| [] -> (nodes, [])

(* run a view *)
let view nodes a =
	let nodes, a = do_actions nodes a in (* delivers Broadcast proposal, returns acks and new-views for next leader *)
	do_actions nodes a (* delivers acks and new view for next leader, returns Broadcast proposal *)

(* deliver a command to be commited *)
let sent = ref 0
let deliver_command id nodes =
	let nodes, _ = advance_leader id nodes [ClientCmd {data = (Fmt.str "hello%d#%d" id (!sent)); callback_id = (Fmt.str "testid%d#%d" id (!sent))}] in
	sent := !sent + 1;
	nodes

let%expect_test "single view" =
	let nodes, init_actions = create_nodes 4 in 
	List.iter print_state nodes;
	List.iter print_action init_actions;
	[%expect {|
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=1
  0: broadcast view=1 src=0 type=generic node=("#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, a = do_actions nodes init_actions in (* delivers Broadcast proposal, returns acks and new-views for next leader *)
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(⊥)
  3: send_next_leader view=1 src=3 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(⊥)
  3: reset_timer view=2
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(⊥)
  2: send_next_leader view=1 src=2 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(⊥)
  2: reset_timer view=2
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(⊥)
  1: send_next_leader view=1 src=1 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(⊥)
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(⊥)
  0: send_next_leader view=1 src=0 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(⊥)
  0: reset_timer view=2 |}];
	let nodes, a = do_actions nodes a in (* delivers acks and new views, returns new broadcast proposal *)
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=2 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=2
  1: broadcast view=2 src=1 type=generic node=("#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}]

  let%expect_test "15 views" =
	let nodes, actions = create_nodes 4 in 
	Fmt.pr "view 1:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 1:
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=1
  0: broadcast view=1 src=0 type=generic node=("#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 2:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 2:
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=2 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=2
  1: broadcast view=2 src=1 type=generic node=("#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 3:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 3:
  state id=3 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=3 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(⊥)) exec=()
  2: reset_timer view=3
  2: broadcast view=3 src=2 type=generic node=("#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 4:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 4:
  state id=3 view=4 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=4 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=4 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=4 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  3: reset_timer view=4
  3: broadcast view=4 src=3 type=generic node=("#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 5:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 5:
  state id=3 view=5 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=5 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=5 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=5 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=5
  0: broadcast view=5 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 6:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 6:
  state id=3 view=6 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=6 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=6 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=6 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=6
  1: broadcast view=6 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 7:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 7:
  state id=3 view=7 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=7 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=7 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=7 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  2: reset_timer view=7
  2: broadcast view=7 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 8:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 8:
  state id=3 view=8 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  3: reset_timer view=8
  3: broadcast view=8 src=3 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 9:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 9:
  state id=3 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=9 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=9
  0: broadcast view=9 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 10:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 10:
  state id=3 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=10 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=10
  1: broadcast view=10 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 11:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 11:
  state id=3 view=11 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=11 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=11 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=11 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  2: reset_timer view=11
  2: broadcast view=11 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 12:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 12:
  state id=3 view=12 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=12 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=12 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=12 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  3: reset_timer view=12
  3: broadcast view=12 src=3 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 13:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 13:
  state id=3 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=13 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=13
  0: broadcast view=13 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 14:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 14:
  state id=3 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=14 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=14
  1: broadcast view=14 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 15:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 15:
  state id=3 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=15 qc_high=(view=14 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(⊥)) exec=()
  2: reset_timer view=15
  2: broadcast view=15 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(⊥) |}]

let%expect_test "single view w/crypto" =
	let nodes, actions = create_nodes 4 ~use_crypto:true in 
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=1
  0: broadcast view=1 src=0 type=generic node=("#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, a = do_actions nodes actions in (* delivers Broadcast proposal, returns new-view and acks for next leader *)
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  3: send_next_leader view=1 src=3 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(#3)
  3: send_next_leader view=1 src=3 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(#3)
  3: reset_timer view=2
  2: send_next_leader view=1 src=2 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(#2)
  2: send_next_leader view=1 src=2 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(#2)
  2: reset_timer view=2
  1: send_next_leader view=1 src=1 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(#1)
  1: send_next_leader view=1 src=1 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(#1)
  0: send_next_leader view=1 src=0 type=new_view node=(⊥) justify=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) sig=(#0)
  0: send_next_leader view=1 src=0 type=generic_ack node=("#"-"b_0"-⊥) justify=() sig=(#0)
  0: reset_timer view=2 |}];
	let nodes, a = do_actions nodes a in (* delivers new-view and acks to next leader, returns broadcast proposal *)
	List.iter print_state nodes;
	List.iter print_action a;
	[%expect {|
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=2 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=2
  1: broadcast view=2 src=1 type=generic node=("#"-"#"-"b_0"-⊥) justify=() sig=(#1) |}]
  
let%expect_test "15 rounds w/crypto" =
	let nodes, actions = create_nodes 4 ~use_crypto:true in 
	Fmt.pr "view 1:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 1:
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=1
  0: broadcast view=1 src=0 type=generic node=("#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 2:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 2:
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=2 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=2
  1: broadcast view=2 src=1 type=generic node=("#"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 3:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 3:
  state id=3 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=3 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=3
  2: broadcast view=3 src=2 type=generic node=("#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 4:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 4:
  state id=3 view=4 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=4 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=4 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=4 qc_high=(view=2 type=generic_ack node=("#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=4
  3: broadcast view=4 src=3 type=generic node=("#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 5:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 5:
  state id=3 view=5 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=5 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=5 qc_high=(view=3 type=generic_ack node=("#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=5 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  0: reset_timer view=5
  0: broadcast view=5 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 6:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 6:
  state id=3 view=6 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=6 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=6 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=6 qc_high=(view=4 type=generic_ack node=("#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  1: reset_timer view=6
  1: broadcast view=6 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 7:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 7:
  state id=3 view=7 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=7 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=7 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=7 qc_high=(view=5 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=7
  2: broadcast view=7 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 8:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 8:
  state id=3 view=8 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=8
  3: broadcast view=8 src=3 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 9:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 9:
  state id=3 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=9 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  0: reset_timer view=9
  0: broadcast view=9 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 10:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 10:
  state id=3 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=10 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  1: reset_timer view=10
  1: broadcast view=10 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 11:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 11:
  state id=3 view=11 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=11 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=11 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=11 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=11
  2: broadcast view=11 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 12:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 12:
  state id=3 view=12 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=12 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=12 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=12 qc_high=(view=10 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=12
  3: broadcast view=12 src=3 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 13:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 13:
  state id=3 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=13 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  0: reset_timer view=13
  0: broadcast view=13 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 14:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 14:
  state id=3 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=14 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  1: reset_timer view=14
  1: broadcast view=14 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 15:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 15:
  state id=3 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=15 qc_high=(view=14 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=15
  2: broadcast view=15 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"#"-"b_0"-⊥) justify=() sig=(#2) |}]

let%expect_test "leader + replicas can commit client commands and respond" =
	let nodes, actions = create_nodes 4 ~use_crypto:true in 
	let nodes = deliver_command 1 nodes in
	let nodes = deliver_command 1 nodes in
	let nodes = deliver_command 2 nodes in
	let nodes = deliver_command 4 nodes in
	Fmt.pr "view 1:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 1:
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  0: reset_timer view=1
  0: broadcast view=1 src=0 type=generic node=("#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 2:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 2:
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=2 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#2,)
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  1: reset_timer view=2
  1: broadcast view=2 src=1 type=generic node=("hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 3:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 3:
  state id=3 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=3 qc_high=(view=2 type=generic_ack node=("hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#2,)
  state id=0 view=3 qc_high=(view=1 type=generic_ack node=("#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=3
  2: broadcast view=3 src=2 type=generic node=("#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 4:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  view 4:
  state id=3 view=4 qc_high=(view=3 type=generic_ack node=("#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#3,)
  state id=2 view=4 qc_high=(view=2 type=generic_ack node=("hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=4 qc_high=(view=2 type=generic_ack node=("hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#2,)
  state id=0 view=4 qc_high=(view=2 type=generic_ack node=("hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=4
  3: broadcast view=4 src=3 type=generic node=("hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 5:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 5:
  state id=3 view=5 qc_high=(view=3 type=generic_ack node=("#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#3,)
  state id=2 view=5 qc_high=(view=3 type=generic_ack node=("#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=5 qc_high=(view=3 type=generic_ack node=("#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#2,)
  state id=0 view=5 qc_high=(view=4 type=generic_ack node=("hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#0,)
  0: reset_timer view=5
  0: broadcast view=5 src=0 type=generic node=("hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 6:@.";
	List.iter print_state nodes;
	[%expect {|
  exec "hello2#2"
  exec "hello2#2"
  exec "hello2#2"
  send_client id="testid2#2"
  exec "hello2#2"
  view 6:
  state id=3 view=6 qc_high=(view=4 type=generic_ack node=("hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#3,)
  state id=2 view=6 qc_high=(view=4 type=generic_ack node=("hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=6 qc_high=(view=5 type=generic_ack node=("hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=6 qc_high=(view=4 type=generic_ack node=("hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#0,) |}];
	let nodes, actions = view nodes actions in
	let nodes = deliver_command 4 nodes in
	Fmt.pr "view 7:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 7:
  state id=3 view=7 qc_high=(view=5 type=generic_ack node=("hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#3,)
  state id=2 view=7 qc_high=(view=6 type=generic_ack node=("#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=7 qc_high=(view=5 type=generic_ack node=("hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=7 qc_high=(view=5 type=generic_ack node=("hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#0,)
  2: reset_timer view=7
  2: broadcast view=7 src=2 type=generic node=("#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 8:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "hello4#3"
  send_client id="testid4#3"
  exec "hello4#3"
  exec "hello4#3"
  exec "hello4#3"
  view 8:
  state id=3 view=8 qc_high=(view=7 type=generic_ack node=("#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#4,)
  state id=2 view=8 qc_high=(view=6 type=generic_ack node=("#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=8 qc_high=(view=6 type=generic_ack node=("#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=8 qc_high=(view=6 type=generic_ack node=("#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#0,)
  3: reset_timer view=8
  3: broadcast view=8 src=3 type=generic node=("hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 9:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "hello1#0"
  exec "hello1#0"
  exec "hello1#0"
  exec "hello1#0"
  send_client id="testid1#0"
  view 9:
  state id=3 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#4,)
  state id=2 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=9 qc_high=(view=8 type=generic_ack node=("hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#1,)
  0: reset_timer view=9
  0: broadcast view=9 src=0 type=generic node=("hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 10:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 10:
  state id=3 view=10 qc_high=(view=8 type=generic_ack node=("hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#4,)
  state id=2 view=10 qc_high=(view=8 type=generic_ack node=("hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=10 qc_high=(view=9 type=generic_ack node=("hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=10 qc_high=(view=8 type=generic_ack node=("hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#1,)
  1: reset_timer view=10
  1: broadcast view=10 src=1 type=generic node=("#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 11:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 11:
  state id=3 view=11 qc_high=(view=9 type=generic_ack node=("hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello4#4,)
  state id=2 view=11 qc_high=(view=10 type=generic_ack node=("#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=11 qc_high=(view=9 type=generic_ack node=("hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=11 qc_high=(view=9 type=generic_ack node=("hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#1,)
  2: reset_timer view=11
  2: broadcast view=11 src=2 type=generic node=("#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 12:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "hello4#4"
  send_client id="testid4#4"
  exec "hello4#4"
  exec "hello4#4"
  exec "hello4#4"
  view 12:
  state id=3 view=12 qc_high=(view=11 type=generic_ack node=("#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=12 qc_high=(view=10 type=generic_ack node=("#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=12 qc_high=(view=10 type=generic_ack node=("#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=12 qc_high=(view=10 type=generic_ack node=("#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello1#1,)
  3: reset_timer view=12
  3: broadcast view=12 src=3 type=generic node=("#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 13:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "hello1#1"
  exec "hello1#1"
  exec "hello1#1"
  exec "hello1#1"
  send_client id="testid1#1"
  view 13:
  state id=3 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=13 qc_high=(view=11 type=generic_ack node=("#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=13 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  0: reset_timer view=13
  0: broadcast view=13 src=0 type=generic node=("#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 14:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 14:
  state id=3 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=14 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=14 qc_high=(view=12 type=generic_ack node=("#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  1: reset_timer view=14
  1: broadcast view=14 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	Fmt.pr "view 15:@.";
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  view 15:
  state id=3 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=15 qc_high=(view=14 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=15 qc_high=(view=13 type=generic_ack node=("#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=15
  2: broadcast view=15 src=2 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"hello1#1"-"hello4#4"-"#"-"#"-"hello1#0"-"hello4#3"-"#"-"hello2#2"-"#"-"b_0"-⊥) justify=() sig=(#2) |}]

let%expect_test "leader + replicas can view change from view 1" =
	let nodes, _ = create_nodes 4 ~use_crypto:true in
	let nodes = deliver_command 1 nodes in
	let timeout = Timeout {view = 1} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	[%expect {|
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
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
  state id=3 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=1 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  1: broadcast view=1 src=1 type=next_view node=(⊥) justify=(view=1 type=complain node=(⊥) sig=(#3,#2,#1)) sig=(#1) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send broadcast for view 2 *)
	[%expect {|
  state id=3 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=2 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=1 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  state id=0 view=2 qc_high=(view=0 type=generic_ack node=("b_0"-⊥) sig=(⊥)) exec=()
  3: reset_timer view=2
  2: reset_timer view=2
  1: reset_timer view=2
  1: broadcast view=2 src=1 type=generic node=("#"-"dummy"-"b_0"-⊥) justify=() sig=(#1)
  0: reset_timer view=2 |}]

let%expect_test "failed view still commits command" =
	let nodes, actions = create_nodes 4 ~use_crypto:true in
	let nodes = deliver_command 2 nodes in
	let nodes, actions = view nodes actions in
	let nodes, actions = view nodes actions in
	let nodes, actions = view nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* 2 chain formed *)
	[%expect {|
  state id=3 view=4 qc_high=(view=3 type=generic_ack node=("#"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#6,)
  state id=0 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=4
  3: broadcast view=4 src=3 type=generic node=("#"-"#"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let timeout = Timeout {view = 4} in
	let nodes, actions = advance_all nodes [timeout] in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send complain messages *)
	[%expect {|
  state id=3 view=4 qc_high=(view=3 type=generic_ack node=("#"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#6,)
  state id=0 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=5
  3: send_next_leader view=4 src=3 type=complain node=(⊥) justify=() sig=(#3)
  2: reset_timer view=5
  2: send_next_leader view=4 src=2 type=complain node=(⊥) justify=() sig=(#2)
  1: reset_timer view=5
  1: send_next_leader view=4 src=1 type=complain node=(⊥) justify=() sig=(#1)
  0: reset_timer view=5
  0: send_next_leader view=4 src=0 type=complain node=(⊥) justify=() sig=(#0) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should broadcast next-view *)
	[%expect {|
  state id=3 view=4 qc_high=(view=3 type=generic_ack node=("#"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=(hello2#6,)
  state id=0 view=4 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  0: broadcast view=4 src=0 type=next_view node=(⊥) justify=(view=4 type=complain node=(⊥) sig=(#3,#2,#1)) sig=(#0) |}];
	let nodes, actions = do_actions nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	(* should send broadcast for view 2 *)
	[%expect {|
  state id=3 view=5 qc_high=(view=3 type=generic_ack node=("#"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=5 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=5 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=5 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=5
  2: reset_timer view=5
  1: send_client success=false callback_id="testid2#6"
  1: reset_timer view=5
  0: reset_timer view=5
  0: broadcast view=5 src=0 type=generic node=("#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  send_client id="testid2#6"
  state id=3 view=6 qc_high=(view=3 type=generic_ack node=("#"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=6 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=6 qc_high=(view=5 type=generic_ack node=("#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=6 qc_high=(view=2 type=generic_ack node=("hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  1: reset_timer view=6
  1: broadcast view=6 src=1 type=generic node=("#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#1) |}];
	let nodes, actions = view nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  state id=3 view=7 qc_high=(view=5 type=generic_ack node=("#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=7 qc_high=(view=6 type=generic_ack node=("#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=7 qc_high=(view=5 type=generic_ack node=("#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=7 qc_high=(view=5 type=generic_ack node=("#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  2: reset_timer view=7
  2: broadcast view=7 src=2 type=generic node=("#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#2) |}];
	let nodes, actions = view nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  state id=3 view=8 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=8 qc_high=(view=6 type=generic_ack node=("#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  3: reset_timer view=8
  3: broadcast view=8 src=3 type=generic node=("#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#3) |}];
	let nodes, actions = view nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  state id=3 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=9 qc_high=(view=7 type=generic_ack node=("#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=9 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  0: reset_timer view=9
  0: broadcast view=9 src=0 type=generic node=("#"-"#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#0) |}];
	let nodes, actions = view nodes actions in
	List.iter print_state nodes;
	List.iter print_action actions;
	[%expect {|
  exec "#"
  exec "#"
  exec "#"
  exec "#"
  state id=3 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=2 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=1 view=10 qc_high=(view=9 type=generic_ack node=("#"-"#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  state id=0 view=10 qc_high=(view=8 type=generic_ack node=("#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) sig=(#3,#2,#1)) exec=()
  1: reset_timer view=10
  1: broadcast view=10 src=1 type=generic node=("#"-"#"-"#"-"#"-"#"-"#"-"dummy"-"dummy"-"hello2#6"-"#"-"b_0"-⊥) justify=() sig=(#1) |}]