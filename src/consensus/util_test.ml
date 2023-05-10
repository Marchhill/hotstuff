open Types
open Util

let%expect_test "node justify to string of None." =
	Fmt.pr "%s@." (node_justify_to_string None);
	[%expect {||}]

let%expect_test "get event type." =
	let m = {id = 0; view = 99; tcp_lens = []; msg_type = NewView; node = None; justify = None; partial_signature = None} in
	Fmt.pr "%s@." (get_event_type (ClientCmd {data = ""; callback_id = 0}));
	Fmt.pr "%s@." (get_event_type (Timeout {view = 10}));
	Fmt.pr "%s@." (get_event_type (Timeout {view = 10}));
	Fmt.pr "%s@." (get_event_type (Prepare m));
	Fmt.pr "%s@." (get_event_type (PreCommit m));
	Fmt.pr "%s@." (get_event_type (Commit m));
	Fmt.pr "%s@." (get_event_type (Decide m));
	Fmt.pr "%s@." (get_event_type (NextView m));
	Fmt.pr "%s@." (get_event_type (Generic m));
	[%expect {|
  client_cmd
  timeout
  timeout
  prepare
  pre_commit
  decide
  next_view
  generic |}]

let%expect_test "print action" =
	print_action (SendClient {id = 0; callback_id = 0; success = true});
	[%expect {| 0: send_client success=true callback_id="0" |}]

let%expect_test "print event" =
	let m = {id = 0; view = 99; tcp_lens = []; msg_type = NewView; node = None; justify = None; partial_signature = None} in
	print_event (NewView m);
	print_event (Prepare m);
	print_event (PrepareAck m);
	print_event (PreCommit m);
	print_event (PreCommitAck m);
	print_event (Commit m);
	print_event (CommitAck m);
	print_event (Decide m);
	print_event (NextView m);
	print_event (Complain m);
	print_event (Generic m);
	print_event (GenericAck m);
	print_event (ClientCmd {data = ""; callback_id = 0});
	print_event (Timeout {view = 10});
	[%expect {|
  new_view view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  prepare view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  prepare_ack view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  pre_commit view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  pre_commit_ack view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  commit view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  commit_ack view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  decide view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  next_view view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  complain view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  generic view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  generic_ack view=99 src=0 type=new_view node=(⊥) justify=() sig=(⊥)
  client_cmd data="" callback_id="0"
  timeout 10 |}]

let%expect_test "node nth fails if node not long enough" =
	let _ = try node_nth 10 (Consensus_chained_impl.b_0) with MissingNodeException -> Fmt.pr "Missing node!@."; (Consensus_chained_impl.b_0) in
	[%expect {| Missing node! |}]

let%expect_test "empty nodes are equal" =
	Fmt.pr "%b@." (equal_nodes None None);
	[%expect {| true |}]

let%expect_test "empty qc are equal" =
	Fmt.pr "%b@." (equal_qc None None);
	[%expect {| true |}]

let%expect_test "anything extends an empty node" =
	Fmt.pr "%b@." (extends None None);
	Fmt.pr "%b@." (extends (Some Consensus_chained_impl.b_0) None);
	[%expect {|
  true
  true |}]

let%expect_test "cannot get height or justify without node internal" =
	let _ = try get_node_height (Consensus_impl.b_0) with NodeInternalException -> Fmt.pr "Missing node internal!@."; 0 in
	[%expect {| Missing node internal! |}];
	let j = {node_offset = 0; view = 0; signature = None; msg_type = Complain; ids = []} in
	let _ = try get_node_justify (Consensus_impl.b_0) with NodeInternalException -> Fmt.pr "Missing node internal!@."; j in
	[%expect {| Missing node internal! |}]

let%expect_test "cannot get None node from qc" =
	let qc = {node = None; view = 0; signature = None; msg_type = PrepareAck; ids = []} in
	let _ = try get_node_from_qc qc with MissingNodeException -> Fmt.pr "Missing node!@."; Consensus_impl.b_0 in
	[%expect {| Missing node! |}]

let%expect_test "trimming node beyond depth does nothing" =
	Fmt.pr "%s@." (node_to_string (Some (trim_node Consensus_impl.b_0 10)));
	[%expect {| []-⊥ |}]

let%expect_test "cannot get quorum of events without messages" =
	let _ = try get_quorum [Timeout {view = 10}] 4 with ThresholdQCException -> Fmt.pr "Could not get a quorum!"; None in
	[%expect {| Could not get a quorum! |}]

let%expect_test "threshold qc rejects bad inputs." =
  let sks, pks = gen_keys 1 in
	let crypto = Some {sk = (List.hd sks); pks = pks} in
	let events = [NewView {id = 0; view = 1; tcp_lens = []; msg_type = NewView; node = None; justify = None; partial_signature = None}] in
	let _ = try threshold_qc crypto events with ThresholdQCException -> Fmt.pr "could not make qc!@."; Consensus_chained_impl.qc_0 in
	[%expect {| could not make qc! |}];
	let _ = try threshold_qc crypto [] with ThresholdQCException -> Fmt.pr "could not make qc!@."; Consensus_chained_impl.qc_0 in
	[%expect {| could not make qc! |}];
	let events = [Timeout {view = 10}] in
	let _ = try threshold_qc crypto events with ThresholdQCException -> Fmt.pr "could not make qc!@."; Consensus_chained_impl.qc_0 in
	[%expect {| could not make qc! |}]

let%expect_test "verify threshold qc rejects incomplete quorums." =
	let sks, pks = gen_keys 1 in
	let crypto = Some {sk = (List.hd sks); pks = pks} in
	let qc = {msg_type = NewView; view = 1; node = None; signature = None; ids = []} in
	let v = verify_threshold_qc crypto 4 (Some Consensus_chained_impl.qc_0) (Some qc) in
	Fmt.pr "%b@." v;
	[%expect {| false |}];
	let qc = {msg_type = NewView; view = 1; node = None; signature = None; ids = [1; 2; 3]} in
	let v = verify_threshold_qc crypto 4 (Some Consensus_chained_impl.qc_0) (Some qc) in
	Fmt.pr "%b@." v;
	[%expect {| false |}]