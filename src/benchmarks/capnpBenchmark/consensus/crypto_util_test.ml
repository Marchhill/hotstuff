open Types
open Crypto_util

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