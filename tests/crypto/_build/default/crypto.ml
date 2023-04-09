open Core_bench

exception AggregateFailedException

let seed = Bytes.extend (String.to_bytes (Int.to_string 69)) 31 0
let (_, pk, sk) = Tezos_crypto.Aggregate_signature.generate_key ~seed:seed ()
let data = String.to_bytes "testtesttesttesttesttesttesttest"
let partial_sig = Tezos_crypto.Aggregate_signature.sign sk data
let partial_sigs_4 = List.init 4 (fun _ -> partial_sig)
let partial_sigs_8 = List.init 8 (fun _ -> partial_sig)
let agg_4 = (match Tezos_crypto.Aggregate_signature.aggregate_signature_opt partial_sigs_4 with Some x -> x | None -> raise AggregateFailedException)
let agg_8 = (match Tezos_crypto.Aggregate_signature.aggregate_signature_opt partial_sigs_8 with Some x -> x | None -> raise AggregateFailedException)

let () = Command_unix.run (Bench.make_command [
	Bench.Test.create ~name:"sign" (fun () ->
		Tezos_crypto.Aggregate_signature.sign sk data
	);
	Bench.Test.create ~name:"check" (fun () ->
		Tezos_crypto.Aggregate_signature.check pk partial_sig data
	);
	Bench.Test.create ~name:"agg_4" (fun () ->
		Tezos_crypto.Aggregate_signature.aggregate_signature_opt partial_sigs_4
	);	
	Bench.Test.create ~name:"agg_check_4" (fun () ->
		Tezos_crypto.Aggregate_signature.check pk agg_4 data
	);
	Bench.Test.create ~name:"agg_8" (fun () ->
		Tezos_crypto.Aggregate_signature.aggregate_signature_opt partial_sigs_8
	);	
	Bench.Test.create ~name:"agg_check_8" (fun () ->
		Tezos_crypto.Aggregate_signature.check pk agg_8 data
	);
])