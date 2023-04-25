open Types
open Util

let get_msg_from_event_exn e = match get_msg_from_event e with Some msg -> msg | None -> raise ThresholdQCException

let threshold_qc crypto events =
	let (signature, ids) = (match crypto with
		| Some _ ->
			let (partial_signatures, ids) = List.map (fun e ->
				let msg = get_msg_from_event_exn e in
				match msg.partial_signature with
					| Some p -> (p, msg.id)
					| None -> raise ThresholdQCException) events
				|> List.split in
			(Tezos_crypto.Aggregate_signature.aggregate_signature_opt partial_signatures, ids)
		| None ->
			(None, [])
	)
	in
	match events with
		| e :: _ ->
			let msg = get_msg_from_event_exn e in
			{msg_type = msg.msg_type; view = msg.view; node = msg.node; signature = signature; ids = ids}
		| [] -> raise ThresholdQCException (* should never happen! *)

let sign crypto msg =
	match crypto with
		| Some crypto ->
			let node_digest = match msg.node with Some n -> n.digest | None -> Bytes.empty in 
			let s = String.to_bytes ((msg_type_to_string msg.msg_type) ^ ":" ^ (Int.to_string msg.view) ^ ":") in
			let s = Bytes.concat s [node_digest] in
			let partial_signature = Some (Tezos_crypto.Aggregate_signature.sign crypto.sk s) in
			{msg with partial_signature = partial_signature}
		| None -> msg

let verify_partial_signature crypto event =
	match get_msg_from_event event with
		| Some msg ->
			(match crypto with
				| Some crypto ->
					(match msg.partial_signature with
						| Some p ->
			        		let node_digest = match msg.node with Some n -> n.digest | None -> Bytes.empty in 
							(* let s = String.to_bytes ((get_event_type event) ^ ":" ^ (Int.to_string msg.view) ^ ":" ^ (node_to_string msg.node)) in *)
							let s = String.to_bytes ((msg_type_to_string msg.msg_type) ^ ":" ^ (Int.to_string msg.view) ^ ":") in
							let s = Bytes.concat s [node_digest] in
							let pk = (List.nth crypto.pks msg.id) in
							Tezos_crypto.Aggregate_signature.check pk p s
						| None -> false
					)
				| None -> true
			)
		| None -> true (* other event types are verified *)

let verify_threshold_qc crypto node_count qc_0 qc =
	match crypto with
		| Some crypto ->
			if equal_qc qc_0 qc then
				true
			else
				(match qc with
					| Some qc ->
						if is_quorum_length qc.ids node_count then
							(match qc.signature with
								| Some signature ->
			        				let node_digest = match qc.node with Some n -> n.digest | None -> Bytes.empty in 
									(* let s = String.to_bytes ((msg_type_to_string qc.msg_type) ^ ":" ^ (Int.to_string qc.view) ^ ":" ^ (node_to_string qc.node)) in *)
									let s = String.to_bytes ((msg_type_to_string qc.msg_type) ^ ":" ^ (Int.to_string qc.view) ^ ":") in
									let s = Bytes.concat s [node_digest] in
									let agg = List.map (fun i -> (List.nth crypto.pks i, None, s)) qc.ids in
									Tezos_crypto.Aggregate_signature.aggregate_check agg signature
								| None -> false
							)
						else false
					| None -> false
				)
		| None -> true

let gen_keys n =
	List.init n (fun i ->
		let seed = Bytes.extend (String.to_bytes (Int.to_string i)) 31 0 in
		let (_, pk, sk) = Tezos_crypto.Aggregate_signature.generate_key ~seed:seed () in
		(sk, pk)
		)
	|> List.split