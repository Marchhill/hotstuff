open Types

let leader_id view node_count = (view - 1) mod node_count

let is_leader view id node_count = id = (leader_id view node_count)

let is_quorum_length l n =
	let f = (n - 1) / 3 in
	(List.length l) >= n - f

let rec node_to_string : node option -> string = function
	| Some x ->
		let cmds_str = Cmd_set.fold (fun cmd acc -> "\"" ^ cmd.data ^ "\"," ^ acc) x.cmds "" in
		"[" ^ cmds_str ^ "]-" ^ (node_to_string x.parent)
	| None -> "⊥"

let rec node_to_string_trunc depth : node option -> string = function
	| Some x ->
      if depth = 0 then
        "..."
      else
        let cmds_str = Cmd_set.fold (fun cmd acc -> "\"" ^ cmd.data ^ "\"," ^ acc) x.cmds "" in
		"[" ^ cmds_str ^ "]-" ^ (node_to_string_trunc (depth - 1) x.parent)
	| None -> "⊥"

let node_to_string_short = node_to_string_trunc 5

let msg_type_to_string : msg_type -> string = function
	| NewView -> "new_view"
	| Prepare -> "prepare"
	| PrepareAck -> "prepare_ack"
	| PreCommit -> "pre_commit"
	| PreCommitAck -> "pre_commit_ack"
	| Commit -> "commit"
	| CommitAck -> "commit_ack"
	| Decide -> "decide"
	| Complain -> "complain"
	| NextView -> "next_view"
	| Generic -> "generic"
	| GenericAck -> "generic_ack"

let partial_signature_to_string id = function
	| Some _ -> Fmt.str "#%d" id
	| None -> "⊥"

let qc_to_string : qc option -> string = function
	| Some qc ->
		let signature = (match qc.signature with
			| Some _ ->
				List.mapi (fun i id -> if i = (List.length qc.ids) - 1 then (Fmt.str "#%d" id) else (Fmt.str "#%d,") id) qc.ids
				|> List.fold_left (^) ""
			| None -> "⊥") in
		Fmt.str "view=%d type=%s node=(%s) sig=(%s)" qc.view (msg_type_to_string qc.msg_type) (node_to_string qc.node) signature
	| None -> ""

let node_justify_to_string : node_justify option -> string = function
	| Some qc ->
		let signature = (match qc.signature with
			| Some _ ->
				List.mapi (fun i id -> if i = (List.length qc.ids) - 1 then (Fmt.str "#%d" id) else (Fmt.str "#%d,") id) qc.ids
				|> List.fold_left (^) ""
			| None -> "⊥") in
		Fmt.str "view=%d type=%s node_offset=(%d) sig=(%s)" qc.view (msg_type_to_string qc.msg_type) qc.node_offset signature
	| None -> ""

let msg_to_string m = Fmt.str "view=%d src=%d type=%s node=(%s) justify=(%s) sig=(%s)" m.view m.id (msg_type_to_string m.msg_type) (node_to_string m.node) (qc_to_string m.justify) (partial_signature_to_string m.id m.partial_signature)

let get_event_type : event -> string = function
	| NewView _ -> "new_view"
	| Prepare _ -> "prepare"
	| PrepareAck _ -> "prepare_ack"
	| PreCommit _ -> "pre_commit"
	| PreCommitAck _ -> "pre_commit_ack"
	| Commit _ -> "commit"
	| CommitAck _ -> "commit_ack"
	| Decide _ -> "decide"
	| NextView _ -> "next_view"
	| Generic _ -> "generic"
	| GenericAck _ -> "generic_ack"
	| ClientCmd _ -> "client_cmd"
	| Timeout _ -> "timeout"
	| Complain _ -> "complain"

let get_msg_from_event : event -> msg option = function
	| NewView m -> Some m
	| Prepare m -> Some m
	| PrepareAck m -> Some m
	| PreCommit m -> Some m
	| PreCommitAck m -> Some m
	| Commit m -> Some m
	| CommitAck m -> Some m
	| Decide m -> Some m
	| NextView m -> Some m
	| Generic m -> Some m
	| GenericAck m -> Some m
	| ClientCmd _ -> None
	| Timeout _ -> None
	| Complain m -> Some m

let print_action = function
	| Broadcast m -> Fmt.pr "%d: broadcast %s@." m.id (msg_to_string m)
	| SendLeader m -> Fmt.pr "%d: send_leader %s@." m.id (msg_to_string m)
	| SendNextLeader m -> Fmt.pr "%d: send_next_leader %s@." m.id (msg_to_string m)
	| SendClient m -> Fmt.pr "%d: send_client success=%b callback_id=\"%s\"@." m.id m.success (Int64.to_string m.callback_id)
	| Execute m -> Fmt.pr "%d: execute node=%s@." m.id (node_to_string_short (Some m.node))
	| ResetTimer m -> Fmt.pr "%d: reset_timer view=%d@." m.id m.view

let print_event = function
	| NewView m -> Fmt.pr "new_view %s@." (msg_to_string m)
	| Prepare m -> Fmt.pr "prepare %s@." (msg_to_string m)
	| PrepareAck m -> Fmt.pr "prepare_ack %s@." (msg_to_string m)
	| PreCommit m -> Fmt.pr "pre_commit %s@." (msg_to_string m)
	| PreCommitAck m -> Fmt.pr "pre_commit_ack %s@." (msg_to_string m)
	| Commit m -> Fmt.pr "commit %s@." (msg_to_string m)
	| CommitAck m -> Fmt.pr "commit_ack %s@." (msg_to_string m)
	| Decide m -> Fmt.pr "decide %s@." (msg_to_string m)
	| NextView m -> Fmt.pr "next_view %s@." (msg_to_string m)
	| Generic m -> Fmt.pr "generic %s@." (msg_to_string m)
	| GenericAck m -> Fmt.pr "generic_ack %s@." (msg_to_string m)
	| ClientCmd cmd -> Fmt.pr "client_cmd data=\"%s\" callback_id=\"%s\"@." cmd.data (Int64.to_string cmd.callback_id)
	| Timeout x -> Fmt.pr "timeout %d@." x.view
	| Complain m -> Fmt.pr "complain %s@." (msg_to_string m)

let rec node_nth n node =
	if n = 0 then
		node
	else
		(match node.parent with
			| Some parent -> node_nth (n - 1) parent
			| None -> raise MissingNodeException
		)

let make_node (cmds : Cmd_set.t) parent (i : node_internal option) =
	(* ??? make sure padding is unambiguous!! *)
	let parent_digest = (match parent with Some p -> (String.of_bytes p.digest) | None -> "") in
	let i_str = (match i with Some i -> Fmt.str "%s:%d" (node_justify_to_string (Some i.justify)) i.height | None -> "") in
	let cmds_str = Cmd_set.fold (fun cmd acc -> acc ^ cmd.data ^ ":" ^ (Int64.to_string cmd.callback_id) ^ ",") cmds "" in
	let digest = Tezos_crypto.Blake2B.hash_string [cmds_str; parent_digest; i_str] in
	{cmds = cmds; parent = parent; i = i; digest = (Tezos_crypto.Blake2B.to_bytes digest)}

let equal_nodes n1 n2 = match n1, n2 with
	| Some n1, Some n2 -> n1.digest = n2.digest
	| None, None -> true
	| _ -> false

let equal_qc (qc1 : qc option) (qc2 : qc option) = match qc1, qc2 with
	| Some qc1, Some qc2 ->
		equal_nodes qc1.node qc2.node &&
		qc1.view = qc2.view &&
		qc1.signature = qc2.signature &&
		qc1.msg_type = qc2.msg_type &&
		qc1.ids = qc2.ids
	| None, None -> true
	| _ -> false

let matching_qc (qc : qc) (msg_type : msg_type) view = qc.msg_type = msg_type && qc.view = view

let rec extends n1 n2 =
	match n1, n2 with
		| Some _n1, Some _ -> (equal_nodes n1 n2) || (extends _n1.parent n2)
		| _, None -> true
		| None, Some _ -> false

let get_node_height n =
	match n.i with
		| Some i -> i.height
		| None -> raise NodeInternalException

let get_node_justify n =
	match n.i with
		| Some i -> i.justify
		| None -> raise NodeInternalException

let get_node_from_qc (qc : qc) =
	match qc.node with
		| Some n -> n
		| None -> raise MissingNodeException

let qc_from_node_justify n =
	let justify = get_node_justify n in
	{node = Some (node_nth justify.node_offset n); view = justify.view; signature = justify.signature; msg_type = justify.msg_type; ids = justify.ids} 

let rec add_dummy_nodes (n : node) = function
	| 0 -> n
	| x -> add_dummy_nodes (make_node Cmd_set.empty (Some n) (Some {height = get_node_height n; justify = get_node_justify n})) (x - 1)

let rec trim_node (n : node) x = match n.parent, x with
	| _, 0 -> {n with parent = None}
	| Some p, x -> {n with parent = Some (trim_node p (x - 1))}
	| None, _ -> n

let rec combine_nodes n1 n2 =
	if equal_nodes n1 n2 then
		n2
	else (
		match n1 with
			| Some n1 -> Some {n1 with parent = (combine_nodes n1.parent n2)}
			| None -> None
	)

(* takes a list of events and returns a subset that forms a quorum if one exist *)
let get_quorum (l : event list) n =
	(* group messages by content *)
	List.fold_left (fun acc e ->
		let msg = (match get_msg_from_event e with Some msg -> msg | None -> raise ThresholdQCException) in
		let lookup = ((get_event_type e), msg.view, (node_to_string msg.node)) in
		let matching = List.assoc_opt lookup acc in
		(match matching with
			| Some matching -> (lookup, e :: matching) :: acc
			| None -> (lookup, [e]) :: acc
		)
	) [] l
	(* only keep list of events *)
	|> List.map snd
	(* find group large enough to be quorum *)
	|> List.find_opt (fun l -> is_quorum_length l n)

let is_quorum l n = Option.is_some (get_quorum l n)

let add_event votes_table event view node_count =
	let v = match Hashtbl.find_opt votes_table view with Some v -> v | None -> [] in
	if (is_quorum v node_count) then
		None
	else (
		let v' = (event :: v) in
		Hashtbl.replace votes_table view v';
		get_quorum v' node_count
	)