module Api = Hs_api.MakeRPC(Capnp_rpc_lwt)

exception UndefinedMessageTypeException

let msg_type_to_api_msg_type : Consensus.msg_type -> Api.MsgType_13468819503706107811.t = function
	| NewView -> NewView
	| Prepare -> Prepare
	| PrepareAck -> PrepareAck
	| PreCommit -> PreCommit
	| PreCommitAck -> PreCommitAck
	| Commit -> Commit
	| CommitAck -> CommitAck
	| Decide -> Decide
	| Generic -> Generic
	| GenericAck -> GenericAck
	| Complain -> Complain
	| NextView -> NextView

let api_msg_type_to_msg_type : Api.MsgType_13468819503706107811.t -> Consensus.msg_type = function
	| NewView -> NewView
	| Prepare -> Prepare
	| PrepareAck -> PrepareAck
	| PreCommit -> PreCommit
	| PreCommitAck -> PreCommitAck
	| Commit -> Commit
	| CommitAck -> CommitAck
	| Decide -> Decide
	| Generic -> Generic
	| GenericAck -> GenericAck
	| Complain -> Complain
	| NextView -> NextView
	| Undefined _ -> raise UndefinedMessageTypeException

let api_node_justify_to_node_justify (api_node_justify : Api.Reader.NodeJustify.t) =
	let node_offset = Int32.to_int (Api.Reader.NodeJustify.node_offset_get api_node_justify) in
	let view = Int32.to_int (Api.Reader.NodeJustify.view_get api_node_justify) in
	let signature = Tezos_crypto.Aggregate_signature.of_string_opt (Api.Reader.NodeJustify.signature_get api_node_justify) in
	let msg_type = api_msg_type_to_msg_type (Api.Reader.NodeJustify.msg_type_get api_node_justify) in
	let ids = List.map Int32.to_int (Api.Reader.NodeJustify.ids_get_list api_node_justify) in
	({node_offset = node_offset; view = view; signature = signature; msg_type = msg_type; ids = ids} : Consensus.node_justify)

let api_node_to_node (api_node : Api.Reader.Node.t) =
	let api_cmds = Api.Reader.Node.cmd_get_list api_node in
	let cmds = Consensus.Cmd_set.of_list (List.map (fun api_cmd ->
		let cmd_data = Api.Reader.Cmd.data_get api_cmd in
		let cmd_id = Api.Reader.Cmd.id_get api_cmd in
		({data = cmd_data; callback_id = cmd_id} : Consensus.cmd)
	) api_cmds) in
	let height = match Int32.to_int (Api.Reader.Node.height_get api_node) with
		| -1 -> None
		| x -> Some x
	in
	let justify = if Api.Reader.Node.has_justify api_node then
		let api_justify = Api.Reader.Node.justify_get api_node in
		Some (api_node_justify_to_node_justify api_justify)
	else
		None
	in
	let i = Consensus.create_node_internal height justify in
	let digest = Tezos_crypto.Hacl.Blake2b.Hash (String.to_bytes (Api.Reader.Node.digest_get api_node)) in
	({cmds = cmds; parent = None; i = i; digest = digest} : Consensus.node)

let rec api_node_list_to_node = function
	| api_node :: xs ->
		let node = api_node_to_node api_node in
		Some {node with parent = (api_node_list_to_node xs)}
	| [] -> None

let api_qc_to_qc (api_qc : Api.Reader.QC.t) =
	let view = Int32.to_int (Api.Reader.QC.view_get api_qc) in
	let signature = Tezos_crypto.Aggregate_signature.of_string_opt (Api.Reader.QC.signature_get api_qc) in
	let node = api_node_list_to_node (Api.Reader.QC.node_get_list api_qc) in
	let msg_type = api_msg_type_to_msg_type (Api.Reader.QC.msg_type_get api_qc) in
	let ids = List.map Int32.to_int (Api.Reader.QC.ids_get_list api_qc) in
	({node = node; view = view; signature = signature; msg_type = msg_type; ids = ids} : Consensus.qc)

let api_msg_to_consensus_event api_msg =
	let view = Int32.to_int (Api.Reader.Msg.cur_view_get api_msg) in
	let id = Int32.to_int (Api.Reader.Msg.id_get api_msg) in
	let tcp_len = Int32.to_int (Api.Reader.Msg.tcp_len_get api_msg) in
	let partial_signature = Tezos_crypto.Aggregate_signature.of_string_opt (Api.Reader.Msg.partial_signature_get api_msg) in
	let node = api_node_list_to_node (Api.Reader.Msg.node_get_list api_msg) in
	let justify = if Api.Reader.Msg.has_justify api_msg then
		let api_justify = Api.Reader.Msg.justify_get api_msg in
		Some (api_qc_to_qc api_justify)
	else
		None
	in
	let msg_type = api_msg_type_to_msg_type (Api.Reader.Msg.type_get api_msg) in
	let msg = ({id = id; view = view; tcp_len = tcp_len; msg_type = msg_type; node = node; justify = justify; partial_signature = partial_signature} : Consensus.msg) in 
	match msg_type with
		| NewView -> (NewView msg : Consensus.event)
		| Prepare -> Prepare msg
		| PrepareAck -> PrepareAck msg
		| PreCommit -> PreCommit msg
		| PreCommitAck -> PreCommitAck msg
		| Commit -> Commit msg
		| CommitAck -> CommitAck msg
		| Decide -> Decide msg
		| Generic -> Generic msg
		| GenericAck -> GenericAck msg
		| Complain -> Complain msg
		| NextView -> NextView msg

let node_justify_to_api_node_justify (builder : Api.Builder.NodeJustify.t) (node_justify : Consensus.node_justify) =
	Api.Builder.NodeJustify.msg_type_set builder (msg_type_to_api_msg_type node_justify.msg_type);
	Api.Builder.NodeJustify.view_set builder (Int32.of_int node_justify.view);
	Api.Builder.NodeJustify.node_offset_set builder (Int32.of_int node_justify.node_offset);
	let _ = Api.Builder.NodeJustify.ids_set_list builder (List.map Int32.of_int node_justify.ids) in
	(match node_justify.signature with
		| Some s -> Api.Builder.NodeJustify.signature_set builder (Tezos_crypto.Aggregate_signature.to_string s)
		| None -> ()
	)

let node_to_api_node (builder : Api.Builder.Node.t) (node : Consensus.node) =
	(* let cmd_builder = Api.Builder.Node.cmd_get builder in *)
	let cmd_builders = List.map (fun (cmd : Consensus.cmd) ->
		let builder = Api.Builder.Cmd.init_root () in
		Api.Builder.Cmd.data_set builder cmd.data;
		Api.Builder.Cmd.id_set builder cmd.callback_id;
		builder
	) (Consensus.Cmd_set.elements node.cmds) in
	let _ = Api.Builder.Node.cmd_set_list builder cmd_builders in
	(match node.i with
			| Some i ->
				let justify_builder = Api.Builder.Node.justify_get builder in
				node_justify_to_api_node_justify justify_builder i.justify;
			| None -> ()
	);
	let height = Int32.of_int (match node.i with
		| Some i ->  i.height
		| None -> -1
	) in
	Api.Builder.Node.height_set builder height;
	let Tezos_crypto.Hacl.Blake2b.Hash digest = node.digest in
	Api.Builder.Node.digest_set builder (String.of_bytes digest)

let rec node_to_api_node_list = function
	| None -> [] 
	| Some node ->
		let builder = Api.Builder.Node.init_root () in
		node_to_api_node builder node;
		builder :: (node_to_api_node_list node.parent)

let qc_to_api_qc (builder : Api.Builder.QC.t) (qc : Consensus.qc) =
	Api.Builder.QC.msg_type_set builder (msg_type_to_api_msg_type qc.msg_type);
	Api.Builder.QC.view_set builder (Int32.of_int qc.view);
	let _ = Api.Builder.QC.ids_set_list builder (List.map Int32.of_int qc.ids) in
	(match qc.signature with
		| Some s -> Api.Builder.QC.signature_set builder (Tezos_crypto.Aggregate_signature.to_string s)
		| None -> ()
	);
	let _ = Api.Builder.QC.node_set_list builder (node_to_api_node_list qc.node) in
	()

let msg_to_api_msg (builder : Api.Builder.Msg.t) (msg : Consensus.msg) =
	let api_msg_type = msg_type_to_api_msg_type msg.msg_type in
	Api.Builder.Msg.cur_view_set builder (Int32.of_int msg.view);
	Api.Builder.Msg.type_set builder api_msg_type;
	Api.Builder.Msg.id_set builder (Int32.of_int msg.id);
	Api.Builder.Msg.tcp_len_set builder (Int32.of_int msg.tcp_len);
	let _ = Api.Builder.Msg.node_set_list builder (node_to_api_node_list msg.node) in
	(match msg.justify with
			| Some justify ->
				let justify_builder = Api.Builder.Msg.justify_get builder in
				qc_to_api_qc justify_builder justify;
			| None -> ()
	);
	(match msg.partial_signature with
			| Some s ->
				Api.Builder.Msg.partial_signature_set builder (Tezos_crypto.Aggregate_signature.to_string s);
			| None -> ()
	)