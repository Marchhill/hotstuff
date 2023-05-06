open Cmdliner

let start_node id nodes batch_size timeout verbose =
	Lwt_main.run begin
		(* let listen_address = `TCP ("0.0.0.0", 9000) in *)
		(* let listen_address = `TCP ("127.0.0.1", 9000 + id) in
		let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
		let service_id = Capnp_rpc_net.Restorer.Id.public "" in
    let node_state = Lib.init id nodes timeout batch_size verbose in
		let restore = Capnp_rpc_net.Restorer.single service_id (Lib.serve node_state) in
		let* vat = Capnp_rpc_unix.serve config ~restore in
		let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
		Fmt.pr "Server ID=%s running. Connect to URI %S.@." (Int.to_string id) (Uri.to_string uri);
		Lib.main_loop node_state *)
    Lib.start_server id nodes batch_size timeout verbose
	end

let id =
	let doc = "Node ID. Server will run on port 9000 + ID" in
	Arg.(value & opt int 0 & info ["i"; "id"] ~docv:"ID" ~doc)

let nodes =
	let doc = "Node count." in
	Arg.(value & opt int 0 & info ["n"; "nodes"] ~docv:"NODES" ~doc)

let verbose =
	let doc = "Output info about messages and state machine." in
	Arg.(value & flag & info ["v"; "verbose"] ~docv:"VERBOSE" ~doc)

let batch_size =
	let doc = "Batch size." in
	Arg.(value & opt int 300 & info ["b"; "batch"] ~docv:"BATCH" ~doc)

let timeout =
	let doc = "View timeout." in
	Arg.(value & opt float 1000. & info ["t"; "timeout"] ~docv:"TIMEOUT" ~doc)

let cmd =
	let doc = "run a hotstuff node" in
	let info = Cmd.info "hs" ~version:"%‌%VERSION%%" ~doc in
	Cmd.v info Term.(const start_node $ id $ nodes $ batch_size $ timeout $ verbose)

let () =
	(* Util.init_logging () *)
	(* Memtrace.trace_if_requested (); *)
	Random.self_init ();
	exit (Cmd.eval cmd);
