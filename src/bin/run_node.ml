open Cmdliner
open Lwt.Infix

let start_node id nodes batch_size timeout verbose =
	Lwt_main.run begin
    	Lib.start_server id nodes batch_size timeout verbose >>=
  		Lib.main_loop
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
