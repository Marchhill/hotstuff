open Cmdliner
open Lwt.Syntax

let secret_key = `Ephemeral
let timeout = 0.5

(* Verbose logging *)

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Stdint.Uint32.to_string x in
    Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout ("%a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let start_node id nodes batch_size verbose =
	Lwt_main.run begin
		let listen_address = `TCP ("0.0.0.0", 9000) in
		let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
		let service_id = Capnp_rpc_net.Restorer.Id.public "" in
    let node_state = Hs.init id nodes timeout batch_size verbose in
		let restore = Capnp_rpc_net.Restorer.single service_id (Hs.local node_state) in
		let* vat = Capnp_rpc_unix.serve config ~restore in
		let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
		Fmt.pr "Server ID=%s running. Connect to URI %S.@." (Int.to_string id) (Uri.to_string uri);
		Hs.main_loop node_state
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

let cmd =
	let doc = "run a hotstuff node" in
	let info = Cmd.info "hs" ~version:"%‌%VERSION%%" ~doc in
	Cmd.v info Term.(const start_node $ id $ nodes $ batch_size $ verbose)

let () =
	(*Fmt_tty.setup_std_outputs ();
	Logs.set_reporter reporter;
	Logs.set_level ~all:true (Some Logs.Info);
	Logs.Src.list () |> List.iter (fun src ->
		if Astring.String.is_prefix ~affix:"capnp" (Logs.Src.name src) then
		Logs.Src.set_level src (Some Logs.Debug);
	);
	Memtrace.trace_if_requested ();
	*)
	Random.self_init ();
	exit (Cmd.eval cmd);
