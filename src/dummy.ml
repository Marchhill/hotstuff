open Cmdliner
open Lwt.Syntax
open Capnp_rpc_lwt
open Api_wrapper

let secret_key = `Ephemeral

let local =
	let module Hs = Api.Service.Hs in
	Hs.local @@ object
		inherit Hs.service

		(* message from another node *)
		method send_msg_impl _params release_param_caps =
			release_param_caps ();
			Service.return_empty ()
		
		(* request from client to commit some command *)
		method client_req_impl _params release_param_caps =
			let open Hs.ClientReq in
			release_param_caps ();
			let response, results = Service.Response.create Results.init_pointer in
			Results.success_set results true;
			Service.return response
		
		(* request from client to end our experiment and quit *)
		method quit_impl _params release_param_caps =
			release_param_caps ();
			exit 0
	end
let start_node id =
	Lwt_main.run begin
		let listen_address = `TCP ("127.0.0.1", 9000 + id) in
		let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
		let service_id = Capnp_rpc_net.Restorer.Id.public "" in
		let restore = Capnp_rpc_net.Restorer.single service_id local in
		let* vat = Capnp_rpc_unix.serve config ~restore in
		let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
		Fmt.pr "Server ID=%s running. Connect to URI %S.@." (Int.to_string id) (Uri.to_string uri);
		fst (Lwt.wait ())
	end

let id =
	let doc = "Node ID. Server will run on port 9000 + ID" in
	Arg.(value & opt int 0 & info ["i"; "id"] ~docv:"ID" ~doc)

let cmd =
	let doc = "run a dummy node" in
	let info = Cmd.info "dummy" ~version:"%‌%VERSION%%" ~doc in
	Cmd.v info Term.(const start_node $ id)

let () =
	exit (Cmd.eval cmd);
