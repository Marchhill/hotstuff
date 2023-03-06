open Lwt.Syntax
(* open Capnp_rpc_lwt *)

let rec print_results sum sqSum count = function
	| (elapsed, conn)::xs -> Fmt.pr "Got reply in %fs, conn in %fs@." elapsed conn;
		print_results (sum +. elapsed) (sqSum +. (elapsed *. elapsed)) (count + 1) xs
	| [] -> let mean = (sum /. (float_of_int count)) in
		let sd = sqrt((sqSum /. (float_of_int count)) -. (mean *. mean)) in
		Fmt.pr "Mean response time: %fs\nStandard deviation: %fs\n@." mean sd;
		Lwt.return ()

let print_results = print_results 0. 0. 0

let () =
	Logs.set_level (Some Logs.Warning);
	Logs.set_reporter (Logs_fmt.reporter ())

let secret_key = `Ephemeral
	
let start_server id =
	let listen_address = `TCP ("127.0.0.1", 9000 + id) in
	let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
	(* let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in *)
	let service_id = Capnp_rpc_net.Restorer.Id.public "" in
	let restore = Capnp_rpc_net.Restorer.single service_id (Echo.local id) in
	let+ vat = Capnp_rpc_unix.serve config ~restore in
	let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
	Fmt.pr "Server running. Connect to URI %S.@." (Uri.to_string uri)

(* let run_client msg sr = Capability.with_ref sr (Echo.ping msg) *)
let rec repeat s = function 0 -> s | n -> s ^ (repeat s (n - 1))

let () =
	Lwt_main.run begin
		let reqs = 1000 in
		let rate = 50. in
		let* () = start_server 0 in
		let* () = start_server 1 in
		let* () = start_server 2 in
    	let conns = Echo.open_conns 3 in
		(* let sr = Capnp_rpc_unix.Vat.import_exn (Capnp_rpc_unix.client_only_vat ()) uri in *)
		let* results = Lwt_list.mapi_p
			(fun i msg ->
				let* () = Lwt_unix.sleep ((Float.of_int i) /. rate) in
				Echo.ping_msg (List.nth conns (i mod 3)) msg
				(* Sturdy_ref.with_cap_exn sr (run_client msg) *)
			)
			(List.init reqs (fun x -> repeat ("ping #" ^ (Int.to_string x)) 1000))
		in
		print_results results
	end