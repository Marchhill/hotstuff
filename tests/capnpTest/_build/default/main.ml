open Lwt.Syntax
open Capnp_rpc_lwt

let rec print_results sum sqSum count overallMean = function
	| (reply, elapsed)::xs -> Fmt.pr "Got reply %S in %fµs@." reply elapsed;
		print_results (sum +. elapsed) (sqSum +. (elapsed *. elapsed)) (count + 1) overallMean xs
	| [] -> let mean = (sum /. (float_of_int count)) in
		let sd = sqrt((sqSum /. (float_of_int count)) -. (mean *. mean)) in
		Fmt.pr "Mean response time: %fµs\nStandard deviation: %fµs\nTotal time: %fµs@." mean sd overallMean;
		Lwt.return ()

let print_results = print_results 0. 0. 0

let () =
	Logs.set_level (Some Logs.Warning);
	Logs.set_reporter (Logs_fmt.reporter ())

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 9000)
	
let start_server () =
	let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
	let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
	let restore = Capnp_rpc_net.Restorer.single service_id Echo.local in
	let+ vat = Capnp_rpc_unix.serve config ~restore in
	Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let run_client msg sr = Capability.with_ref sr (Echo.ping msg)

let () =
	Lwt_main.run begin
		let reqs = 10000 in
		let* uri = start_server () in
		let sr = Capnp_rpc_unix.Vat.import_exn (Capnp_rpc_unix.client_only_vat ()) uri in
		let start = Sys.time () in
		let* results = Lwt_list.map_s
			(fun msg -> Sturdy_ref.with_cap_exn sr (run_client msg))
			(List.init reqs (fun x -> "ping #" ^ (string_of_int x)))
		in
		let elapsed = (Sys.time () -. start) *. 1000000. in
		print_results (elapsed /. (float_of_int reqs)) results
	end