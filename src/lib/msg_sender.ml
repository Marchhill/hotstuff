(* open Api_wrapper *)
open Capnp_rpc_lwt
open Util
open Types
open Lwt.Syntax

let rec connect service t =
	let* r = Sturdy_ref.connect service in
	match r with
		| Ok conn -> Capability.when_released conn (fun () -> Fmt.pr "conn released!!@.");Lwt.return conn
		| Error _ ->
			let* () = Lwt_unix.sleep t in
			Fmt.pr "backed off %f...@." t;
			connect service (t *. 2.) (* binary exponential backoff *)

let open_conn vat id =
	let uri = Uri.of_string ("capnp://insecure@10.0.0." ^ (Int.to_string (id + 1)) ^ ":9000") in
	(* let uri = Uri.of_string ("capnp://insecure@127.0.0.1:" ^ (Int.to_string (id + 9000))) in *)
	let sr = Capnp_rpc_unix.Vat.import_exn vat uri in
	(sr, ref (connect sr 0.1))

let open_conns nodes =
	let client_vat = Capnp_rpc_unix.client_only_vat () in
	let ids = List.init nodes Fun.id in
	List.map (open_conn client_vat) ids

let get_cap (sr, cap) =
	let* old_cap = !cap in
	(match Capability.problem old_cap with
		| Some ex ->
			Fmt.pr "connection broken!!! %s@." ex.reason;
			cap := connect sr 0.01;
			!cap
		| None -> Lwt.return old_cap
	)

(* used by nodes to communicate with eachother *)
let send_msg conn (msg : Consensus.msg) stats =
	let send cap =
		let open Api_wrapper.Api.Client.Hs.SendMsg in
		let _t1 = Time_now.nanoseconds_since_unix_epoch () in
		let request, params = Capability.Request.create Params.init_pointer in
		let msg_builder = Params.msg_get params in
		let _t2 = Time_now.nanoseconds_since_unix_epoch () in
		Api_wrapper.msg_to_api_msg msg_builder msg;
		let _t3 = Time_now.nanoseconds_since_unix_epoch () in
		let* () = Capability.call_for_unit_exn cap method_id request in
		let _t4 = Time_now.nanoseconds_since_unix_epoch () in
		Lwt.return ()
	in
	Lwt.async(fun () ->
		let t1 = Time_now.nanoseconds_since_unix_epoch () in
		let* cap = get_cap conn in
		let t2 = Time_now.nanoseconds_since_unix_epoch () in
		Capability.inc_ref cap;
		let* _ = Capability.with_ref cap send in
		let t3 = Time_now.nanoseconds_since_unix_epoch () in
		stats.connection_times := (delta t1 t2) :: !(stats.connection_times);
		stats.send_times := (delta t2 t3) :: !(stats.send_times);
		Lwt.return_unit
	);
	Lwt.return_unit

(* send a command to a node (used by client) *)
let send_req conn (cmd : Consensus.cmd) _t stats =
	let send cap =
		let open Api_wrapper.Api.Client.Hs.ClientReq in
		let request, params = Capability.Request.create Params.init_pointer in
		let cmd_builder = Params.cmd_get params in
		Api_wrapper.cmd_to_api_cmd cmd_builder cmd;
		let* res = Capability.call_for_value cap method_id request in
		let success = match res with
			| Ok r -> Results.success_get r
			| Error _ -> false
		in
		Lwt.return success
	in
	(* asychronously wait t seconds then timeout *)
	let timeout_p, timeout_r = Lwt.task () in
	Lwt.async (fun () ->
		let* () = Lwt_unix.sleep _t in
		(match Lwt.state timeout_p with
			| Lwt.Return _ -> ()
			| Lwt.Fail _ -> ()
			| Lwt.Sleep ->
				Fmt.pr "request timed out: %s@." cmd.data;
				Lwt.wakeup timeout_r false);
		Lwt.return_unit);
	let dispatch_p, dispatch_r = Lwt.task () in
	Lwt.async(fun () ->
		let t1 = Time_now.nanoseconds_since_unix_epoch () in
		let* cap = get_cap conn in
		let t2 = Time_now.nanoseconds_since_unix_epoch () in
		Capability.inc_ref cap;
		let* r = Capability.with_ref cap send in
		(match Lwt.state dispatch_p with
		| Lwt.Return _ -> ()
		| Lwt.Fail _ -> ()
		| Lwt.Sleep ->
			Lwt.wakeup dispatch_r r);
		let t3 = Time_now.nanoseconds_since_unix_epoch () in
		stats.connection_times := (delta t1 t2) :: !(stats.connection_times);
		stats.send_times := (delta t2 t3) :: !(stats.send_times);
		Lwt.return_unit
	);
	(* either timeout or return result*)
	Lwt.pick [timeout_p; dispatch_p]

let send_quit conn =
	let send cap =
		let open Api_wrapper.Api.Client.Hs.Quit in
		let request = Capability.Request.create_no_args () in
		(* let* () = Capability.call_for_unit_exn cap method_id request in *)
		let* _ = Capability.call_for_unit cap method_id request in
		Lwt.return ()
	in
	let* cap = get_cap conn in
	Capability.inc_ref cap;
	let* () = Capability.with_ref cap send in
	Lwt.return_unit