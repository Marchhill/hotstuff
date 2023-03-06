module Api = Echo_api.MakeRPC(Capnp_rpc_lwt)

open Capnp_rpc_lwt
open Lwt.Syntax

let local id =
	let module Echo = Api.Service.Echo in
	Echo.local @@ object
		inherit Echo.service

		method ping_impl params release_param_caps =
			let open Echo.Ping in
			let msg = Params.msg_get params in
      		Fmt.pr "%d recv: %s@." id msg;
			release_param_caps ();
			(*Service.return_lwt (fun () ->
				let* () = Lwt_unix.sleep 2. in
				Lwt_result.return (Service.Response.create_empty ())
			)*)
			Service.return_empty ()
	end


let rec connect service t =
		let* r = Sturdy_ref.connect service in
		match r with
			| Ok conn -> Lwt.return conn
			| Error _ ->
				let* () = Lwt_unix.sleep t in
				Fmt.pr "backed off %f...@." t;
				connect service (t *. 2.) (* binary exponential backoff *)

let open_conn vat id  =
	let uri = Uri.of_string ("capnp://insecure@127.0.0.1:" ^ Int.to_string (id + 9000)) in
	let sr = Capnp_rpc_unix.Vat.import_exn vat uri in
	(sr, ref (connect sr 0.1))
	
let open_conns nodes =
	let client_vat = Capnp_rpc_unix.client_only_vat () in
	let ids = List.init nodes Fun.id in
	List.map (open_conn client_vat) ids

(* calculate delta between timestamps in seconds s*)
let delta x y =
	let open Base.Int63 in
	(to_float (y - x)) /. 1_000_000_000.

let get_cap (sr, cap) =
	let* old_cap = !cap in
	(match Capability.problem old_cap with
		| Some ex ->
			Fmt.pr "connection broken!!! %s@." ex.reason;
			cap := connect sr 0.01;
			!cap
		| None -> Fmt.pr "reusing@."; Lwt.return old_cap
	)

let ping_msg conn msg =
	let ping cap =
		let open Api.Client.Echo.Ping in
		let request, params = Capability.Request.create Params.init_pointer in
		Params.msg_set params msg;
		let t1 = Time_now.nanoseconds_since_unix_epoch () in
		let* () = Capability.call_for_unit_exn cap method_id request in
		let t2 = Time_now.nanoseconds_since_unix_epoch () in
		Lwt.return (delta t1 t2)
	in
	let t1 = Time_now.nanoseconds_since_unix_epoch () in
	let* cap = get_cap conn in
	let t2 = Time_now.nanoseconds_since_unix_epoch () in
	Capability.inc_ref cap;
	let* t = Capability.with_ref cap ping in
	Lwt.return((t, delta t1 t2))

(* used by nodes to communicate with eachother *)
(*let send_msg conn (msg : Consensus.msg) =
	let send cap =
		let open Api.Client.Hs.SendMsg in
		let t1 = Time_now.nanoseconds_since_unix_epoch () in
		let request, params = Capability.Request.create Params.init_pointer in
		let msg_builder = Params.msg_get params in
		let t2 = Time_now.nanoseconds_since_unix_epoch () in
		msg_to_api_msg msg_builder msg;
		let t3 = Time_now.nanoseconds_since_unix_epoch () in
		(* let* () = Lwt_unix.sleep 0.01 in *)
		let* () = Capability.call_for_unit_exn cap method_id request in
		let t4 = Time_now.nanoseconds_since_unix_epoch () in
		Fmt.pr "send init = %fs, conv = %fs, dispatch = %fs@." (delta t1 t2) (delta t2 t3) (delta t3 t4);
		Lwt.return ()
	in
	let t1 = Time_now.nanoseconds_since_unix_epoch () in
	let* cap = get_cap conn in
	let t2 = Time_now.nanoseconds_since_unix_epoch () in
	Capability.inc_ref cap; (* increase ref count to stop conenction being released ??? *)
	let* () = Capability.with_ref cap send in
	let t3 = Time_now.nanoseconds_since_unix_epoch () in
	Fmt.pr "msg cap = %fs, send = %fs@." (delta t1 t2) (delta t2 t3);
Lwt.return_unit*)