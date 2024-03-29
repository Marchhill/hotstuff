open Lwt.Syntax
open Lwt.Infix

(* calculate delta between timestamps in seconds s*)
let delta x y =
	let open Base.Int63 in
	(to_float (y - x)) /. 1_000_000_000.
let sent = ref 0
let callbacks = ref 0

let gen_callback_id () = callbacks := !callbacks + 1; !callbacks

(* wait until we send a request to some node and it is commited *)
let rec await_first_commit i conn timeout retries =
	if retries = 0 then
		Lwt.return false
	else (
		let tmp = Lib.empty_stats (Base.Int63.zero) in (* these stats are discarded *)
		let* success = Lib.send_req conn ({data = "init"; callback_id = (gen_callback_id ())} : Consensus.cmd) timeout tmp in
		if success then (
			Fmt.pr "connected to %d!@." i;
			Lwt.return true
		)
		else (
			await_first_commit i conn timeout (retries - 1)
		)
	)

(* wait until we send a request to each node and they are all commited *)
let await_connections conns timeout retries =
	List.mapi (fun i conn -> await_first_commit i conn timeout retries) conns
	|> Lwt.all
	>|= List.fold_left (&&) true

let run_command conns timeout stats msg_size =
	(* let id = !sent mod (List.length conns) in
	let conn = List.nth conns id in *)
	let data = "" in
	let callback_id = gen_callback_id () in
	let data = if msg_size = 1 then
		data
	else
		List.fold_left (fun acc x -> acc ^ x) "" (List.init msg_size (fun _ -> "x"))
	in
	sent := !sent + 1;
	let res = List.map (fun conn ->
		Lib.send_req conn ({data = data; callback_id = callback_id} : Consensus.cmd) timeout stats
	) conns in
	Lwt.pick res

let benchmark conns res rate timeout msg_size t =
	let s = Lib.empty_stats (Base.Int63.zero) in
	let period = Base.Int63.of_float((1. /. rate) *. 1_000_000_000.) in
	let rec aux (i : int) (target : Base.Int63.t) =
		if i >= (Array.length res) then
			Lwt.return_unit
		else (
			(* yield every 100 iterations *)
			let* () = if (i mod 1000) = 0 then
				Lwt.pause ()
			else
				Lwt.return_unit
			in
			(* sleep if not yet time to deliver next command *)
			let now = Time_now.nanoseconds_since_unix_epoch () in
			let* () = if Base.Int63.(<) now target then (
				Lwt_unix.sleep (Lib.delta now target)
			)
			else
				Lwt.return_unit
			in
			Lwt.async (fun () ->
				let x = Base.Int63.(-) (Time_now.nanoseconds_since_unix_epoch ()) t in
				let* success = run_command conns timeout s msg_size in
				let y = Base.Int63.(-) (Time_now.nanoseconds_since_unix_epoch ()) t in
				let ret = if success then Some (x, y) else None in
				Lwt.return (Array.set res i ret)
			);
			aux (i + 1) (Base.Int63.(+) target period)
		)
	in
	let* () = aux 0 t in
	Lwt.return s

let run_client nodes version time rate req_times_fp stats_fp msg_size batch_size =
	Lwt_main.run begin
		let conns = Lib.open_conns nodes in
		let* connected = await_connections conns 5. 10 in
		if not connected then (
			Fmt.epr "abort!@.";
			exit 1;
		);
		(*Lwt.async (fun () ->
		  let* () = Lwt_unix.sleep 5. in
			Lib.send_quit (List.hd conns)
		); view change experiment *)
		Fmt.pr "connected to all!@.";
		(* run and ignore results for 3s to allow batches to fill up *)
(*		let startup = Array.make (3 * rate) None in
		let* _ = benchmark conns startup (Float.of_int rate) 1000. msg_size (Base.Int63.zero) in*)
		(* begin actual benchamrking*)
		let n = time * rate in (* calculate based on actual number sent (filter)*)
		let res = Array.make n None in
		let start_time = Time_now.nanoseconds_since_unix_epoch () in
		let* stats = benchmark conns res (Float.of_int rate) 1000. msg_size start_time in
		(* wait 15 seconds to recieve any outstanding requests *)
		Fmt.pr "waiting 15s for responses...@.";
		let* () = Lwt_unix.sleep 15. in
		(* output request times to csv file*)
		let name = match req_times_fp with
			| Some s ->
				Out_channel.with_open_text s (fun oc ->
					Printf.fprintf oc "sent, rec\n";
					for i = 0 to (n - 1) do
						match res.(i) with
							| Some r ->
								let s = Base.Int63.to_string (fst r) in
								let e = Base.Int63.to_string (snd r) in
								Printf.fprintf oc "%s, %s\n" s e
							| None -> ()
					done
				);
				Filename.basename s |> Filename.remove_extension
			| None -> ""
		in
		let success = Array.fold_left (fun acc -> function Some _ -> acc + 1 | None -> acc) 0  res in
		let elapsed =
			let open Base.Int63 in
			let (lo, hi) = Array.fold_left (fun (lo, hi) -> function Some (_, x) -> (min lo x, max hi x) | None -> (lo, hi)) (max_value, min_value) res in
			delta lo hi
		in
    	let elapsed = max elapsed (Float.of_int time) in
		let goodput = (Float.of_int success) /. elapsed in
		let res = Array.map (function (Some (x, y)) -> delta x y | None -> 0.) res in
		let sum = Array.fold_left (+.) 0. res in
		let sum_sq = Array.fold_left (fun acc x -> acc +. (x *. x)) 0. res in
		let mean = sum /. (Float.of_int success) in
		let sd = Float.sqrt ((sum_sq /. (Float.of_int success)) -. (mean *. mean)) in
		let version = match version with Some v -> v | None -> "x" in
		Fmt.pr "\nname = %s\nversion = %s\nnodes = %d \n reqs = %d / %d\nthroughput = %dreq/s\ngoodput = %freq/s\nmean = %fs\nsd = %fs@." name version nodes success n rate goodput mean sd;
		Lib.print_stats !(stats.connection_times) "req_conn" "s" 12345;
		Lib.print_stats !(stats.send_times) "req_send" "s" 12345;
		(* append stats to csv file*)
		(match stats_fp with
			| Some s ->
				Out_channel.with_open_gen [Open_append] 0o666 s (fun oc ->
					Printf.fprintf oc "%s, %s, %d, %d, %f, %f, %f, %d, %d, %s, %d\n" name version nodes rate goodput mean sd success n batch_size msg_size
				)
			| None -> ());
		let _ = List.map (fun conn ->
			Lwt.async (fun () -> Lib.send_quit conn)
		) conns in
		Lwt.return ()
	end

open Cmdliner

let nodes =
	let doc = "Node count." in
	Arg.(required & pos 0 (some int) None & info [] ~docv:"NODES" ~doc)

let version =
	let doc = "Ablation version." in
	Arg.(value & opt (some string) None & info ["v"; "version"] ~docv:"VERSION" ~doc)

let req_times_fp =
	let doc = "Filepath to append request times to." in
	Arg.(value & opt (some string) None & info ["times"] ~docv:"TIMES" ~doc)

let stats_fp =
	let doc = "Filepath to append stats to." in
	Arg.(value & opt (some string) None & info ["stats"] ~docv:"STATS" ~doc)

let time =
	let doc = "Time to run experiment for (in seconds)." in
	Arg.(value & opt int 100 & info ["t"; "time"] ~docv:"TIME" ~doc)

let throughput =
	let doc = "Rate at which to send requests." in
	Arg.(value & opt int 10 & info ["r"; "rate"] ~docv:"RATE" ~doc)

let msg_size =
	let doc = "Message size." in
	Arg.(value & opt int 1 & info ["s"; "size"] ~docv:"SIZE" ~doc)

let batch_size =
	let doc = "Batch size." in
	Arg.(value & opt string "1" & info ["b"; "batch"] ~docv:"BATCH_SIZE" ~doc)

let connect_cmd =
	let doc = "run the client" in
	let info = Cmd.info "connect" ~doc in
	Cmd.v info Term.(const run_client $ nodes $ version $ time $ throughput $ req_times_fp $ stats_fp $ msg_size $ batch_size)

let () =
	Random.self_init ();
	exit (Cmd.eval connect_cmd)
