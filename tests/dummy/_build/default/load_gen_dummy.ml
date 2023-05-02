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
		let tmp = Util.empty_stats (Base.Int63.zero) in (* these stats are discarded *)
		let* success = Net.send_req conn ({data = "init"; callback_id = (gen_callback_id ())} : Consensus.cmd) timeout tmp in
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
		Net.send_req conn ({data = data; callback_id = callback_id} : Consensus.cmd) timeout stats
	) conns in
	Lwt.pick res

let benchmark conns time timeout msg_size start_time =
	let s = Util.empty_stats (Base.Int63.zero) in
	let end_time = Base.Int63.(+) start_time (Base.Int63.of_int (time * 1_000_000_000)) in
	let rec aux (i : int) =
		let now = Time_now.nanoseconds_since_unix_epoch () in
		if Base.Int63.(>) now end_time then
			Lwt.return i
		else (
			(* sleep if not yet time to deliver next command *)
			let* () = if (i mod 10) = 0 then (
				if (i mod 100) = 0 then
					Fmt.pr "sent %d!@." i;
				Lwt.pause ()
			)
			else
				Lwt.return_unit
			in
			Lwt.async (fun () -> let* _ = run_command conns timeout s msg_size in Lwt.return_unit);
			aux (i + 1)
		)
	in
	let* i = aux 0 in
	Lwt.return (i, s)

let run_client nodes _version time _rate req_times_fp stats_fp msg_size _batch_size =
	Lwt_main.run begin
		let conns = Net.open_conns nodes in
		let* connected = await_connections conns 5. 10 in
		if not connected then (
			Fmt.epr "abort!@.";
			exit 1;
		);
		Fmt.pr "connected to all!@.";
		(* run and ignore results for 3s to allow batches to fill up *)
		(* begin actual benchamrking*)
		let start_time = Time_now.nanoseconds_since_unix_epoch () in
		let* (i, stats) = benchmark conns time 1000. msg_size start_time in
		let name = match req_times_fp with
			| Some s ->
				Filename.basename s |> Filename.remove_extension
			| None -> ""
		in
		(* output request times to csv file*)
		let goodput = ((Float.of_int i) /. (Float.of_int time)) *. (Float.of_int msg_size) in
		let n = List.length !(stats.send_times) in
		let sum = List.fold_left (+.) 0. !(stats.send_times) in
		let mean = sum /. (Float.of_int n) in
		Fmt.pr "%s: n = %d, goodput = %fbytes/s@." name i goodput;
		Util.print_stats !(stats.connection_times) "req_conn" "s" 12345;
		Util.print_stats !(stats.send_times) "req_send" "s" 12345;
		(* let success = Array.fold_left (fun acc -> function Some _ -> acc + 1 | None -> acc) 0  res in
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
		Util.print_stats !(stats.connection_times) "req_conn" "s" 12345;
		Util.print_stats !(stats.send_times) "req_send" "s" 12345;
		*)
		(* append stats to csv file*)
		(match stats_fp with
			| Some s ->
				Out_channel.with_open_gen [Open_append] 0o666 s (fun oc ->
					Printf.fprintf oc "%s, %f, %d, %f, %f\n" name goodput msg_size mean sum
				)
			| None -> ());
		let _ = List.map (fun conn ->
			Lwt.async (fun () -> Net.send_quit conn)
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
