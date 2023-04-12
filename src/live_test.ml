open Lwt.Syntax

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

let sent = ref 0

let gen_callback_id () =Random.bits64 ()

(* wait until we send a request to some node and it is commited *)
let rec await_first_commit i conn timeout =
	let tmp = Util.empty_stats (Base.Int63.zero) in (* these stats are discarded *)
	let* success = Net.send_req conn ({data = "init"; callback_id = (gen_callback_id ())} : Consensus.cmd) timeout tmp in
	if success then (
		Fmt.pr "connected to %d!@." i;
		Lwt.return_unit
	)
	else (
		await_first_commit i conn timeout
	)

(* wait until we send a request to each node and they are all commited *)
let await_connections conns timeout =
	List.mapi (fun i conn -> await_first_commit i conn timeout) conns
	|> Lwt.join

let run_command conns timeout stats msg_size =
	(* let id = !sent mod (List.length conns) in
	let conn = List.nth conns id in *)
	let data = Fmt.str "c%d" !sent in
	let callback_id = gen_callback_id () in
	(* simulate the size of a batch *)
	let data = if msg_size = 1 then
    	data
	else
    	List.fold_left (fun acc x -> acc ^ x) "" (List.init msg_size (fun _ -> data ^ (Int64.to_string callback_id)))
	in
	sent := !sent + 1;
	(* Net.send_req conn ({data = data; callback_id = callback_id} : Consensus.cmd) timeout stats *)
	let res = List.map (fun conn ->
		Net.send_req conn ({data = data; callback_id = callback_id} : Consensus.cmd) timeout stats
	) conns in
	Lwt.pick res

let benchmark conns res rate timeout msg_size =
	let t = Time_now.nanoseconds_since_unix_epoch () in
	let s = Util.empty_stats (Base.Int63.zero) in
	let period = Base.Int63.of_float((1. /. rate) *. 1_000_000_000.) in
	let rec aux (i : int) (target : Base.Int63.t) =
		if i >= (Array.length res) then
			Lwt.return_unit
		else (
			let now = Time_now.nanoseconds_since_unix_epoch () in
			let* () = if Base.Int63.(<) now target then
				Lwt_unix.sleep (Util.delta now target)
			else
				Lwt.return_unit
			in
			Lwt.async (fun () ->
				let x = Base.Int63.(-) (Time_now.nanoseconds_since_unix_epoch ()) t in
				let* success = run_command conns timeout s msg_size in
				let y = Base.Int63.(-) (Time_now.nanoseconds_since_unix_epoch ()) t in
				(* Fmt.pr "%d: success = %b at %s@." i success (to_string y); *)
				let ret = if success then Some (x, y) else None in
				Lwt.return (Array.set res i ret)
			);
			aux (i + 1) (Base.Int63.(+) target period)
		)
	in
	let* () = aux 0 t in
	Lwt.return s

(* calculate delta between timestamps in seconds s*)
let delta x y =
	let open Base.Int63 in
	(to_float (y - x)) /. 1_000_000_000.

let run_client nodes chained time rate req_times_fp stats_fp msg_size batch_size =
	Lwt_main.run begin
		let conns = Net.open_conns nodes in
		let promise, resolver = Lwt.wait () in
		let* () = await_connections conns 3. in
		Fmt.pr "connected to all!@.";
		Lwt.async (fun () ->
			let n = time * rate in (* calculate based on actual number sent (filter)*)
			let res = Array.make n None in
			let* stats = benchmark conns res (Float.of_int rate) 1000. msg_size in
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
			let goodput = (Float.of_int success) /. elapsed in
			let res = Array.map (function (Some (x, y)) -> delta x y | None -> 0.) res in
			(* let goodput = (Float.of_int n) /. elapsed in *)
			let sum = Array.fold_left (+.) 0. res in
			let sum_sq = Array.fold_left (fun acc x -> acc +. (x *. x)) 0. res in
			let mean = sum /. (Float.of_int n) in
			let sd = Float.sqrt ((sum_sq /. (Float.of_int n)) -. (mean *. mean)) in
			let chained = if chained then "y" else "n" in
			Fmt.pr "\nname = %s\nchained = %s\nnodes = %d \n reqs = %d / %d\nthroughput = %dreq/s\ngoodput = %freq/s\nmean = %fs\nsd = %fs@." name chained nodes success n rate goodput mean sd;
			Util.print_stats !(stats.connection_times) "req_conn" "s" 69;
			Util.print_stats !(stats.send_times) "req_send" "s" 69;
			(* append stats to csv file*)
			(match stats_fp with
				| Some s ->
					Out_channel.with_open_gen [Open_append] 0o666 s (fun oc ->
						Printf.fprintf oc "%s, %s, %d, %d, %f, %f, %f, %d, %d, %d, %d\n" name chained nodes rate goodput mean sd success n batch_size msg_size
					)
				| None -> ());
			Lwt.wakeup resolver ();
			Lwt.return_unit
		);
		let* () = Lwt.join [promise; Lwt_unix.sleep (Float.of_int time)] in (* wait until all requests send AND time complete*)
		let q = List.map (fun conn ->
			Net.send_quit conn
		) conns in
		let* () = Lwt.join q in
		Lwt.return ()
	end

open Cmdliner

let nodes =
	let doc = "Node count." in
	Arg.(required & pos 0 (some int) None & info [] ~docv:"NODES" ~doc)

let chained =
	let doc = "Whether to use chained version." in
	Arg.(value & flag & info ["c"; "chained"] ~docv:"CHAINED" ~doc)

let req_times_fp =
	let doc = "Filepath to append request times to." in
	Arg.(value & opt (some string) None & info ["times"] ~docv:"TIMES" ~doc)

let stats_fp =
	let doc = "Filepath to append stats to." in
	Arg.(value & opt (some string) None & info ["stats"] ~docv:"STATS" ~doc)

let time =
	let doc = "Time to run experiment for." in
	Arg.(value & opt int 100 & info ["t"; "time"] ~docv:"TIME" ~doc)

let throughput =
	let doc = "Rate at which to send requests." in
	Arg.(value & opt int 10 & info ["r"; "rate"] ~docv:"RATE" ~doc)

let msg_size =
	let doc = "Message size." in
	Arg.(value & opt int 1 & info ["s"; "size"] ~docv:"SIZE" ~doc)

let batch_size =
	let doc = "Batch size." in
	Arg.(value & opt int 1 & info ["b"; "batch"] ~docv:"BATCH_SIZE" ~doc)

let connect_cmd =
	let doc = "run the client" in
	let info = Cmd.info "connect" ~doc in
	Cmd.v info Term.(const run_client $ nodes $ chained $ time $ throughput $ req_times_fp $ stats_fp $ msg_size $ batch_size)

let () =
	Random.self_init ();
	(*Fmt_tty.setup_std_outputs ();
	Logs.set_reporter reporter;
	Logs.set_level ~all:true (Some Logs.Info);
	Logs.Src.list () |> List.iter (fun src ->
		if Astring.String.is_prefix ~affix:"capnp" (Logs.Src.name src) then
		Logs.Src.set_level src (Some Logs.Debug);
	);*)
	exit (Cmd.eval connect_cmd)
