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

let run_command conns timeout _i =
	(* let id = !sent mod (List.length conns) in *)
	(* let conn = List.nth conns id in *)
	let data = Fmt.str "cmdcmdcmdcmdcmdcmdcmdcmdcmdcmdmcmdmcdmcmdmcmdcmdm#%d" !sent in
	(* let data = "c" in *)
	sent := !sent + 1;
	let callback_id = Fmt.str "%x" (Int64.to_int (Random.bits64 ())) in
	(* let callback_id = "a" in *)
	(* Fmt.pr "%d: sending \"%s\"@." _i data; *)
	let res = List.mapi (fun _i conn ->
		Net.send_req conn ({data = data; callback_id = callback_id} : Consensus.cmd) timeout
	) conns in
	Lwt.pick res
	(* Hs.send_req conn ({data = data; callback_id = callback_id} : Consensus.cmd) timeout *)

let benchmark conns res rate timeout =
	let open Base.Int63 in
	let tmp = List.init (Array.length res) (fun _ -> ()) in
	let t = Time_now.nanoseconds_since_unix_epoch () in
	Lwt_list.iteri_p (fun i () ->
			let* () = Lwt_unix.sleep ((Float.of_int i) /. rate) in
			let x = Time_now.nanoseconds_since_unix_epoch () - t in
			let* success = run_command conns timeout i in
			let y = Time_now.nanoseconds_since_unix_epoch () - t in
			(* Fmt.pr "%d: success = %b at %s@." i success (to_string y); *)
			let ret = if success then Some (x, y) else None in
			Lwt.return (Array.set res i ret)
		) tmp

(* calculate delta between timestamps in seconds s*)
let delta x y =
	let open Base.Int63 in
	(to_float (y - x)) /. 1_000_000_000.

let run_client nodes chained time rate req_times_fp stats_fp =
	Lwt_main.run begin
		let conns = Net.open_conns nodes in
		let promise, resolver = Lwt.wait () in
		Lwt.async (fun () ->
			let n = time * rate in
			let res = Array.make n None in
			let* () = benchmark conns res (Float.of_int rate) 1000. in
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
			let goodput = (Float.of_int n) /. elapsed in
			let res = Array.map (function (Some (x, y)) -> delta x y | None -> 0.) res in
			(* let goodput = (Float.of_int n) /. elapsed in *)
			let sum = Array.fold_left (+.) 0. res in
			let sum_sq = Array.fold_left (fun acc x -> acc +. (x *. x)) 0. res in
			let mean = sum /. (Float.of_int n) in
			let sd = Float.sqrt ((sum_sq /. (Float.of_int n)) -. (mean *. mean)) in
			let chained = if chained then "y" else "n" in
			Fmt.pr "\nname = %s\nchained = %s\nnodes = %d \n reqs = %d / %d\nthroughput = %dreq/s\ngoodput = %freq/s\nmean = %fs\nsd = %fs@." name chained nodes success n rate goodput mean sd;
			(* append stats to csv file*)
			(match stats_fp with
				| Some s ->
					Out_channel.with_open_gen [Open_append] 0o666 s (fun oc ->
						Printf.fprintf oc "%s, %s, %d, %d, %f, %f, %f\n" name chained nodes rate goodput mean sd
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
	let doc = "Time to reun experiment for." in
	Arg.(value & opt int 100 & info ["t"; "time"] ~docv:"TIME" ~doc)

let throughput =
	let doc = "Rate at which to send requests." in
	Arg.(value & opt int 10 & info ["r"; "rate"] ~docv:"RATE" ~doc)

let connect_cmd =
	let doc = "run the client" in
	let info = Cmd.info "connect" ~doc in
	Cmd.v info Term.(const run_client $ nodes $ chained $ time $ throughput $ req_times_fp $ stats_fp)

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
