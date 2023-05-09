open Lwt.Syntax
open Types

let empty_stats t = {
	last_view = ref (t);
	view_times = ref [];
	action_times = ref [];
	advance_times = ref [];
	connection_times = ref [];
	send_times = ref [];
	sizes = ref [];
	recv_msg_times = ref [];
	recv_req_times = ref [];
	res_times = ref [];
	req_queue_times = ref [];
	msg_queue_times = ref [];
}

let print_stats l name units id =
	let n = List.length l in
	let sum = List.fold_left (+.) 0. l in
	let sum_sq = List.fold_left (fun acc x -> acc +. (x *. x)) 0. l in
	let mean = sum /. (Float.of_int n) in
	let sd = Float.sqrt ((sum_sq /. (Float.of_int n)) -. (mean *. mean)) in
	Fmt.pr "%d: %s mean = %f%s, sd = %f%s, count = %d, sum = %f@." id name mean units sd units n sum
	(* Fmt.pr "%d: %s : %s@." id name (List.fold_left (fun acc x -> acc ^ ", " ^ (Float.to_string x)) "" l) *)

(* calculate delta between timestamps in seconds s*)
let delta x y =
	let open Base.Int63 in
	(to_float (y - x)) /. 1_000_000_000.

let on_timeout t f x =
	let promise, resolver = Lwt.task () in
	Lwt.async (fun () ->
		let* () = Lwt_unix.sleep t in
		(match Lwt.state promise with
			| Lwt.Return _ -> ()
			| Lwt.Fail _ -> ()
			| Lwt.Sleep -> Lwt.wakeup resolver true);
		Lwt.return ()
	);
	Lwt.async (fun () ->
		let* s = promise in
		if s then f x else ();
		Lwt.return ()
	);
	(promise, resolver)

let cancel_timeout t =
	match Lwt.state (fst t) with
			| Lwt.Return _ -> ()
			| Lwt.Fail _ -> ()
			| Lwt.Sleep -> Lwt.wakeup (snd t) false

let create_timer t =
	let timer = ref (on_timeout t Fun.id ()) in
	(fun f x ->
		cancel_timeout !timer;
		timer := on_timeout t f x	
	)

(* hardcode same pk for all nodes to avoid pki! *)
let gen_key _ =
	let sk = Tezos_crypto.Aggregate_signature.Secret_key.of_b58check_exn "BLsk2bwzMPpXwy9hoMuV7MP6muK8NLKMWRVUNAaMs7ZDJwahUhtadY" in
	let pk = Tezos_crypto.Aggregate_signature.Secret_key.to_public_key sk in
	let pkh = Tezos_crypto.Aggregate_signature.Public_key.hash pk in
	(pkh, pk, sk)

(* get our secret key and a list of public keys for all nodes *)
let gen_keys id nodes =
	let keys = List.init nodes gen_key in
	let _, _, sk = List.nth keys id in
	let pks = List.map (fun (_, x, _) -> x) keys in
	(sk, pks)

(* Verbose Cap'n Proto logging *)

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

let init_logging () =
	Fmt_tty.setup_std_outputs ();
	Logs.set_reporter reporter;
	Logs.set_level ~all:true (Some Logs.Info);
	Logs.Src.list () |> List.iter (fun src ->
		if Astring.String.is_prefix ~affix:"capnp" (Logs.Src.name src) then
		Logs.Src.set_level src (Some Logs.Debug);
	);