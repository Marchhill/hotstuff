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

(*
let sizeof v =
	let rec rec_size d r =
	  if List.memq r d then (1, d) else
	  if not(Obj.is_block r) then (1, r::d) else
	  if (Obj.tag r) = (Obj.double_tag) then (2, r::d) else
	  if (Obj.tag r) = (Obj.string_tag) then (Obj.size r, r::d) else
	  if (Obj.tag r) = (Obj.object_tag) ||
		 (Obj.tag r) = (Obj.closure_tag)
	  then invalid_arg "please only provide datas"
	  else
		let len = Obj.size r in
		let rec aux d sum i =
		  if i >= len then (sum, r::d) else
		  let this = Obj.field r i in
		  let this_size, d = rec_size d this in
		  aux d (sum + this_size) (i+1)
		in
		aux d (1) 0
	in
	fst(rec_size [] (Obj.repr v))
*)

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