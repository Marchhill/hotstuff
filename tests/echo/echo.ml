open Core
open Async

(* Copy data from the reader to the writer, using the provided buffer
	as scratch space *)

(*
let rec copy_blocks buffer r w =
	match%bind Reader.read r buffer with
		| `Eof -> return ()
		| `Ok bytesRead ->
			let () = Writer.write_bytes w buffer ~len:bytesRead in
			let%bind () = Writer.flushed w in
			copy_blocks buffer r w
*)
	
(** Starts a TCP server, which listens on the specified port, invoking
    copy_blocks every time a client connects. *)
let run ~uppercase ~port =
	let host_and_port =
		Tcp.Server.create
			~on_handler_error:`Raise
			(Tcp.Where_to_listen.of_port port)
			(fun _addr r w ->
				Pipe.transfer
					(Reader.pipe r)
					(Writer.pipe w)
					~f:(if uppercase then String.uppercase else Fn.id))
	in
	ignore (host_and_port : ((Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t));
	Deferred.never ()

let () =
	Command.async ~summary:"server that returns bytes sent to it"
		(let%map_open.Command
			uppercase = flag "-uppercase" no_arg ~doc:"convert to uppercase" and
			port = flag "-port" (optional_with_default 9000 int) ~doc:"port to listen on"
	in
	fun () -> run ~uppercase ~port)
	|> Command_unix.run