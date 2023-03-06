open Core
open Async

let dispatch_req conn req = Rpc.Rpc.dispatch_exn Intf.echo_api conn req

let connect address =
  print_endline @@ Fmt.str "Connecting to %s" (Host_and_port.to_string address); 
  Tcp.with_connection
        (Tcp.Where_to_connect.of_host_and_port
           address)
        ~timeout:(sec 1.)
        (fun _ r w -> 
           match%bind Rpc.Connection.create r w ~connection_state:(fun _ -> ()) with
           | Error exn -> raise exn
           | Ok conn -> return conn)

let client ~port =
  print_endline "Starting client";
  let address = Host_and_port.create ~host:"localhost" ~port in
  let%bind conn = connect address in
  let request_string = "ccc" in
  printf "got here";
  let%bind res = (dispatch_req conn request_string) in
  print_endline res;
  Deferred.unit

let () =
  Command.async_spec
    ~summary:"A trivial Async-RPC server"
    Command.Spec.(
      empty
      +> flag "-port" ~doc:" Port to send request to" (optional_with_default 8080 int)
    )
    (fun port () -> 
         client ~port
    )
  |> Command_unix.run