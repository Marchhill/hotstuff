open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let run_client sr =
  Capability.with_ref sr (Echo.ping "hello"); return ()

let connect uri =
  Lwt_main.run begin
    let client_vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
    Capnp_rpc_unix.with_cap_exn sr run_client
  end

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  let doc = "run the client" in
  let info = Cmd.info "connect" ~doc in
  Cmd.v info Term.(const connect $ connect_addr)

let () =
  exit (Cmd.eval conn nect_cmd)