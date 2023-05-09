module Api : module type of Hs_api.MakeRPC(Capnp_rpc_lwt)

val cmd_to_api_cmd : Api.Builder.Cmd.t -> Consensus.cmd -> unit
val api_cmd_to_cmd : Api.Reader.Cmd.t -> Consensus.cmd
val msg_to_api_msg : Api.Builder.Msg.t -> Consensus.msg -> unit
val api_msg_to_consensus_event : Api.Reader.Msg.t -> Consensus.event