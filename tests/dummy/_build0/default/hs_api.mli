[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t

  module MsgType_13468819503706107811 : sig
    type t =
      | NewView
      | Prepare
      | PrepareAck
      | PreCommit
      | PreCommitAck
      | Commit
      | CommitAck
      | Decide
      | Generic
      | GenericAck
      | NextView
      | Complain
      | Undefined of int
  end

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module MsgType : sig
      type t = MsgType_13468819503706107811.t =
        | NewView
        | Prepare
        | PrepareAck
        | PreCommit
        | PreCommitAck
        | Commit
        | CommitAck
        | Decide
        | Generic
        | GenericAck
        | NextView
        | Complain
        | Undefined of int
    end
    module Cmd : sig
      type struct_t = [`Cmd_e60a35650e6db524]
      type t = struct_t reader_t
      val has_data : t -> bool
      val data_get : t -> string
      val id_get : t -> int32
      val id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Node : sig
      type struct_t = [`Node_ea189050e2754a42]
      type t = struct_t reader_t
      val has_cmd : t -> bool
      val cmd_get : t -> (ro, Cmd.t, array_t) Capnp.Array.t
      val cmd_get_list : t -> Cmd.t list
      val cmd_get_array : t -> Cmd.t array
      val has_justify : t -> bool
      val justify_get : t -> [`NodeJustify_c0fdcb72d34556b8] reader_t
      val justify_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`NodeJustify_c0fdcb72d34556b8] MessageWrapper.StructRef.t
      val height_get : t -> int32
      val height_get_int_exn : t -> int
      val has_digest : t -> bool
      val digest_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module QC : sig
      type struct_t = [`QC_aaf4fe9e0de4e4d5]
      type t = struct_t reader_t
      val msg_type_get : t -> MsgType.t
      val view_get : t -> int32
      val view_get_int_exn : t -> int
      val has_node : t -> bool
      val node_get : t -> (ro, Node.t, array_t) Capnp.Array.t
      val node_get_list : t -> Node.t list
      val node_get_array : t -> Node.t array
      val has_signature : t -> bool
      val signature_get : t -> string
      val has_ids : t -> bool
      val ids_get : t -> (ro, int32, array_t) Capnp.Array.t
      val ids_get_list : t -> int32 list
      val ids_get_array : t -> int32 array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module NodeJustify : sig
      type struct_t = [`NodeJustify_c0fdcb72d34556b8]
      type t = struct_t reader_t
      val msg_type_get : t -> MsgType.t
      val view_get : t -> int32
      val view_get_int_exn : t -> int
      val node_offset_get : t -> int32
      val node_offset_get_int_exn : t -> int
      val has_signature : t -> bool
      val signature_get : t -> string
      val has_ids : t -> bool
      val ids_get : t -> (ro, int32, array_t) Capnp.Array.t
      val ids_get_list : t -> int32 list
      val ids_get_array : t -> int32 array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Msg : sig
      type struct_t = [`Msg_986f7ea6fac7edc0]
      type t = struct_t reader_t
      val cur_view_get : t -> int32
      val cur_view_get_int_exn : t -> int
      val type_get : t -> MsgType.t
      val has_node : t -> bool
      val node_get : t -> (ro, Node.t, array_t) Capnp.Array.t
      val node_get_list : t -> Node.t list
      val node_get_array : t -> Node.t array
      val has_justify : t -> bool
      val justify_get : t -> QC.t
      val justify_get_pipelined : struct_t MessageWrapper.StructRef.t -> QC.struct_t MessageWrapper.StructRef.t
      val has_partial_signature : t -> bool
      val partial_signature_get : t -> string
      val id_get : t -> int32
      val id_get_int_exn : t -> int
      val has_tcp_lens : t -> bool
      val tcp_lens_get : t -> (ro, int32, array_t) Capnp.Array.t
      val tcp_lens_get_list : t -> int32 list
      val tcp_lens_get_array : t -> int32 array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Hs : sig
      type t = [`Hs_b5a89f8ead4514ca]
      module SendMsg : sig
        module Params : sig
          type struct_t = [`SendMsg_954d887a333f7dd6]
          type t = struct_t reader_t
          val has_msg : t -> bool
          val msg_get : t -> Msg.t
          val msg_get_pipelined : struct_t MessageWrapper.StructRef.t -> Msg.struct_t MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`SendMsg_bead28930d58d73b]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module ClientReq : sig
        module Params : sig
          type struct_t = [`ClientReq_96698cd49ba41a4b]
          type t = struct_t reader_t
          val has_cmd : t -> bool
          val cmd_get : t -> Cmd.t
          val cmd_get_pipelined : struct_t MessageWrapper.StructRef.t -> Cmd.struct_t MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`ClientReq_e5463cacc70303a2]
          type t = struct_t reader_t
          val success_get : t -> bool
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Quit : sig
        module Params : sig
          type struct_t = [`Quit_fa4433344eb9c67f]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Quit_b311cff704dcfeb8]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module MsgType : sig
      type t = MsgType_13468819503706107811.t =
        | NewView
        | Prepare
        | PrepareAck
        | PreCommit
        | PreCommitAck
        | Commit
        | CommitAck
        | Decide
        | Generic
        | GenericAck
        | NextView
        | Complain
        | Undefined of int
    end
    module Cmd : sig
      type struct_t = [`Cmd_e60a35650e6db524]
      type t = struct_t builder_t
      val has_data : t -> bool
      val data_get : t -> string
      val data_set : t -> string -> unit
      val id_get : t -> int32
      val id_get_int_exn : t -> int
      val id_set : t -> int32 -> unit
      val id_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Node : sig
      type struct_t = [`Node_ea189050e2754a42]
      type t = struct_t builder_t
      val has_cmd : t -> bool
      val cmd_get : t -> (rw, Cmd.t, array_t) Capnp.Array.t
      val cmd_get_list : t -> Cmd.t list
      val cmd_get_array : t -> Cmd.t array
      val cmd_set : t -> (rw, Cmd.t, array_t) Capnp.Array.t -> (rw, Cmd.t, array_t) Capnp.Array.t
      val cmd_set_list : t -> Cmd.t list -> (rw, Cmd.t, array_t) Capnp.Array.t
      val cmd_set_array : t -> Cmd.t array -> (rw, Cmd.t, array_t) Capnp.Array.t
      val cmd_init : t -> int -> (rw, Cmd.t, array_t) Capnp.Array.t
      val has_justify : t -> bool
      val justify_get : t -> [`NodeJustify_c0fdcb72d34556b8] builder_t
      val justify_set_reader : t -> [`NodeJustify_c0fdcb72d34556b8] reader_t -> [`NodeJustify_c0fdcb72d34556b8] builder_t
      val justify_set_builder : t -> [`NodeJustify_c0fdcb72d34556b8] builder_t -> [`NodeJustify_c0fdcb72d34556b8] builder_t
      val justify_init : t -> [`NodeJustify_c0fdcb72d34556b8] builder_t
      val height_get : t -> int32
      val height_get_int_exn : t -> int
      val height_set : t -> int32 -> unit
      val height_set_int_exn : t -> int -> unit
      val has_digest : t -> bool
      val digest_get : t -> string
      val digest_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module QC : sig
      type struct_t = [`QC_aaf4fe9e0de4e4d5]
      type t = struct_t builder_t
      val msg_type_get : t -> MsgType.t
      val msg_type_set : t -> MsgType.t -> unit
      val msg_type_set_unsafe : t -> MsgType.t -> unit
      val view_get : t -> int32
      val view_get_int_exn : t -> int
      val view_set : t -> int32 -> unit
      val view_set_int_exn : t -> int -> unit
      val has_node : t -> bool
      val node_get : t -> (rw, Node.t, array_t) Capnp.Array.t
      val node_get_list : t -> Node.t list
      val node_get_array : t -> Node.t array
      val node_set : t -> (rw, Node.t, array_t) Capnp.Array.t -> (rw, Node.t, array_t) Capnp.Array.t
      val node_set_list : t -> Node.t list -> (rw, Node.t, array_t) Capnp.Array.t
      val node_set_array : t -> Node.t array -> (rw, Node.t, array_t) Capnp.Array.t
      val node_init : t -> int -> (rw, Node.t, array_t) Capnp.Array.t
      val has_signature : t -> bool
      val signature_get : t -> string
      val signature_set : t -> string -> unit
      val has_ids : t -> bool
      val ids_get : t -> (rw, int32, array_t) Capnp.Array.t
      val ids_get_list : t -> int32 list
      val ids_get_array : t -> int32 array
      val ids_set : t -> (rw, int32, array_t) Capnp.Array.t -> (rw, int32, array_t) Capnp.Array.t
      val ids_set_list : t -> int32 list -> (rw, int32, array_t) Capnp.Array.t
      val ids_set_array : t -> int32 array -> (rw, int32, array_t) Capnp.Array.t
      val ids_init : t -> int -> (rw, int32, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module NodeJustify : sig
      type struct_t = [`NodeJustify_c0fdcb72d34556b8]
      type t = struct_t builder_t
      val msg_type_get : t -> MsgType.t
      val msg_type_set : t -> MsgType.t -> unit
      val msg_type_set_unsafe : t -> MsgType.t -> unit
      val view_get : t -> int32
      val view_get_int_exn : t -> int
      val view_set : t -> int32 -> unit
      val view_set_int_exn : t -> int -> unit
      val node_offset_get : t -> int32
      val node_offset_get_int_exn : t -> int
      val node_offset_set : t -> int32 -> unit
      val node_offset_set_int_exn : t -> int -> unit
      val has_signature : t -> bool
      val signature_get : t -> string
      val signature_set : t -> string -> unit
      val has_ids : t -> bool
      val ids_get : t -> (rw, int32, array_t) Capnp.Array.t
      val ids_get_list : t -> int32 list
      val ids_get_array : t -> int32 array
      val ids_set : t -> (rw, int32, array_t) Capnp.Array.t -> (rw, int32, array_t) Capnp.Array.t
      val ids_set_list : t -> int32 list -> (rw, int32, array_t) Capnp.Array.t
      val ids_set_array : t -> int32 array -> (rw, int32, array_t) Capnp.Array.t
      val ids_init : t -> int -> (rw, int32, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Msg : sig
      type struct_t = [`Msg_986f7ea6fac7edc0]
      type t = struct_t builder_t
      val cur_view_get : t -> int32
      val cur_view_get_int_exn : t -> int
      val cur_view_set : t -> int32 -> unit
      val cur_view_set_int_exn : t -> int -> unit
      val type_get : t -> MsgType.t
      val type_set : t -> MsgType.t -> unit
      val type_set_unsafe : t -> MsgType.t -> unit
      val has_node : t -> bool
      val node_get : t -> (rw, Node.t, array_t) Capnp.Array.t
      val node_get_list : t -> Node.t list
      val node_get_array : t -> Node.t array
      val node_set : t -> (rw, Node.t, array_t) Capnp.Array.t -> (rw, Node.t, array_t) Capnp.Array.t
      val node_set_list : t -> Node.t list -> (rw, Node.t, array_t) Capnp.Array.t
      val node_set_array : t -> Node.t array -> (rw, Node.t, array_t) Capnp.Array.t
      val node_init : t -> int -> (rw, Node.t, array_t) Capnp.Array.t
      val has_justify : t -> bool
      val justify_get : t -> QC.t
      val justify_set_reader : t -> QC.struct_t reader_t -> QC.t
      val justify_set_builder : t -> QC.t -> QC.t
      val justify_init : t -> QC.t
      val has_partial_signature : t -> bool
      val partial_signature_get : t -> string
      val partial_signature_set : t -> string -> unit
      val id_get : t -> int32
      val id_get_int_exn : t -> int
      val id_set : t -> int32 -> unit
      val id_set_int_exn : t -> int -> unit
      val has_tcp_lens : t -> bool
      val tcp_lens_get : t -> (rw, int32, array_t) Capnp.Array.t
      val tcp_lens_get_list : t -> int32 list
      val tcp_lens_get_array : t -> int32 array
      val tcp_lens_set : t -> (rw, int32, array_t) Capnp.Array.t -> (rw, int32, array_t) Capnp.Array.t
      val tcp_lens_set_list : t -> int32 list -> (rw, int32, array_t) Capnp.Array.t
      val tcp_lens_set_array : t -> int32 array -> (rw, int32, array_t) Capnp.Array.t
      val tcp_lens_init : t -> int -> (rw, int32, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Hs : sig
      type t = [`Hs_b5a89f8ead4514ca]
      module SendMsg : sig
        module Params : sig
          type struct_t = [`SendMsg_954d887a333f7dd6]
          type t = struct_t builder_t
          val has_msg : t -> bool
          val msg_get : t -> Msg.t
          val msg_set_reader : t -> Msg.struct_t reader_t -> Msg.t
          val msg_set_builder : t -> Msg.t -> Msg.t
          val msg_init : t -> Msg.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`SendMsg_bead28930d58d73b]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module ClientReq : sig
        module Params : sig
          type struct_t = [`ClientReq_96698cd49ba41a4b]
          type t = struct_t builder_t
          val has_cmd : t -> bool
          val cmd_get : t -> Cmd.t
          val cmd_set_reader : t -> Cmd.struct_t reader_t -> Cmd.t
          val cmd_set_builder : t -> Cmd.t -> Cmd.t
          val cmd_init : t -> Cmd.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`ClientReq_e5463cacc70303a2]
          type t = struct_t builder_t
          val success_get : t -> bool
          val success_set : t -> bool -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Quit : sig
        module Params : sig
          type struct_t = [`Quit_fa4433344eb9c67f]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Quit_b311cff704dcfeb8]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
    module Hs : sig
      type t = [`Hs_b5a89f8ead4514ca]
      val interface_id : Stdint.Uint64.t
      module SendMsg : sig
        module Params = Builder.Hs.SendMsg.Params
        module Results = Reader.Hs.SendMsg.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module ClientReq : sig
        module Params = Builder.Hs.ClientReq.Params
        module Results = Reader.Hs.ClientReq.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Quit : sig
        module Params = Builder.Hs.Quit.Params
        module Results = Reader.Hs.Quit.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
  end

  module Service : sig
    module Hs : sig
      type t = [`Hs_b5a89f8ead4514ca]
      val interface_id : Stdint.Uint64.t
      module SendMsg : sig
        module Params = Reader.Hs.SendMsg.Params
        module Results = Builder.Hs.SendMsg.Results
      end
      module ClientReq : sig
        module Params = Reader.Hs.ClientReq.Params
        module Results = Builder.Hs.ClientReq.Results
      end
      module Quit : sig
        module Params = Reader.Hs.Quit.Params
        module Results = Builder.Hs.Quit.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual send_msg_impl : (SendMsg.Params.t, SendMsg.Results.t) MessageWrapper.Service.method_t
        method virtual client_req_impl : (ClientReq.Params.t, ClientReq.Results.t) MessageWrapper.Service.method_t
        method virtual quit_impl : (Quit.Params.t, Quit.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
