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

module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  include Capnp.Runtime.BuilderInc.Make[@inlined](MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module MsgType_13468819503706107811 = struct
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
    let decode u16 = match u16 with
      | 0 -> NewView
      | 1 -> Prepare
      | 2 -> PrepareAck
      | 3 -> PreCommit
      | 4 -> PreCommitAck
      | 5 -> Commit
      | 6 -> CommitAck
      | 7 -> Decide
      | 8 -> Generic
      | 9 -> GenericAck
      | 10 -> NextView
      | 11 -> Complain
      | v -> Undefined v
    let encode_safe enum = match enum with
      | NewView -> 0
      | Prepare -> 1
      | PrepareAck -> 2
      | PreCommit -> 3
      | PreCommitAck -> 4
      | Commit -> 5
      | CommitAck -> 6
      | Decide -> 7
      | Generic -> 8
      | GenericAck -> 9
      | NextView -> 10
      | Complain -> 11
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | NewView -> 0
      | Prepare -> 1
      | PrepareAck -> 2
      | PreCommit -> 3
      | PreCommitAck -> 4
      | Commit -> 5
      | CommitAck -> 6
      | Decide -> 7
      | Generic -> 8
      | GenericAck -> 9
      | NextView -> 10
      | Complain -> 11
      | Undefined x -> x
  end
  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option
    let of_pointer = RA_.deref_opt_struct_pointer

    module MsgType = struct
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
    module Cmd = struct
      type struct_t = [`Cmd_e60a35650e6db524]
      type t = struct_t reader_t
      let has_data x =
        RA_.has_field x 0
      let data_get x =
        RA_.get_blob ~default:"" x 0
      let id_get x =
        RA_.get_int32 ~default:(0l) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Node = struct
      type struct_t = [`Node_ea189050e2754a42]
      type t = struct_t reader_t
      let has_cmd x =
        RA_.has_field x 0
      let cmd_get x = 
        RA_.get_struct_list x 0
      let cmd_get_list x =
        Capnp.Array.to_list (cmd_get x)
      let cmd_get_array x =
        Capnp.Array.to_array (cmd_get x)
      let has_justify x =
        RA_.has_field x 1
      let justify_get x =
        RA_.get_struct x 1
      let justify_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let height_get x =
        RA_.get_int32 ~default:(0l) x 0
      let height_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (height_get x)
      let has_digest x =
        RA_.has_field x 2
      let digest_get x =
        RA_.get_blob ~default:"" x 2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module QC = struct
      type struct_t = [`QC_aaf4fe9e0de4e4d5]
      type t = struct_t reader_t
      let msg_type_get x =
        let discr = RA_.get_uint16 ~default:0 x 0 in
        MsgType_13468819503706107811.decode discr
      let view_get x =
        RA_.get_int32 ~default:(0l) x 4
      let view_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (view_get x)
      let has_node x =
        RA_.has_field x 0
      let node_get x = 
        RA_.get_struct_list x 0
      let node_get_list x =
        Capnp.Array.to_list (node_get x)
      let node_get_array x =
        Capnp.Array.to_array (node_get x)
      let has_signature x =
        RA_.has_field x 1
      let signature_get x =
        RA_.get_blob ~default:"" x 1
      let has_ids x =
        RA_.has_field x 2
      let ids_get x =
        RA_.get_int32_list x 2
      let ids_get_list x =
        Capnp.Array.to_list (ids_get x)
      let ids_get_array x =
        Capnp.Array.to_array (ids_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module NodeJustify = struct
      type struct_t = [`NodeJustify_c0fdcb72d34556b8]
      type t = struct_t reader_t
      let msg_type_get x =
        let discr = RA_.get_uint16 ~default:0 x 0 in
        MsgType_13468819503706107811.decode discr
      let view_get x =
        RA_.get_int32 ~default:(0l) x 4
      let view_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (view_get x)
      let node_offset_get x =
        RA_.get_int32 ~default:(0l) x 8
      let node_offset_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (node_offset_get x)
      let has_signature x =
        RA_.has_field x 0
      let signature_get x =
        RA_.get_blob ~default:"" x 0
      let has_ids x =
        RA_.has_field x 1
      let ids_get x =
        RA_.get_int32_list x 1
      let ids_get_list x =
        Capnp.Array.to_list (ids_get x)
      let ids_get_array x =
        Capnp.Array.to_array (ids_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Msg = struct
      type struct_t = [`Msg_986f7ea6fac7edc0]
      type t = struct_t reader_t
      let cur_view_get x =
        RA_.get_int32 ~default:(0l) x 0
      let cur_view_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (cur_view_get x)
      let type_get x =
        let discr = RA_.get_uint16 ~default:0 x 4 in
        MsgType_13468819503706107811.decode discr
      let has_node x =
        RA_.has_field x 0
      let node_get x = 
        RA_.get_struct_list x 0
      let node_get_list x =
        Capnp.Array.to_list (node_get x)
      let node_get_array x =
        Capnp.Array.to_array (node_get x)
      let has_justify x =
        RA_.has_field x 1
      let justify_get x =
        RA_.get_struct x 1
      let justify_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_partial_signature x =
        RA_.has_field x 2
      let partial_signature_get x =
        RA_.get_blob ~default:"" x 2
      let id_get x =
        RA_.get_int32 ~default:(0l) x 8
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (id_get x)
      let has_tcp_lens x =
        RA_.has_field x 3
      let tcp_lens_get x =
        RA_.get_int32_list x 3
      let tcp_lens_get_list x =
        Capnp.Array.to_list (tcp_lens_get x)
      let tcp_lens_get_array x =
        Capnp.Array.to_array (tcp_lens_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Hs = struct
      type t = [`Hs_b5a89f8ead4514ca]
      module SendMsg = struct
        module Params = struct
          type struct_t = [`SendMsg_954d887a333f7dd6]
          type t = struct_t reader_t
          let has_msg x =
            RA_.has_field x 0
          let msg_get x =
            RA_.get_struct x 0
          let msg_get_pipelined x =
            MessageWrapper.Untyped.struct_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`SendMsg_bead28930d58d73b]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module ClientReq = struct
        module Params = struct
          type struct_t = [`ClientReq_96698cd49ba41a4b]
          type t = struct_t reader_t
          let has_cmd x =
            RA_.has_field x 0
          let cmd_get x =
            RA_.get_struct x 0
          let cmd_get_pipelined x =
            MessageWrapper.Untyped.struct_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`ClientReq_e5463cacc70303a2]
          type t = struct_t reader_t
          let success_get x =
            RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Quit = struct
        module Params = struct
          type struct_t = [`Quit_fa4433344eb9c67f]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Quit_b311cff704dcfeb8]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module MsgType = struct
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
    module Cmd = struct
      type struct_t = [`Cmd_e60a35650e6db524]
      type t = struct_t builder_t
      let has_data x =
        BA_.has_field x 0
      let data_get x =
        BA_.get_blob ~default:"" x 0
      let data_set x v =
        BA_.set_blob x 0 v
      let id_get x =
        BA_.get_int32 ~default:(0l) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (id_get x)
      let id_set x v =
        BA_.set_int32 ~default:(0l) x 0 v
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Node = struct
      type struct_t = [`Node_ea189050e2754a42]
      type t = struct_t builder_t
      let has_cmd x =
        BA_.has_field x 0
      let cmd_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 0
      let cmd_get_list x =
        Capnp.Array.to_list (cmd_get x)
      let cmd_get_array x =
        Capnp.Array.to_array (cmd_get x)
      let cmd_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 0 v
      let cmd_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 0 n
      let cmd_set_list x v =
        let builder = cmd_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let cmd_set_array x v =
        let builder = cmd_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_justify x =
        BA_.has_field x 1
      let justify_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:2 x 1
      let justify_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:2 x 1 v
      let justify_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:2 x 1 (Some v)
      let justify_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:2 x 1
      let height_get x =
        BA_.get_int32 ~default:(0l) x 0
      let height_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (height_get x)
      let height_set x v =
        BA_.set_int32 ~default:(0l) x 0 v
      let height_set_int_exn x v = height_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let has_digest x =
        BA_.has_field x 2
      let digest_get x =
        BA_.get_blob ~default:"" x 2
      let digest_set x v =
        BA_.set_blob x 2 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:3
    end
    module QC = struct
      type struct_t = [`QC_aaf4fe9e0de4e4d5]
      type t = struct_t builder_t
      let msg_type_get x =
        let discr = BA_.get_uint16 ~default:0 x 0 in
        MsgType_13468819503706107811.decode discr
      let msg_type_set x e =
        BA_.set_uint16 ~default:0 x 0 (MsgType_13468819503706107811.encode_safe e)
      let msg_type_set_unsafe x e =
        BA_.set_uint16 ~default:0 x 0 (MsgType_13468819503706107811.encode_unsafe e)
      let view_get x =
        BA_.get_int32 ~default:(0l) x 4
      let view_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (view_get x)
      let view_set x v =
        BA_.set_int32 ~default:(0l) x 4 v
      let view_set_int_exn x v = view_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let has_node x =
        BA_.has_field x 0
      let node_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let node_get_list x =
        Capnp.Array.to_list (node_get x)
      let node_get_array x =
        Capnp.Array.to_array (node_get x)
      let node_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let node_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let node_set_list x v =
        let builder = node_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let node_set_array x v =
        let builder = node_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_signature x =
        BA_.has_field x 1
      let signature_get x =
        BA_.get_blob ~default:"" x 1
      let signature_set x v =
        BA_.set_blob x 1 v
      let has_ids x =
        BA_.has_field x 2
      let ids_get x =
        BA_.get_int32_list x 2
      let ids_get_list x =
        Capnp.Array.to_list (ids_get x)
      let ids_get_array x =
        Capnp.Array.to_array (ids_get x)
      let ids_set x v =
        BA_.set_int32_list x 2 v
      let ids_init x n =
        BA_.init_int32_list x 2 n
      let ids_set_list x v =
        let builder = ids_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let ids_set_array x v =
        let builder = ids_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:3
    end
    module NodeJustify = struct
      type struct_t = [`NodeJustify_c0fdcb72d34556b8]
      type t = struct_t builder_t
      let msg_type_get x =
        let discr = BA_.get_uint16 ~default:0 x 0 in
        MsgType_13468819503706107811.decode discr
      let msg_type_set x e =
        BA_.set_uint16 ~default:0 x 0 (MsgType_13468819503706107811.encode_safe e)
      let msg_type_set_unsafe x e =
        BA_.set_uint16 ~default:0 x 0 (MsgType_13468819503706107811.encode_unsafe e)
      let view_get x =
        BA_.get_int32 ~default:(0l) x 4
      let view_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (view_get x)
      let view_set x v =
        BA_.set_int32 ~default:(0l) x 4 v
      let view_set_int_exn x v = view_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let node_offset_get x =
        BA_.get_int32 ~default:(0l) x 8
      let node_offset_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (node_offset_get x)
      let node_offset_set x v =
        BA_.set_int32 ~default:(0l) x 8 v
      let node_offset_set_int_exn x v = node_offset_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let has_signature x =
        BA_.has_field x 0
      let signature_get x =
        BA_.get_blob ~default:"" x 0
      let signature_set x v =
        BA_.set_blob x 0 v
      let has_ids x =
        BA_.has_field x 1
      let ids_get x =
        BA_.get_int32_list x 1
      let ids_get_list x =
        Capnp.Array.to_list (ids_get x)
      let ids_get_array x =
        Capnp.Array.to_array (ids_get x)
      let ids_set x v =
        BA_.set_int32_list x 1 v
      let ids_init x n =
        BA_.init_int32_list x 1 n
      let ids_set_list x v =
        let builder = ids_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let ids_set_array x v =
        let builder = ids_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:2
    end
    module Msg = struct
      type struct_t = [`Msg_986f7ea6fac7edc0]
      type t = struct_t builder_t
      let cur_view_get x =
        BA_.get_int32 ~default:(0l) x 0
      let cur_view_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (cur_view_get x)
      let cur_view_set x v =
        BA_.set_int32 ~default:(0l) x 0 v
      let cur_view_set_int_exn x v = cur_view_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let type_get x =
        let discr = BA_.get_uint16 ~default:0 x 4 in
        MsgType_13468819503706107811.decode discr
      let type_set x e =
        BA_.set_uint16 ~default:0 x 4 (MsgType_13468819503706107811.encode_safe e)
      let type_set_unsafe x e =
        BA_.set_uint16 ~default:0 x 4 (MsgType_13468819503706107811.encode_unsafe e)
      let has_node x =
        BA_.has_field x 0
      let node_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let node_get_list x =
        Capnp.Array.to_list (node_get x)
      let node_get_array x =
        Capnp.Array.to_array (node_get x)
      let node_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let node_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let node_set_list x v =
        let builder = node_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let node_set_array x v =
        let builder = node_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_justify x =
        BA_.has_field x 1
      let justify_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:3 x 1
      let justify_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:3 x 1 v
      let justify_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:3 x 1 (Some v)
      let justify_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:3 x 1
      let has_partial_signature x =
        BA_.has_field x 2
      let partial_signature_get x =
        BA_.get_blob ~default:"" x 2
      let partial_signature_set x v =
        BA_.set_blob x 2 v
      let id_get x =
        BA_.get_int32 ~default:(0l) x 8
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (id_get x)
      let id_set x v =
        BA_.set_int32 ~default:(0l) x 8 v
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let has_tcp_lens x =
        BA_.has_field x 3
      let tcp_lens_get x =
        BA_.get_int32_list x 3
      let tcp_lens_get_list x =
        Capnp.Array.to_list (tcp_lens_get x)
      let tcp_lens_get_array x =
        Capnp.Array.to_array (tcp_lens_get x)
      let tcp_lens_set x v =
        BA_.set_int32_list x 3 v
      let tcp_lens_init x n =
        BA_.init_int32_list x 3 n
      let tcp_lens_set_list x v =
        let builder = tcp_lens_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let tcp_lens_set_array x v =
        let builder = tcp_lens_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:4 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:4 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:4
    end
    module Hs = struct
      type t = [`Hs_b5a89f8ead4514ca]
      module SendMsg = struct
        module Params = struct
          type struct_t = [`SendMsg_954d887a333f7dd6]
          type t = struct_t builder_t
          let has_msg x =
            BA_.has_field x 0
          let msg_get x =
            BA_.get_struct ~data_words:2 ~pointer_words:4 x 0
          let msg_set_reader x v =
            BA_.set_struct ~data_words:2 ~pointer_words:4 x 0 v
          let msg_set_builder x v =
            BA_.set_struct ~data_words:2 ~pointer_words:4 x 0 (Some v)
          let msg_init x =
            BA_.init_struct ~data_words:2 ~pointer_words:4 x 0
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`SendMsg_bead28930d58d73b]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
      module ClientReq = struct
        module Params = struct
          type struct_t = [`ClientReq_96698cd49ba41a4b]
          type t = struct_t builder_t
          let has_cmd x =
            BA_.has_field x 0
          let cmd_get x =
            BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
          let cmd_set_reader x v =
            BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
          let cmd_set_builder x v =
            BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
          let cmd_init x =
            BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`ClientReq_e5463cacc70303a2]
          type t = struct_t builder_t
          let success_get x =
            BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let success_set x v =
            BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
      end
      module Quit = struct
        module Params = struct
          type struct_t = [`Quit_fa4433344eb9c67f]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Quit_b311cff704dcfeb8]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
  end

  module Client = struct
    module Hs = struct
      type t = [`Hs_b5a89f8ead4514ca]
      let interface_id = Stdint.Uint64.of_string "0xb5a89f8ead4514ca"
      module SendMsg = struct
        module Params = Builder.Hs.SendMsg.Params
        module Results = Reader.Hs.SendMsg.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module ClientReq = struct
        module Params = Builder.Hs.ClientReq.Params
        module Results = Reader.Hs.ClientReq.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      module Quit = struct
        module Params = Builder.Hs.Quit.Params
        module Results = Reader.Hs.Quit.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:2
      end
      let method_name = function
        | 0 -> Some "sendMsg"
        | 1 -> Some "clientReq"
        | 2 -> Some "quit"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Hs" method_name
    end
  end

  module Service = struct
    module Hs = struct
      type t = [`Hs_b5a89f8ead4514ca]
      let interface_id = Stdint.Uint64.of_string "0xb5a89f8ead4514ca"
      module SendMsg = struct
        module Params = Reader.Hs.SendMsg.Params
        module Results = Builder.Hs.SendMsg.Results
      end
      module ClientReq = struct
        module Params = Reader.Hs.ClientReq.Params
        module Results = Builder.Hs.ClientReq.Results
      end
      module Quit = struct
        module Params = Reader.Hs.Quit.Params
        module Results = Builder.Hs.Quit.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#send_msg_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#client_req_impl
          | 2 -> MessageWrapper.Untyped.abstract_method self#quit_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Hs"
        method virtual send_msg_impl : (SendMsg.Params.t, SendMsg.Results.t) MessageWrapper.Service.method_t
        method virtual client_req_impl : (ClientReq.Params.t, ClientReq.Results.t) MessageWrapper.Service.method_t
        method virtual quit_impl : (Quit.Params.t, Quit.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
  end
  module MessageWrapper = MessageWrapper
end [@@inline]

module Make(M:Capnp.MessageSig.S) = MakeRPC[@inlined](Capnp.RPC.None(M)) [@@inline]
