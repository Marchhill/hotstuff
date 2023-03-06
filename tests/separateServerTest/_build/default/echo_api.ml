[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t


  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module Echo : sig
      type t = [`Echo_e3c57123725aa3b6]
      module Ping : sig
        module Params : sig
          type struct_t = [`Ping_a6218afbc91f999c]
          type t = struct_t reader_t
          val has_msg : t -> bool
          val msg_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Ping_899bc77de52e9ec4]
          type t = struct_t reader_t
          val has_reply : t -> bool
          val reply_get : t -> string
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
    module Echo : sig
      type t = [`Echo_e3c57123725aa3b6]
      module Ping : sig
        module Params : sig
          type struct_t = [`Ping_a6218afbc91f999c]
          type t = struct_t builder_t
          val has_msg : t -> bool
          val msg_get : t -> string
          val msg_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Ping_899bc77de52e9ec4]
          type t = struct_t builder_t
          val has_reply : t -> bool
          val reply_get : t -> string
          val reply_set : t -> string -> unit
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

    module Echo = struct
      type t = [`Echo_e3c57123725aa3b6]
      module Ping = struct
        module Params = struct
          type struct_t = [`Ping_a6218afbc91f999c]
          type t = struct_t reader_t
          let has_msg x =
            RA_.has_field x 0
          let msg_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Ping_899bc77de52e9ec4]
          type t = struct_t reader_t
          let has_reply x =
            RA_.has_field x 0
          let reply_get x =
            RA_.get_text ~default:"" x 0
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

    module Echo = struct
      type t = [`Echo_e3c57123725aa3b6]
      module Ping = struct
        module Params = struct
          type struct_t = [`Ping_a6218afbc91f999c]
          type t = struct_t builder_t
          let has_msg x =
            BA_.has_field x 0
          let msg_get x =
            BA_.get_text ~default:"" x 0
          let msg_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Ping_899bc77de52e9ec4]
          type t = struct_t builder_t
          let has_reply x =
            BA_.has_field x 0
          let reply_get x =
            BA_.get_text ~default:"" x 0
          let reply_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
    end
  end

  module Client = struct
    module Echo = struct
      type t = [`Echo_e3c57123725aa3b6]
      let interface_id = Stdint.Uint64.of_string "0xe3c57123725aa3b6"
      module Ping = struct
        module Params = Builder.Echo.Ping.Params
        module Results = Reader.Echo.Ping.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      let method_name = function
        | 0 -> Some "ping"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Echo" method_name
    end
  end

  module Service = struct
    module Echo = struct
      type t = [`Echo_e3c57123725aa3b6]
      let interface_id = Stdint.Uint64.of_string "0xe3c57123725aa3b6"
      module Ping = struct
        module Params = Reader.Echo.Ping.Params
        module Results = Builder.Echo.Ping.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#ping_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Echo"
        method virtual ping_impl : (Ping.Params.t, Ping.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
  end
  module MessageWrapper = MessageWrapper
end [@@inline]

module Make(M:Capnp.MessageSig.S) = MakeRPC[@inlined](Capnp.RPC.None(M)) [@@inline]
