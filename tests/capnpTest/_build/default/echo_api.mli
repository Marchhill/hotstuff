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

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
    module Echo : sig
      type t = [`Echo_e3c57123725aa3b6]
      val interface_id : Stdint.Uint64.t
      module Ping : sig
        module Params = Builder.Echo.Ping.Params
        module Results = Reader.Echo.Ping.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
  end

  module Service : sig
    module Echo : sig
      type t = [`Echo_e3c57123725aa3b6]
      val interface_id : Stdint.Uint64.t
      module Ping : sig
        module Params = Reader.Echo.Ping.Params
        module Results = Builder.Echo.Ping.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual ping_impl : (Ping.Params.t, Ping.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
