(* message types we can send *)
type msg_type = Generic | GenericAck | NewView | Prepare | PrepareAck | PreCommit | PreCommitAck | Commit | CommitAck | Decide | Complain | NextView

type cmd = {data: string; callback_id: string}
module Cmd_set : Set.S with type elt = cmd

type node_justify = {node_offset: int; view: int; signature: Tezos_crypto.Aggregate_signature.t option; msg_type: msg_type; ids: int list} 
type node_internal = {justify: node_justify; height: int}
type node = {cmds: Cmd_set.t; parent: node option; i: node_internal option; digest: Tezos_crypto.Hacl.Blake2b.hash}
type qc = {node: node option; view: int; signature: Tezos_crypto.Aggregate_signature.t option; msg_type: msg_type; ids: int list}

type msg = {id: int; msg_type: msg_type; view: int; node: node option; justify: qc option; partial_signature: Tezos_crypto.Aggregate_signature.t option}

type action =
	| Broadcast of msg
	| SendLeader of msg
	| SendNextLeader of msg
	| SendClient of {id: int; callback_id: string; success: bool}
	| Execute of {id: int; node: node}
	| ResetTimer of {id: int; view: int}

type event = 
	| NewView of msg
	| Prepare of msg
	| PrepareAck of msg
	| PreCommit of msg
	| PreCommitAck of msg
	| Commit of msg
	| CommitAck of msg
	| Decide of msg
	| NextView of msg
	| Generic of msg
	| GenericAck of msg
	| ClientCmd of cmd
	| Timeout of {view: int}
	| Complain of msg
	| Beat

type crypto = {sk: Tezos_crypto.Aggregate_signature.secret_key; pks: Tezos_crypto.Aggregate_signature.public_key list}

exception ThresholdQCException
exception CannotExecuteException
exception EventTypeNotFoundException
exception LoggingException
exception NodeInternalException
exception MissingNodeException
exception MissingQcException
exception NeedKeysToUseCrypto