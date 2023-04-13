@0xb287252b6cbed46e;

enum MsgType {
	newView @0;
	prepare @1;
	prepareAck @2;
	preCommit @3;
	preCommitAck @4;
	commit @5;
	commitAck @6;
	decide @7;
	generic @8;
	genericAck @9;
	nextView @10;
	complain @11;
}

struct Cmd {
	data @0 :Data;
	id @1 :Int64;
}

struct Node {
	cmd @0 :List(Cmd);
	justify @1 :NodeJustify;
	height @2 :Int32;
	digest @3 : Data;
}

struct QC {
	msgType @0 :MsgType;
	view @1 :Int32;
	node @2 :List(Node);
	signature @3 :Data;
	ids @4 :List(Int32);
}

struct NodeJustify {
	msgType @0 :MsgType;
	view @1 :Int32;
	nodeOffset @2 :Int32;
	signature @3 :Data;
	ids @4 :List(Int32);
}

struct Msg {
	curView @0 :Int32;
	type @1 :MsgType;
	node @2 :List(Node);
	justify @3 :QC;
	partialSignature @4 :Data;
	id @5 :Int32;
	tcpLens @6 :List(Int32);
}

interface Hs {
	sendMsg @0 (msg :Msg) -> ();
	clientReq @1 (cmd :Cmd) -> (success :Bool);
	quit @2 () -> ();
}