open Base

type t = (string * int) list
let empty = []
let toList x = x

let countLines acc x =
	let count = List.Assoc.find ~equal:String.equal acc x in
	match count with
		| None -> List.Assoc.add ~equal:String.equal acc x 1
		| Some c -> List.Assoc.add ~equal:String.equal acc x (c + 1)