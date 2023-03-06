open Base
open Stdio

let () =
	In_channel.fold_lines In_channel.stdin ~init:Counter.empty ~f:(Counter.countLines)
	|> Counter.toList
	|> List.iter ~f:(fun (s, c) -> printf "%d : %s\n" c s)