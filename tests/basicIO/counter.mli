open Base

type t

val empty : t

val countLines : t -> string -> t

val toList : t -> (string * int) list