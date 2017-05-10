open Ctypes

type t = private Ec_public of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

val of_private : Ec_private.t -> t

val of_bytes : string -> t option
val of_bytes_exn : string -> t
val of_hex : Hex.t -> t option
val of_hex_exn : Hex.t -> t

val of_uncomp_point : ?compress:bool -> string -> t option
val of_uncomp_point_exn : ?compress:bool -> string -> t

val to_bytes : t -> string
val to_hex : t -> Hex.t
