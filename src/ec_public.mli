open Ctypes

type t = private Ec_public of unit ptr

val of_private : Ec_private.t -> t

val of_bytes : string -> t option
val of_bytes_exn : string -> t
val of_hex : Hex.t -> t option
val of_hex_exn : Hex.t -> t

val to_bytes : t -> string
val to_hex : t -> Hex.t
