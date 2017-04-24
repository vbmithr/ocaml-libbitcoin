open Ctypes

type t = private Ec_public of unit ptr
val of_private : Ec_private.t -> t
val of_hex : Hex.t -> t option
val of_hex_exn : Hex.t -> t

val encode : t -> Hex.t
val to_bytes : t -> string
