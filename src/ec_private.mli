open Ctypes

module Ec_secret : sig
  type t = private Ec_secret of unit ptr

  val of_bytes : string -> t
  val of_hex : Hex.t -> t
end

type t = private Ec_private of unit ptr

val of_wif : ?testnet:bool -> string -> t option
val of_wif_exn : ?testnet:bool -> string -> t

val of_secret :
  ?testnet:bool -> ?compress:bool -> Ec_secret.t -> t

val secret : t -> Ec_secret.t

val encode : t -> string
(** [encode t] is the WIF encoding of [t]. *)

val to_wif : t -> string
(** alias of [encode]. *)
