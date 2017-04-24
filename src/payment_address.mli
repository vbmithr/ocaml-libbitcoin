open Ctypes

type t = private Payment_address of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

type version =
  | P2KH
  | P2SH
  | Testnet_P2KH
  | Testnet_P2SH

val version : t -> version

val of_b58check : Base58.t -> t option
val of_b58check_exn : Base58.t -> t
val of_point : ?version:version -> Ec_public.t -> t
val of_script : ?version:version -> Script.t -> t option

val to_b58check : t -> Base58.t
val to_hash : t -> Hash.short_hash
val to_script : t -> Script.t
