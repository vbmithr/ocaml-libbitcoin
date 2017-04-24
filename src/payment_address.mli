open Ctypes

type t = private Payment_address of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

type version =
  | P2PKH
  | P2SH
  | Testnet_P2PKH
  | Testnet_P2SH

val version : t -> version
val is_valid : t -> bool

val of_b58check : Base58.t -> t option
val of_b58check_exn : Base58.t -> t
val of_point : ?version:version -> Ec_public.t -> t
val of_script : ?version:version -> Script.t -> t option
val of_script_exn : ?version:version -> Script.t -> t

val to_b58check : t -> Base58.t
val to_hex : t -> Hex.t
val to_script : t -> Script.t
(** [to_script t] is a valid scriptPubKey to be used with [t] as an
    output address. *)
