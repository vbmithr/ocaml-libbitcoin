open Ctypes

type t = private Payment_address of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

type version =
  | P2KH
  | P2SH
  | WIF
  | BIP32_Pubkey
  | BIP32_Privkey

  | Testnet_P2KH
  | Testnet_P2SH
  | Testnet_WIF
  | Testnet_BIP32_Pubkey
  | Testnet_BIP32_Privkey

val of_b58check : string -> t option
val of_b58check_exn : string -> t
val of_point : ?version:version -> Ec_public.t -> t
val of_script : ?version:version -> Script.t -> t option

val to_b58check : t -> string
val to_hash : t -> Hash.short_hash
val to_script : t -> Script.t
