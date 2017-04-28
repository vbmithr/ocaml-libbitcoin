open Ctypes

type t = private Payment_address of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

val version : t -> Base58.Bitcoin.version
val is_valid : t -> bool

val of_b58check : Base58.Bitcoin.t -> t option
val of_b58check_exn : Base58.Bitcoin.t -> t
val of_point : ?version:Base58.Bitcoin.version -> Ec_public.t -> t
val of_script : ?version:Base58.Bitcoin.version -> Script.t -> t option
val of_script_exn : ?version:Base58.Bitcoin.version -> Script.t -> t

val to_b58check : t -> Base58.Bitcoin.t
val to_script : t -> Script.t
(** [to_script t] is a valid scriptPubKey to be used with [t] as an
    output address. *)
