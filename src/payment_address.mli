open Ctypes

type t = private Payment_address of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

val version : t -> Base58.Bitcoin.version
val is_valid : t -> bool

val of_b58check : Base58.Bitcoin.t -> t option
val of_b58check_exn : Base58.Bitcoin.t -> t
val of_point : ?version:Base58.Bitcoin.version -> Ec_public.t -> t

val of_script : ?version:Base58.Bitcoin.version -> Script.t -> t
(** [of_script ~version script] is [addr], where [addr] is the address
    of version [version], corresponding to the bitcoin160 hash of the
    binary serialized [script]. *)

val to_b58check : t -> Base58.Bitcoin.t
val to_script : t -> Script.t
(** [to_script t] is a valid scriptPubKey to be used with [t] as an
    output address. *)
