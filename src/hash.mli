open Ctypes

module type S = sig
  type t
  val of_bytes_exn : string -> t
  val of_hex_exn : Hex.t -> t

  val of_bytes : string -> t option
  val of_hex : Hex.t -> t option

  val to_bytes : t -> bytes

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Hash32 : S

type hash = private Hash of unit ptr
type short_hash = private Short_hash of unit ptr

val hash_of_ptr : unit ptr -> hash
val hash_of_bytes : string -> hash
val hash_of_hex : Hex.t -> hash
(** [hash_of_hex hex] assumes [hex] is little-endian. *)

val hash_to_bytes : hash -> string
val hash_to_hex : hash -> Hex.t
(** [hash_to_hex h] will convert to bitcoin little-endian hex
    representation. *)

val short_hash_of_ptr : unit ptr -> short_hash
val short_hash_of_bytes : string -> short_hash
val short_hash_of_hex : Hex.t -> short_hash
val short_hash_to_bytes : short_hash -> string
val short_hash_to_hex : short_hash -> Hex.t
