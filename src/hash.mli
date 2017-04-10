open Ctypes

module type S = sig
  type t
  val of_bytes_exn : string -> t
  val of_hex_exn : Hex.t -> t

  val of_bytes : string -> t option
  val of_hex : Hex.t -> t option

  val to_bytes : t -> string
  val to_hex : t -> Hex.t
end

module Make (Size : sig val length : int end) : S

module Hash20 : S
module Hash32 : S

type hash = private Hash of unit ptr
type short_hash = private Short_hash of unit ptr

val hash : unit ptr -> hash
val hash_of_bytes : string -> hash
val hash_of_hex : Hex.t -> hash
val hash_to_bytes : hash -> string
val hash_to_hex : hash -> Hex.t

val short_hash : unit ptr -> short_hash
val short_hash_of_bytes : string -> short_hash
val short_hash_of_hex : Hex.t -> short_hash
val short_hash_to_bytes : short_hash -> string
val short_hash_to_hex : short_hash -> Hex.t
