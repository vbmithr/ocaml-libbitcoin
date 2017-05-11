open Ctypes

module Header : sig
  type header_ptr

  type t = {
    version : int ;
    prev_block_hash : Hash.Hash32.t ;
    merkle : Hash.Hash32.t ;
    timestamp : Ptime.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
    hash : Hash.Hash32.t ;
    header_ptr : header_ptr ;
  }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

type block_ptr
type t = {
  header : Header.t ;
  transactions : Transaction.List.t ;
  block_ptr : block_ptr ;
}

val pp : Format.formatter -> t -> unit
val show : t -> string

val of_bytes : string -> t option
val of_bytes_exn : string -> t
