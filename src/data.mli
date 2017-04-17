open Ctypes

module String : sig
  type t = private String of unit ptr

  val of_ptr : unit ptr -> t
  val of_string : ?pos:int -> ?len:int -> string -> t

  val to_string : t -> string

  module List : sig
    type t = private String_list of unit ptr
    val of_ptr : unit ptr -> t

    val of_list : string list -> t
    val to_list : t -> string list
  end
end

module Chunk : sig
  type t = private Chunk of unit ptr

  val of_ptr : unit ptr -> t
  val of_ptr_nodestroy : unit ptr -> t
  val create : unit -> t
  val of_bytes : string -> t
  val of_hex : Hex.t -> t

  val to_bytes : t -> string
  val to_hex : t -> Hex.t
end
