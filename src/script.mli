open Ctypes

type t = private Script of unit ptr

val of_ptr : unit ptr -> t
val of_ptr_nodestroy : unit ptr -> t

val invalid : unit -> t
  (** [invalid ()] is an empty script, invalid until modified. *)

val of_chunk : ?prefix:bool -> Data.Chunk.t -> t option
val of_bytes : ?prefix:bool -> string -> t option
val of_hex : ?prefix:bool -> Hex.t -> t option
val of_mnemonic : string -> t option

val create_multisig :
  ?data:string -> threshold:int -> Ec_public.t list -> t

val endorsement : Data.Chunk.t -> Ec_public.t -> t

val is_valid : t -> bool
