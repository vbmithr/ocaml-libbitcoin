open Ctypes

type rule_fork =
  | Easy_blocks
  | BIP16
  | BIP30
  | BIP34
  | BIP66
  | BIP65
  | Allowed_duplicates
  | Deep_freeze
  | Activations
  | Consensus
  | All

type t = private Script of unit ptr

val of_ptr : unit ptr -> t
val of_ptr_nodestroy : unit ptr -> t

val to_string : ?active_forks:rule_fork list -> t -> string
val pp : Format.formatter -> t -> unit
val show : t -> string

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
