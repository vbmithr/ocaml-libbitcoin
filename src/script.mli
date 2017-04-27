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

module Script : sig
  module Opcode : sig
    type t =
      | Zero
      | Const of int
      | Dup
      | Drop
      | Hash160
      | Data of string
      | Equal
      | Equalverify
      | Checksig
      | Checkmultisig

    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

  type t = Opcode.t list
  val pp : Format.formatter -> t -> unit
end

type t = private Script of unit ptr

val pp : Format.formatter -> t -> unit
val show : t -> string

val of_ptr : unit ptr -> t
val of_ptr_nodestroy : unit ptr -> t

val invalid : unit -> t
  (** [invalid ()] is an empty script, invalid until modified. *)

val of_script : Script.t -> t
val of_chunk : ?prefix:bool -> Data.Chunk.t -> t option
val of_bytes : ?prefix:bool -> string -> t option
val of_hex : ?prefix:bool -> Hex.t -> t option
val of_mnemonic : string -> t option

val to_string : ?active_forks:rule_fork list -> t -> string
val to_bytes : ?prefix:bool -> t -> string

val is_valid : t -> bool

module P2PKH : sig
  val scriptPubKey : Base58.t -> t
  val scriptSig : string -> Ec_public.t -> t
end

module P2SH_multisig : sig
  val scriptPubKey : Base58.t -> t

  val scriptRedeem :
    ?append_script:Script.t ->
    threshold:int ->
    Ec_public.t list -> Script.t

  val scriptSig:
    endorsements:string list ->
    scriptRedeem:Script.t -> t
end
