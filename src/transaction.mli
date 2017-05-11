open Ctypes
open Foreign

module Point : sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Output_point : sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val create : hash:Hash.Hash32.t -> index:int -> t
end

module Input : sig
  type t
  type input = t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int

  val create :
    ?sequence:Int32.t ->
    prev_out_hash:Hash.Hash32.t ->
    prev_out_index:int ->
    script:Script.t ->
    unit -> t

  val of_bytes : ?wire:bool -> string -> t option
  val of_bytes_exn : ?wire:bool -> string -> t
  val to_bytes : ?wire:bool -> t -> string

  val of_hex : ?wire:bool -> Hex.t -> t option
  val of_hex_exn : ?wire:bool -> Hex.t -> t
  val to_hex : ?wire:bool -> t -> Hex.t

  val is_valid : t -> bool

  val get_script : t -> Script.t
  val set_script : t -> Script.t -> unit

  val serialized_size : ?wire:bool -> t -> int
end

module Output : sig
  type t
  type output = t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int

  val create : value:Int64.t -> script:Script.t -> t

  val get_value : t -> Int64.t
  val get_script : t -> Script.t

  val set_value : t -> Int64.t -> unit
  val set_script : t -> Script.t -> unit

  val is_valid : t -> bool
end

type locktime =
  | Immediate
  | Block of int
  | Time of Ptime.t

type input_outputs
type transaction_ptr

type t = {
  hash : Hash.Hash32.t ;
  version : int ;
  locktime : locktime ;

  ios : input_outputs ;
  transaction_ptr : transaction_ptr ;
}

val pp : Format.formatter -> t -> unit
val show : t -> string

val get_inputs : t -> Input.t list
val get_outputs : t -> Output.t list

val set_inputs : t -> Input.t list -> unit
val set_outputs : t -> Output.t list -> unit

val create :
  ?version:int ->
  ?locktime:locktime ->
  Input.t list ->
  Output.t list -> t

val is_valid : t -> bool
val serialized_size : t -> int
val check : t -> (unit, string) result

val of_bytes : ?wire:bool -> string -> t option
val of_hex : ?wire:bool -> Hex.t -> t option

val of_bytes_exn : ?wire:bool -> string -> t
val of_hex_exn : ?wire:bool -> Hex.t -> t

val to_bytes : ?wire:bool -> t -> string
val to_hex : ?wire:bool -> t -> Hex.t

module List : sig
  type transaction_list_ptr

  type nonrec t = {
    transactions : t list ;
    transaction_list_ptr : transaction_list_ptr ;
  }

  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val of_ptr : unit ptr -> t
end

module Sign : sig
  type hashtype =
    | All
    | None
    | Single
    | AnyoneCanPay

  val endorse :
    ?hashtype:hashtype list ->
    tx:t ->
    index:int ->
    prev_out_script:Script.t ->
    secret:Ec_private.Ec_secret.t ->
    unit -> string option

  val endorse_exn :
    ?hashtype:hashtype list ->
    tx:t ->
    index:int ->
    prev_out_script:Script.t ->
    secret:Ec_private.Ec_secret.t ->
    unit -> string
end
