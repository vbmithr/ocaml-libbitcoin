open Ctypes
open Foreign

module Point : sig
  type t = private {
    mutable hash : Hash.Hash32.t ;
    mutable index : int ;
    point_ptr : unit ptr ;
  }

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Output_point : sig
  type t = private {
    point : Point.t ;
    output_point_ptr : unit ptr ;
  }

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val create : hash:Hash.Hash32.t -> index:int -> t
end

module Input : sig
  type t = private {
    sequence: Int32.t ;
    prev_out : Output_point.t ;
    mutable script : Script.t ;
    input_ptr : unit ptr ;
  }
  type input = t

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val create :
    ?sequence:Int32.t ->
    prev_out:Output_point.t ->
    script:Script.t ->
    unit -> t

  val set_script : t -> Script.t -> unit

  val is_valid : t -> bool

  module List : sig
    type t = private {
      inputs : input list ;
      input_list_ptr : unit ptr ;
    }
  end
end

module Output : sig
  type t = private {
    value : Int64.t ;
    script : Script. t ;
    output_ptr : unit ptr ;
  }
  type output = t

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val create : value:Int64.t -> script:Script.t -> t

  val is_valid : t -> bool

  module List : sig
    type t = private {
      outputs : output list ;
      output_list_ptr : unit ptr ;
    }

    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end
end

type locktime =
  | Immediate
  | Block of int
  | Time of Ptime.t

type t = private {
  hash : Hash.Hash32.t ;
  version : int ;
  locktime : locktime ;
  mutable inputs : Input.List.t ;
  mutable outputs : Output.List.t ;
  transaction_ptr : unit ptr ;
}

val pp : Format.formatter -> t -> unit
val show : t -> string

val set_inputs : t -> Input.t list -> unit
val set_outputs : t -> Output.t list -> unit

val create :
  ?version:int ->
  ?locktime:locktime ->
  Input.t list ->
  Output.t list -> t

val is_valid : t -> bool
val check : t -> (unit, string) result

val of_bytes : ?wire:bool -> string -> t option
val of_hex : ?wire:bool -> Hex.t -> t option

val to_bytes : ?wire:bool -> t -> string
val to_hex : ?wire:bool -> t -> Hex.t

module Sign : sig
  type hashtype =
    | All
    | None
    | Single
    | AnyoneCanPay

  type endorsement = private Endorsement of Data.Chunk.t

  val endorse :
    ?hashtype:hashtype list ->
    ?index:int ->
    tx:t ->
    prev_out_script:Script.t ->
    secret:Ec_private.Ec_secret.t ->
    unit -> endorsement option
end
