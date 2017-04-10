open Ctypes
open Foreign

module Output_point : sig
  type t = private Output_point of unit ptr
  val create : tx:Hash.Hash32.t -> txid:int -> t
end

module Input : sig
  type t = private Input of unit ptr
  val create :
    ?sequence:int ->
    previous_output:Output_point.t ->
    script:Script.t ->
    unit -> t

  val get_script : t -> Script.t
  val set_script : t -> Script.t -> unit

  val is_valid : t -> bool
end

module Output : sig
  type t = private Output of unit ptr
  val create : value:Int64.t -> script:Script.t -> t

  val is_valid : t -> bool
end

type t = private Transaction of unit ptr

val get_inputs : t -> Input.t list
val get_outputs : t -> Output.t list
val set_inputs : t -> Input.t list -> unit
val set_outputs : t -> Output.t list -> unit

type locktime =
  | Immediate
  | Block of int
  | Time of Ptime.t

val create :
  ?version:int ->
  ?locktime:locktime ->
  Input.t list ->
  Output.t list -> t

val check : t -> (unit, string) result

val to_data : ?wire:bool -> t -> Data.Chunk.t

module Sign : sig
  type hashtype =
    | All
    | None
    | Single
    | AnyoneCanPay

  type endorsement = private Endorsement of Data.Chunk.t

  val endorse :
    ?hashtype:hashtype list ->
    tx:t ->
    input_id:int ->
    prev_out_script:Script.t ->
    secret:Ec_private.Ec_secret.t ->
    unit -> endorsement option
end
