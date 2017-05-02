open Ctypes
open Foreign

module Point : sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val of_ptr : unit ptr -> t
  val create : hash:Hash.Hash32.t -> index:int -> t

  val get_ptr : t -> unit ptr
  val get_hash : t -> Hash.Hash32.t
  val get_index : t -> int
  val set_hash : t -> Hash.Hash32.t -> unit
  val set_index : t -> int -> unit
end = struct
  type t = {
    mutable hash : Hash.Hash32.t ;
    mutable index : int ;
    point_ptr : unit ptr ;
  }

  let pp ppf { hash ; index } =
    Format.fprintf ppf "%a:%d" Hash.Hash32.pp hash index

  let show t =
    Format.asprintf "%a" pp t

  let of_ptr point_ptr =
    let get_hash = foreign "bc_point__hash"
        (ptr void @-> returning (ptr void)) in
    let get_index = foreign "bc_point__index"
        (ptr void @-> returning int) in
    let hash =
      Hash.(get_hash point_ptr |> hash_of_ptr |> hash_to_bytes |> Hash32.of_bytes_exn) in
    let index = get_index point_ptr in
    {  hash ; index ; point_ptr }

  let create ~hash ~index =
    let create = foreign "bc_create_point_Tuple"
        (ptr void @-> int @-> returning (ptr void)) in
    let destroy = foreign "bc_destroy_point"
        (ptr void @-> returning void) in
    let Hash.Hash h = Hash.(hash_of_bytes (Hash32.to_bytes hash)) in
    let point_ptr = create h index in
    Gc.finalise destroy point_ptr ;
    { hash ; index ; point_ptr }

  let get_ptr { point_ptr } = point_ptr
  let get_hash { hash } = hash
  let get_index { index } = index

  let set_hash t hash =
    let set_hash = foreign "bc_point__set_hash"
        (ptr void @-> ptr void @-> returning void) in
    let Hash.Hash h = Hash.(hash_of_bytes (Hash32.to_bytes hash)) in
    set_hash t.point_ptr h ;
    t.hash <- hash

  let set_index t index =
    let set_index = foreign "bc_point__set_index"
        (ptr void @-> int @-> returning void) in
    set_index t.point_ptr index ;
    t.index <- index
end

module Output_point : sig
  type t = private {
    point : Point.t ;
    output_point_ptr : unit ptr ;
  }

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val of_ptr : unit ptr -> t
  val of_point : Point.t -> t
  val create : hash:Hash.Hash32.t -> index:int -> t
end = struct

  let destroy = foreign "bc_destroy_output_point"
      (ptr void @-> returning void)

  type t = {
    point : Point.t ;
    output_point_ptr : unit ptr ;
  }

  let pp ppf { point } = Point.pp ppf point
  let show { point } = Point.show point

  let of_ptr output_point_ptr =
    let point_base = foreign "bc_output_point__point_Base"
        (ptr void @-> returning (ptr void)) in
    let point = Point.of_ptr (point_base output_point_ptr) in
    { point ; output_point_ptr }

  let of_point point =
    let create = foreign "bc_create_output_point_Point"
        (ptr void @-> returning (ptr void)) in
    let output_point_ptr = create (Point.get_ptr point) in
    Gc.finalise destroy output_point_ptr ;
    {
      point = point ;
      output_point_ptr ;
    }

  let create ~hash ~index =
    of_point (Point.create ~hash ~index)
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

  val get_script : t -> Script.t
  val set_script : t -> Script.t -> unit

  val of_bytes : ?wire:bool -> string -> t option
  val of_bytes_exn : ?wire:bool -> string -> t
  val to_bytes : ?wire:bool -> t -> string

  val of_hex : ?wire:bool -> Hex.t -> t option
  val of_hex_exn : ?wire:bool -> Hex.t -> t
  val to_hex : ?wire:bool -> t -> Hex.t

  val is_valid : t -> bool

  module List : sig
    type t = private {
      inputs : input list ;
      input_list_ptr : unit ptr ;
    }

    val show : t -> string
    val pp : Format.formatter -> t -> unit

    val of_ptr : unit ptr -> t
    val of_list : input list -> t
  end
end = struct

  let empty = foreign "bc_create_input"
      (void @-> returning (ptr void))
  let create = foreign "bc_create_input_Values"
      (ptr void @-> ptr void @-> int32_t @-> returning (ptr void))
  let destroy = foreign "bc_destroy_input"
      (ptr void @-> returning void)
  let get_script = foreign "bc_input__script"
      (ptr void @-> returning (ptr void))
  let get_sequence = foreign "bc_input__sequence"
      (ptr void @-> returning int32_t)
  let get_previous_output = foreign "bc_input__previous_output"
      (ptr void @-> returning (ptr void))
  let set_script = foreign "bc_input__set_script"
      (ptr void @-> ptr void @-> returning void)
  let is_valid = foreign "bc_input__is_valid"
      (ptr void @-> returning bool)
  let to_data = foreign "bc_input__to_data"
      (ptr void @-> returning (ptr void))
  let to_data_nowire = foreign "bc_input__to_data_nowire"
      (ptr void @-> returning (ptr void))
  let from_data = foreign "bc_input__from_data"
      (ptr void @-> ptr void @-> returning bool)
  let from_data_nowire = foreign "bc_input__from_data_nowire"
      (ptr void @-> ptr void @-> returning bool)

  type t = {
    sequence: Int32.t ;
    prev_out : Output_point.t ;
    mutable script : Script.t ;
    input_ptr : unit ptr ;
  }
  type input = t
  let pp ppf { sequence ; prev_out ; script } =
    Format.fprintf ppf
      "{@[<hov 1> sequence = 0x%lx ;@;prev_out = %a ;@;script = %a }@]"
      sequence Output_point.pp prev_out Script.pp script

  let show t =
    Format.asprintf "%a" pp t

  let compare t t' = ptr_compare t.input_ptr t'.input_ptr

  let of_ptr input_ptr =
    let sequence = get_sequence input_ptr in
    let prev_out = Output_point.of_ptr (get_previous_output input_ptr) in
    let script = Script.of_ptr_nodestroy (get_script input_ptr) in
    { sequence ; prev_out ; script ; input_ptr }

  let to_bytes ?(wire=true) { input_ptr } =
    let to_data_f = match wire with true -> to_data | false -> to_data_nowire in
    Data.Chunk.(to_data_f input_ptr |> of_ptr |> to_bytes)

  let of_bytes ?(wire=true) bytes =
    let Data.Chunk.Chunk chunk_ptr = Data.Chunk.of_bytes bytes in
    let from_data_f = match wire with true -> from_data | false -> from_data_nowire in
    let empty = empty () in
    match from_data_f empty chunk_ptr with
    | false -> None
    | true -> Some (of_ptr empty)

  let of_bytes_exn ?wire bytes =
    match (of_bytes ?wire bytes) with
    | None -> invalid_arg "Transaction.of_bytes_exn"
    | Some t -> t

  let to_hex ?wire t = Hex.of_string (to_bytes ?wire t)
  let of_hex ?wire hex = of_bytes ?wire (Hex.to_string hex)
  let of_hex_exn ?wire hex = of_bytes_exn ?wire (Hex.to_string hex)

  let create
      ?(sequence=0xffff_ffffl)
      ~prev_out_hash
      ~prev_out_index
      ~script:(Script.Script script_ptr as script) () =
    let prev_out =
      Output_point.create prev_out_hash prev_out_index in
    let input_ptr =
      create prev_out.output_point_ptr script_ptr sequence in
    Gc.finalise destroy input_ptr ;
    { sequence ; prev_out ; script ; input_ptr }

  let get_script { script } = script

  let set_script t (Script.Script script_ptr as script) =
    set_script t.input_ptr script_ptr ;
    t.script <- script

  let is_valid { input_ptr } =
    is_valid input_ptr

  module List = struct

    let create = foreign
        "bc_create_input_list"
        (void @-> returning (ptr void))
    let destroy = foreign
        "bc_destroy_input_list"
        ((ptr void) @-> returning void)
    let push_back = foreign
        "bc_input_list__push_back"
        ((ptr void) @-> (ptr void) @-> returning void)

    type t = {
      inputs : input list ;
      input_list_ptr : unit ptr ;
    }

    let pp ppf t =
      Format.pp_print_list pp ppf t.inputs

    let show t =
      Format.asprintf "%a" pp t

    let to_list input_list_ptr =
      let size = foreign "bc_input_list__size"
          (ptr void @-> returning int) in
      let get_at = foreign "bc_input_list__at"
          (ptr void @-> int @-> returning (ptr void)) in
      let rec get acc = function
        | n when n < 0 -> acc
        | n -> get (of_ptr (get_at input_list_ptr n) :: acc) (pred n) in
      get [] (pred (size input_list_ptr))

    let of_ptr input_list_ptr =
      let inputs = to_list input_list_ptr in
      { inputs ; input_list_ptr }

    let of_list inputs =
      let input_list_ptr = create () in
      ListLabels.iter inputs ~f:begin fun ( { input_ptr } ) ->
        push_back input_list_ptr input_ptr
      end ;
      Gc.finalise destroy input_list_ptr ;
      { inputs ; input_list_ptr }
  end
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

  module List : sig
    type t = private {
      outputs : output list ;
      output_list_ptr : unit ptr ;
    }

    val show : t -> string
    val pp : Format.formatter -> t -> unit

    val of_ptr : unit ptr -> t
    val of_list : output list -> t
  end
end = struct
  let create = foreign
      "bc_create_output_Value"
      (int64_t @-> ptr void @-> returning (ptr void))
  let destroy = foreign
      "bc_destroy_output"
      (ptr void @-> returning void)
  let is_valid = foreign "bc_output__is_valid"
      (ptr void @-> returning bool)

  type t = {
    mutable value : Int64.t ;
    mutable script : Script.t ;
    output_ptr : unit ptr ;
  }

  let compare t t' =
    ptr_compare t.output_ptr t'.output_ptr

  type output = t

  let pp ppf { value ; script } =
    Format.fprintf ppf "{@[<hov 1> value = %Ld ;@;script = %a }@]"
      value Script.pp script

  let show t =
    Format.asprintf "%a" pp t

  let of_ptr output_ptr =
    let get_value = foreign "bc_output__value"
        (ptr void @-> returning int64_t) in
    let get_script = foreign "bc_output__script"
        (ptr void @-> returning (ptr void)) in
    let value = get_value output_ptr in
    let script = Script.of_ptr_nodestroy (get_script output_ptr) in
    { value ; script ; output_ptr }

  let create ~value ~script:(Script.Script script_ptr as script) =
    let output_ptr = create value script_ptr in
    Gc.finalise destroy output_ptr ;
    { value ; script ; output_ptr }

  let get_value { value } = value
  let get_script { script } = script

  let set_value t value =
    let set_value = foreign "bc_output__set_value"
        (ptr void @-> int64_t @-> returning void) in
    set_value t.output_ptr value ;
    t.value <- value

  let set_script t (Script.Script script_ptr as script) =
    let set_script = foreign "bc_output__set_script"
        (ptr void @-> ptr void @-> returning void) in
    set_script t.output_ptr script_ptr ;
    t.script <- script

  let is_valid { output_ptr } =
    is_valid output_ptr

  module List = struct
    let create = foreign "bc_create_output_list"
        (void @-> returning (ptr void))
    let destroy = foreign "bc_destroy_output_list"
        (ptr void @-> returning void)
    let push_back = foreign "bc_output_list__push_back"
        (ptr void @-> ptr void @-> returning void)
    let size = foreign "bc_output_list__size"
        (ptr void @-> returning int)
    let get_at = foreign "bc_output_list__at"
        (ptr void @-> int @-> returning (ptr void))

    type t = {
      outputs : output list ;
      output_list_ptr : unit ptr ;
    }

    let pp ppf t =
      let open Format in
      let pp_sep fmt () = fprintf fmt " ;@;" in
      pp_print_list ~pp_sep pp ppf t.outputs

    let show t =
      Format.asprintf "%a" pp t

    let to_list output_list_ptr =
      let rec get acc = function
        | n when n < 0 -> acc
        | n -> get (of_ptr (get_at output_list_ptr n) :: acc) (pred n) in
      get [] (pred (size output_list_ptr))

    let of_ptr output_list_ptr =
      let outputs = to_list output_list_ptr in
      { outputs ; output_list_ptr }

    let of_list outputs =
      let output_list_ptr = create () in
      ListLabels.iter outputs ~f:begin fun { output_ptr } ->
        push_back output_list_ptr output_ptr
      end ;
      Gc.finalise destroy output_list_ptr ;
      { outputs ; output_list_ptr }
  end
end

type locktime =
  | Immediate
  | Block of int
  | Time of Ptime.t

let pp_locktime ppf = function
  | Immediate -> Format.fprintf ppf "Immediate"
  | Block i -> Format.fprintf ppf "Block %d" i
  | Time pt -> Format.fprintf ppf "Time %a" Ptime.pp pt

let int_of_locktime = function
  | Immediate -> 0
  | Block i -> i
  | Time t -> int_of_float (Ptime.to_float_s t)

let locktime_of_int = function
  | 0 -> Immediate
  | n when n <= 500_000_000 -> Block n
  | n ->
      match Ptime.of_float_s (float_of_int n) with
      | None -> invalid_arg "locktime_of_int"
      | Some t -> Time t

type t = {
  hash : Hash.Hash32.t ;
  version : int ;
  locktime : locktime ;
  mutable inputs : Input.List.t ;
  mutable outputs : Output.List.t ;
  transaction_ptr : unit ptr ;
}

let pp ppf { hash ; version ; locktime ; inputs ; outputs } =
  Format.fprintf ppf
    "{@[<hov 1> hash = %a ;@;version@ = %d ;@;locktime = %a ;\
     @;inputs = [@[<hov 0>%a@]] ;@;outputs = [@[<hov 0>%a@]]}@]"
    Hash.Hash32.pp hash version pp_locktime locktime
    Input.List.pp inputs Output.List.pp outputs

let show t =
  Format.asprintf "%a" pp t

let create = foreign "bc_create_transaction_Parts"
    (int @-> int @-> ptr void @-> ptr void @-> returning (ptr void))
let destroy = foreign "bc_destroy_transaction"
    (ptr void @-> returning void)
let get_inputs = foreign "bc_transaction__inputs"
    (ptr void @-> returning (ptr void))
let get_outputs = foreign "bc_transaction__outputs"
    (ptr void @-> returning (ptr void))
let set_inputs = foreign "bc_transaction__set_inputs"
    (ptr void @-> ptr void @-> returning void)
let set_outputs = foreign "bc_transaction__set_outputs"
    (ptr void @-> ptr void @-> returning void)
let is_valid = foreign "bc_transaction__is_valid"
    (ptr void @-> returning bool)
let to_data = foreign "bc_transaction__to_data"
    (ptr void @-> returning (ptr void))
let to_data_nowire = foreign "bc_transaction__to_data_nowire"
    (ptr void @-> returning (ptr void))
let check = foreign "bc_transaction__check"
    (ptr void @-> returning (ptr void))
let get_hash = foreign "bc_transaction__hash"
    (ptr void @-> returning (ptr void))

let of_ptr_nodestroy transaction_ptr =
  let get_version = foreign "bc_transaction__version"
      (ptr void @-> returning int) in
  let get_locktime = foreign "bc_transaction__locktime"
      (ptr void @-> returning int) in
  let version = get_version transaction_ptr in
  let locktime = locktime_of_int (get_locktime transaction_ptr) in
  let hash = Hash.hash_of_ptr (get_hash transaction_ptr) in
  let hash = Hash.Hash32.of_bytes_exn (Hash.hash_to_bytes hash) in
  let inputs = Input.List.of_ptr (get_inputs transaction_ptr) in
  let outputs = Output.List.of_ptr (get_outputs transaction_ptr) in
  { hash ; version ; locktime ; inputs ; outputs ; transaction_ptr }

let create ?(version=1) ?(locktime=Immediate) inputs outputs =
  let inputs = Input.List.of_list inputs in
  let outputs = Output.List.of_list outputs in
  let transaction_ptr =
    create version (int_of_locktime locktime)
      inputs.input_list_ptr
      outputs.output_list_ptr in
  Gc.finalise destroy transaction_ptr ;
  let hash = Hash.hash_of_ptr (get_hash transaction_ptr) in
  let hash = Hash.Hash32.of_bytes_exn (Hash.hash_to_bytes hash) in
  { hash ; version ; locktime ; inputs ; outputs ; transaction_ptr }

let get_inputs { inputs } = inputs.inputs
let get_outputs { outputs } = outputs.outputs

let set_inputs t inputs =
  let inputs = Input.List.of_list inputs in
  set_inputs t.transaction_ptr inputs.input_list_ptr ;
  t.inputs <- inputs

let set_outputs t outputs =
  let outputs = Output.List.of_list outputs in
  set_outputs t.transaction_ptr outputs.output_list_ptr ;
  t.outputs <- outputs

let from_data ?(wire=true) (Data.Chunk.Chunk chunk) =
  let factory_from_data = foreign "bc_transaction__factory_from_data"
      (ptr void @-> returning (ptr void)) in
  let factory_from_data_nowire = foreign "bc_transaction__factory_from_data_nowire"
      (ptr void @-> returning (ptr void)) in
  let from_data = if wire then factory_from_data else factory_from_data_nowire in
  let transaction_ptr = from_data chunk in
  if ptr_compare transaction_ptr null = 0 then (None : t option)
  else begin
    Gc.finalise destroy transaction_ptr ;
    Some (of_ptr_nodestroy transaction_ptr)
  end

let of_bytes ?wire bytes =
  from_data ?wire (Data.Chunk.of_bytes bytes)

let of_hex ?wire hex =
  from_data ?wire (Data.Chunk.of_hex hex)

let of_bytes_exn ?wire bytes =
  match of_bytes ?wire bytes with
  | None -> invalid_arg "Transaction.of_bytes"
  | Some t -> t

let of_hex_exn ?wire hex =
  match of_hex ?wire hex with
  | None -> invalid_arg "Transaction.of_hex"
  | Some t -> t

let to_data ?(wire=true) { transaction_ptr } =
  let f = if wire then to_data else to_data_nowire in
  Data.Chunk.of_ptr (f transaction_ptr)

let to_bytes ?wire t =
  Data.Chunk.to_bytes (to_data ?wire t)

let to_hex ?wire t =
  Data.Chunk.to_hex (to_data ?wire t)

let is_valid { transaction_ptr } = is_valid transaction_ptr

let serialized_size { transaction_ptr } =
  let serialized_size = foreign "bc_transaction__serialized_size"
      (ptr void @-> returning int) in
  serialized_size transaction_ptr

let check { transaction_ptr } =
  let ret = check transaction_ptr in
  let error = Error.of_ptr ret in
  match Error.message error with
  | None -> Ok ()
  | Some msg -> Error msg

module Sign = struct
  type hashtype =
    | All
    | None
    | Single
    | AnyoneCanPay

  let int_of_hashtype = function
    | All -> 0x1
    | None -> 0x2
    | Single -> 0x3
    | AnyoneCanPay -> 0x80

  let int_of_hashtypes hts =
    ListLabels.fold_left hts ~init:0 ~f:begin fun acc ht ->
      let ht = int_of_hashtype ht in
      acc lor ht
    end

  let create_endorsement = foreign "bc_script__create_endorsement"
      ((ptr void) @-> (ptr void) @-> (ptr void) @->
       (ptr void) @-> int @-> int @-> returning bool)

  let endorse
      ?(hashtype=[All])
      ~tx:{ transaction_ptr }
      ~index
      ~prev_out_script:(Script.Script script)
      ~secret:(Ec_private.Ec_secret.Ec_secret secret)
      () =
    let chunk = Data.Chunk.create () in
    let chunk_ptr = match chunk with Data.Chunk.Chunk ptr -> ptr in
    match create_endorsement chunk_ptr secret script
            transaction_ptr index (int_of_hashtypes hashtype) with
    | true ->
      Some (Data.Chunk.to_bytes chunk)
    | false -> None

  let endorse_exn
      ?hashtype
      ~tx
      ~index
      ~prev_out_script
      ~secret
      () =
    match endorse ?hashtype ~tx ~index ~prev_out_script ~secret () with
    | None -> invalid_arg "Transaction.Sign.endorse_exn"
    | Some endorsement -> endorsement
end
