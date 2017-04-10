open Ctypes
open Foreign

module Output_point = struct

  let create = foreign
      "bc_create_output_point_Tuple"
      ((ptr void) @-> int @-> returning (ptr void))
  let destroy = foreign
      "bc_destroy_output_point"
      ((ptr void) @-> returning void)

  type t = Output_point of unit ptr
  let create ~tx ~txid =
    let tx_bytes = Hash.Hash32.to_bytes tx in
    let Hash.Hash h = Hash.hash_of_bytes tx_bytes in
    let ret = create h txid in
    Gc.finalise destroy ret ;
    Output_point ret
end

module Input = struct

  let create = foreign
      "bc_create_input_Values"
      ((ptr void) @-> (ptr void) @-> int @-> returning (ptr void))
  let destroy = foreign
      "bc_destroy_input"
      ((ptr void) @-> returning void)
  let get_script = foreign "bc_input__script"
      ((ptr void) @-> returning (ptr void))
  let set_script = foreign "bc_input__set_script"
      ((ptr void) @-> (ptr void) @-> returning void)
  let is_valid = foreign "bc_input__is_valid"
      ((ptr void) @-> returning bool)

  type t = Input of unit ptr
  type input = t
  let create
      ?(sequence=0xffff_ffff)
      ~previous_output:(Output_point.Output_point previous_output)
      ~script:(Script.Script script) () =
    let ret = create previous_output script sequence in
    Gc.finalise destroy ret ;
    Input ret

  let get_script (Input t) =
    Script.of_ptr_nodestroy (get_script t)

  let set_script (Input t) (Script.Script script) =
    set_script t script

  let is_valid (Input t) =
    is_valid t

  module List : sig
    type t = private Input_list of unit ptr
    val of_list : input list -> t
    val of_ptr_nodestroy : unit ptr -> t
    val to_list : t -> input list
  end = struct

    let create = foreign
        "bc_create_input_list"
        (void @-> returning (ptr void))
    let destroy = foreign
        "bc_destroy_input_list"
        ((ptr void) @-> returning void)
    let push_back = foreign
        "bc_input_list__push_back"
        ((ptr void) @-> (ptr void) @-> returning void)
    let size = foreign "bc_input_list__size"
        ((ptr void) @-> returning int)
    let get_at = foreign "bc_input_list__at"
        ((ptr void) @-> int @-> returning (ptr void))

    type t = Input_list of unit ptr
    let of_ptr_nodestroy ptr = Input_list ptr
    let of_list points =
      let ret = create () in
      ListLabels.iter points ~f:begin fun (Input p) ->
        push_back ret p
      end ;
      Gc.finalise destroy ret ;
      Input_list ret

    let to_list (Input_list inputs) =
      let rec get acc = function
        | n when n < 0 -> acc
        | n -> get (Input (get_at inputs n) :: acc) (pred n) in
      get [] (pred (size inputs))
  end
end

module Output = struct

  let create = foreign
      "bc_create_output_Value"
      (uint64_t @-> (ptr void) @-> returning (ptr void))
  let destroy = foreign
      "bc_destroy_output"
      ((ptr void) @-> returning void)
  let is_valid = foreign "bc_output__is_valid"
      ((ptr void) @-> returning bool)

  type t = Output of unit ptr
  type output = t
  let create ~value ~script:(Script.Script script) =
    let value = Unsigned.UInt64.of_int64 value in
    let ret = create value script in
    Gc.finalise destroy ret ;
    Output ret

  let is_valid (Output t) =
    is_valid t

  module List : sig
    type t = private Output_list of unit ptr
    val of_list : output list -> t
    val of_ptr_nodestroy : unit ptr -> t
    val to_list : t -> output list
  end = struct
    let create = foreign
        "bc_create_output_list"
        (void @-> returning (ptr void))
    let destroy = foreign
        "bc_destroy_output_list"
        ((ptr void) @-> returning void)
    let push_back = foreign
        "bc_output_list__push_back"
        ((ptr void) @-> (ptr void) @-> returning void)
    let size = foreign "bc_output_list__size"
        ((ptr void) @-> returning int)
    let get_at = foreign "bc_output_list__at"
        ((ptr void) @-> int @-> returning (ptr void))

    type t = Output_list of unit ptr
    let of_ptr_nodestroy outputs = Output_list outputs
    let of_list points =
      let ret = create () in
      ListLabels.iter points ~f:begin fun (Output p) ->
        push_back ret p
      end ;
      Gc.finalise destroy ret ;
      Output_list ret

    let to_list (Output_list outputs) =
      let rec get acc = function
        | n when n < 0 -> acc
        | n -> get (Output (get_at outputs n) :: acc) (pred n) in
      get [] (pred (size outputs))
  end
end

type t = Transaction of unit ptr

type locktime =
  | Immediate
  | Block of int
  | Time of Ptime.t

let int_of_locktime = function
  | Immediate -> 0
  | Block i -> i
  | Time t -> int_of_float (Ptime.to_float_s t)

let create = foreign
    "bc_create_transaction_Parts"
    (int @-> int @-> ptr void @-> ptr void @-> returning (ptr void))
let destroy = foreign
    "bc_destroy_transaction"
    (ptr void @-> returning void)
let get_input = foreign "bc_transaction__inputs"
    (ptr void @-> returning (ptr void))
let get_output = foreign "bc_transaction__outputs"
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

let create ?(version=1) ?(locktime=Immediate) inputs outputs =
  let Input.List.Input_list inputs = Input.List.of_list inputs in
  let Output.List.Output_list outputs = Output.List.of_list outputs in
  let ret =
    create version (int_of_locktime locktime) inputs outputs in
  Gc.finalise destroy ret ;
  Transaction ret

let get_inputs (Transaction t) =
  Input.List.(to_list (of_ptr_nodestroy (get_input t)))

let get_outputs (Transaction t) =
  Output.List.(to_list (of_ptr_nodestroy (get_output t)))

let set_inputs (Transaction t) inputs =
  let Input.List.Input_list input_list = Input.List.of_list inputs in
  set_inputs t input_list

let set_outputs (Transaction t) outputs =
  let Output.List.Output_list output_list = Output.List.of_list outputs in
  set_outputs t output_list

let to_data ?(wire=true) (Transaction t) =
  let f = if wire then to_data else to_data_nowire in
  Data.Chunk.of_ptr (f t)

let check (Transaction t) =
  let ret = check t in
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

  type endorsement = Endorsement of Data.Chunk.t

  let create_endorsement = foreign "bc_script__create_endorsement"
      ((ptr void) @-> (ptr void) @-> (ptr void) @->
       (ptr void) @-> int @-> int @-> returning bool)

  let endorse
      ?(hashtype=[])
      ~tx:(Transaction tx)
      ~input_id
      ~prev_out_script:(Script.Script script)
      ~secret:(Ec_private.Ec_secret.Ec_secret secret)
      () =
    let chunk = Data.Chunk.create () in
    let chunk_ptr = match chunk with Data.Chunk.Chunk ptr -> ptr in
    match create_endorsement chunk_ptr secret script
            tx input_id (int_of_hashtypes hashtype) with
    | true -> Some (Endorsement chunk)
    | false -> None
end
