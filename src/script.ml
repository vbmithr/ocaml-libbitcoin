open Ctypes
open Foreign

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

let int_of_rule_fork = function
  | Easy_blocks -> 1 lsl 0
  | BIP16 -> 1 lsl 1
  | BIP30 -> 1 lsl 2
  | BIP34 -> 1 lsl 3
  | BIP66 -> 1 lsl 4
  | BIP65 -> 1 lsl 5
  | Allowed_duplicates -> 1 lsl 6
  | Deep_freeze -> 1 lsl 7
  | Activations -> 7 lsl 3
  | Consensus -> 63 lsl 1
  | All -> lnot 0

let int_of_rule_forks =
  ListLabels.fold_left ~init:0 ~f:begin fun a rf ->
    a lor (int_of_rule_fork rf)
  end

module Operation : sig
  type t

  val of_ptr_nodestroy : unit ptr -> t
  val to_bytes : t -> string
end= struct
  type t = Operation of unit ptr
  let to_data = foreign "bc_operation__to_data"
      (ptr void @-> returning (ptr void))

  let of_ptr_nodestroy ptr =
    Operation ptr

  let to_bytes (Operation ptr) =
    let chunk = Data.Chunk.of_ptr (to_data ptr) in
    Data.Chunk.to_bytes chunk
end

module Script = struct
  module Opcode = struct
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

    let to_string = function
      | Zero -> "zero"
      | Const v ->
        if v > 0 && v < 17 then string_of_int v
        else invalid_arg "Opcode.to_string: Const must belong to [1; 16]"
      | Dup -> "dup"
      | Drop -> "drop"
      | Hash160 -> "hash160"
      | Data data ->
        let `Hex data_hex = Hex.of_string data in "[" ^ data_hex ^ "]"
      | Equal -> "equal"
      | Equalverify -> "equalverify"
      | Checksig -> "checksig"
      | Checkmultisig -> "checkmultisig"

    let pp ppf t = Format.fprintf ppf "%s" (to_string t)
  end

  type t = Opcode.t list
  let pp ppf opcodes =
    let open Format in
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") Opcode.pp ppf opcodes
end

type t = Script of unit ptr

let destroy = foreign "bc_destroy_script"
    ((ptr void) @-> returning void)

let create = foreign "bc_create_script"
    (void @-> returning (ptr void))

let of_ptr_nodestroy ptr = Script ptr
let of_ptr ptr =
  Gc.finalise destroy ptr ;
  Script ptr

let invalid () =
  let ret = create () in
  Gc.finalise destroy ret ;
  Script ret

let of_chunk ?(prefix=false) (Data.Chunk.Chunk chunk) =
  let create_data = foreign "bc_create_script_Data"
      (ptr void @-> bool @-> returning (ptr void)) in
  let script = create_data chunk prefix in
  if ptr_compare script null = 0 then None
  else begin
    Gc.finalise destroy script ;
    Some (Script script)
  end

let of_chunk_exn ?prefix chunk =
  match of_chunk ?prefix chunk with
  | None -> invalid_arg "Script.of_chunk_exn"
  | Some t -> t

let of_bytes ?prefix bytes =
  of_chunk ?prefix (Data.Chunk.of_bytes bytes)

let of_bytes_exn ?prefix bytes =
  of_chunk_exn ?prefix (Data.Chunk.of_bytes bytes)

let of_hex ?prefix hex =
  of_bytes ?prefix (Hex.to_string hex)

let of_hex_exn ?prefix hex =
  of_bytes_exn ?prefix (Hex.to_string hex)

let to_string ?(active_forks=[]) (Script script) =
  let to_string = foreign "bc_script__to_string"
      (ptr void @-> int @-> returning (ptr void)) in
  let active_forks = int_of_rule_forks active_forks in
  let str = to_string script active_forks in
  Data.String.(of_ptr str |> to_string)

let to_bytes ?(prefix=false) (Script script) =
  let to_data = foreign "bc_script__to_data"
      (ptr void @-> bool @-> returning (ptr void)) in
  let ret = to_data script prefix in
  Data.Chunk.(to_bytes (of_ptr ret))

let to_hex ?prefix t =
  Hex.of_string (to_bytes ?prefix t)

let pp ppf t =
  Format.fprintf ppf "%s" (to_string t)

let show t = to_string t

let of_mnemonic str =
  let from_string = foreign "bc_script__from_string"
      (ptr void @-> string @-> returning bool) in
  let script = create () in
  Gc.finalise destroy script ;
  match from_string script str with
  | false -> None
  | true -> Some (Script script)

let of_mnemonic_exn str =
  match of_mnemonic str with
  | None -> invalid_arg "Script.of_mnemonic"
  | Some t -> t

let of_script opcodes =
  let opcodes_str = Format.asprintf "%a%!" Script.pp opcodes in
  match of_mnemonic opcodes_str with
  | None -> invalid_arg ("Script.of_script: " ^ opcodes_str)
  | Some script -> script

let is_valid (Script t) =
  let is_valid =
    foreign "bc_script__is_valid" ((ptr void) @-> returning bool) in
  let is_valid_operations =
    foreign "bc_script__is_valid_operations" ((ptr void) @-> returning bool) in
  is_valid t && is_valid_operations t

let at = foreign "bc_script__at"
    (ptr void @-> int @-> returning (ptr_opt void))

let operation (Script t) i =
  match at t i with
  | None -> None
  | Some ptr -> Some (Operation.of_ptr_nodestroy ptr)

module P2PKH = struct
  let scriptPubKey { Base58.Bitcoin.payload } =
    of_script
      Script.Opcode.[Dup ; Hash160 ; Data payload ; Equalverify ; Checksig]

  let scriptSig endorsement pk =
    let pk = Ec_public.to_bytes pk in
    of_script Script.Opcode.[Data endorsement ; Data pk]
end

module P2SH_multisig = struct
  let scriptPubKey { Base58.Bitcoin.payload } =
    of_script Script.Opcode.[Hash160 ; Data payload ; Equal]

  let scriptRedeem ?append_script ~threshold pks =
    let nb_pks = List.length pks in
    let open Script.Opcode in
    let addrs = ListLabels.rev_map pks ~f:begin fun pk ->
        Data (Ec_public.to_bytes pk)
      end in
    let script = List.rev_append addrs (Const nb_pks :: [Checkmultisig]) in
    let script = Const threshold :: script in
    match append_script with
    | None -> script
    | Some script' -> script' @ script

  let scriptSig ~endorsements ~scriptRedeem =
    let open Script.Opcode in
    let scriptRedeem_bytes = to_bytes scriptRedeem in
    let script = [Data scriptRedeem_bytes] in
    of_script
      (Zero :: (ListLabels.map endorsements ~f:(fun e -> Data e)) @ script)
end
