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

type t = Script of unit ptr

let destroy = foreign "bc_destroy_script"
    ((ptr void) @-> returning void)

let create = foreign "bc_create_script"
    (void @-> returning (ptr void))

let create_data = foreign "bc_create_script_Data"
    ((ptr void) @-> bool @-> returning (ptr void))

let from_string = foreign "bc_script__from_string"
    ((ptr void) @-> string @-> returning bool)

let of_ptr_nodestroy ptr = Script ptr
let of_ptr ptr =
  Gc.finalise destroy ptr ;
  Script ptr

let invalid () =
  let ret = create () in
  Gc.finalise destroy ret ;
  Script ret

let of_chunk ?(prefix=false) (Data.Chunk.Chunk chunk) =
  let script = create_data chunk prefix in
  if ptr_compare script null = 0 then None
  else begin
    Gc.finalise destroy script ;
    Some (Script script)
  end

let of_bytes ?(prefix=false) bytes =
  of_chunk (Data.Chunk.of_bytes bytes)

let of_hex ?prefix hex =
  of_bytes ?prefix (Hex.to_string hex)

let to_string ?(active_forks=[]) (Script script) =
  let to_string = foreign "bc_script__to_string"
      (ptr void @-> int @-> returning (ptr void)) in
  let active_forks = int_of_rule_forks active_forks in
  let str = to_string script active_forks in
  Data.String.(of_ptr str |> to_string)

let pp ppf t =
  Format.fprintf ppf "%s" (to_string t)

let show t = to_string t

let of_mnemonic str =
  let script = create () in
  Gc.finalise destroy script ;
  match from_string script str with
  | false -> None
  | true -> Some (Script script)

let add_data ?(append_space=true) buf (`Hex data) =
  Buffer.add_char buf '[';
  Buffer.add_string buf data;
  Buffer.add_char buf ']';
  if append_space then Buffer.add_char buf ' '

let create_multisig ?data ~threshold addrs =
  let nb_addrs = List.length addrs in
  let addrs = ListLabels.map addrs ~f:Ec_public.encode in
  if threshold < 1 || threshold > nb_addrs then
    invalid_arg "create_multisig";
  let buf = Buffer.create 128 in
  begin match data with
  | None -> ()
  | Some data -> add_data buf (Hex.of_string data)
  end ;
  Buffer.add_string buf (string_of_int threshold) ;
  Buffer.add_char buf ' ' ;
  ListLabels.iter addrs ~f:(add_data buf) ;
  Buffer.add_string buf (string_of_int nb_addrs) ;
  Buffer.add_char buf ' ' ;
  Buffer.add_string buf "checkmultisig" ;
  match of_mnemonic (Buffer.contents buf) with
  | Some script -> script
  | None -> failwith "create_multisig: internal"

let endorsement endorsement pk =
  let `Hex endorsement_hex = Data.Chunk.to_hex endorsement in
  let `Hex pk_hex = Ec_public.encode pk in
  let mnemonic = Printf.sprintf "[%s] [%s]" endorsement_hex pk_hex in
  match of_mnemonic mnemonic with
  | Some script -> script
  | None -> failwith "endorsement: internal"

let is_valid (Script t) =
  let is_valid =
    foreign "bc_script__is_valid" ((ptr void) @-> returning bool) in
  let is_valid_operations =
    foreign "bc_script__is_valid_operations" ((ptr void) @-> returning bool) in
  is_valid t && is_valid_operations t
