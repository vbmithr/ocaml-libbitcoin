open Ctypes
open Foreign

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

let of_mnemonic str =
  let script = create () in
  Gc.finalise destroy script ;
  match from_string script str with
  | false -> None
  | true -> Some (Script script)

let add_data ?(append_space=true) buf data =
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
  | Some data -> add_data buf data
  end ;
  Buffer.add_string buf (string_of_int threshold) ;
  Buffer.add_char buf ' ' ;
  ListLabels.iter addrs ~f:(fun (`Hex addr) -> add_data buf addr) ;
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
