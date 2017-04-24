open Ctypes
open Foreign

let destroy = foreign "bc_destroy_payment_address"
    ((ptr void) @-> returning void)

let of_string = foreign "bc_create_payment_address_String"
    (string @-> returning (ptr void))

let of_point = foreign "bc_create_payment_address_Point_Version"
    ((ptr void) @-> int @-> returning (ptr void))

let of_script = foreign "bc_create_payment_address_Script_Version"
    ((ptr void) @-> int @-> returning (ptr void))

type t = Payment_address of unit ptr

type version =
  | P2KH
  | P2SH
  | Testnet_P2KH
  | Testnet_P2SH

let int_of_version = function
  | P2KH -> 0x00
  | P2SH -> 0x05
  | Testnet_P2KH -> 0x6F
  | Testnet_P2SH -> 0xC4

let version_of_int = function
  | 0x00 -> P2KH
  | 0x05 -> P2SH
  | 0x6F -> Testnet_P2KH
  | 0xC4 -> Testnet_P2SH
  | _ -> invalid_arg "Payment_address.version_of_int"

let version (Payment_address addr_ptr) =
  let version = foreign "bc_payment_address__version"
      (ptr void @-> returning int) in
  version_of_int (version addr_ptr)

let of_b58check (`Base58 str) =
  let ret = of_string str in
  if ptr_compare ret null = 0 then None
  else begin
    Gc.finalise destroy ret ;
    Some (Payment_address ret)
  end

let of_b58check_exn str =
  match of_b58check str with
  | None -> invalid_arg "Payment_address.of_b58check_exn"
  | Some t -> t

let of_point ?(version=P2KH) (Ec_public.Ec_public pk) =
  let ret = of_point pk (int_of_version version) in
  if ptr_compare ret null = 0 then failwith "of_point"
  else begin
    Gc.finalise destroy ret ;
    Payment_address ret
  end

let of_script ?(version=P2KH) (Script.Script script) =
  let ret = of_script script (int_of_version version) in
  if ptr_compare ret null = 0 then None
  else begin
    Gc.finalise destroy ret ;
    Some (Payment_address ret)
  end

let to_b58check (Payment_address t) =
  let encode = foreign "bc_payment_address__encoded"
      ((ptr void) @-> returning (ptr void)) in
  `Base58 Data.String.(to_string (of_ptr (encode t)))

let to_hash (Payment_address t) =
  let hash = foreign "bc_payment_address__hash"
      ((ptr void) @-> returning (ptr void)) in
  Hash.short_hash_of_ptr (hash t)

let to_script addr =
  let hash = to_hash addr in
  let `Hex hash_hex = Hash.short_hash_to_hex hash in
  let mnemonic = match version addr with
    | P2KH | Testnet_P2KH ->
      Printf.sprintf "dup hash160 [%s] equalverify checksig" hash_hex
    | P2SH | Testnet_P2SH ->
      Printf.sprintf "hash160 [%s] equal" hash_hex in
  match Script.of_mnemonic mnemonic with
  | Some script -> script
  | None -> failwith "Payment_address.to_script: internal"

let pp ppf addr =
  let `Base58 addr_str = to_b58check addr in
  Format.fprintf ppf "%s" addr_str

let show addr =
  Format.asprintf "%a" pp addr

