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
  | WIF
  | BIP32_Pubkey
  | BIP32_Privkey

  | Testnet_P2KH
  | Testnet_P2SH
  | Testnet_WIF
  | Testnet_BIP32_Pubkey
  | Testnet_BIP32_Privkey

let int_of_version = function
  | P2KH -> 0x00
  | P2SH -> 0x05
  | WIF -> 0x80
  | BIP32_Pubkey -> 0x0488B21E
  | BIP32_Privkey -> 0x0488ADE4

  | Testnet_P2KH -> 0x6F
  | Testnet_P2SH -> 0xC4
  | Testnet_WIF -> 0xEF
  | Testnet_BIP32_Pubkey -> 0x043587CF
  | Testnet_BIP32_Privkey -> 0x04358394

let of_b58check str =
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
  Data.String.(to_string (of_ptr (encode t)))

let to_hash (Payment_address t) =
  let hash = foreign "bc_payment_address__hash"
      ((ptr void) @-> returning (ptr void)) in
  Hash.short_hash_of_ptr (hash t)

let to_script addr =
  let hash = to_hash addr in
  let `Hex hash_hex = Hash.short_hash_to_hex hash in
  match Script.of_mnemonic
          (Printf.sprintf "dup hash160 [%s] equalverify checksig" hash_hex) with
  | Some script -> script
  | None -> failwith "create_p2kh: internal"


