open Ctypes
open Foreign

let destroy = foreign "bc_destroy_payment_address"
    ((ptr void) @-> returning void)

let of_string = foreign "bc_create_payment_address_String"
    (string @-> returning (ptr void))

let of_point = foreign "bc_create_payment_address_Point_Version"
    (ptr void @-> int @-> returning (ptr_opt void))

let of_script = foreign "bc_create_payment_address_Script_Version"
    (ptr void @-> int @-> returning (ptr_opt void))

type t = Payment_address of unit ptr

let version (Payment_address addr_ptr) =
  let version = foreign "bc_payment_address__version"
      (ptr void @-> returning int) in
  Base58.Bitcoin.version_of_int (version addr_ptr)

let is_valid = foreign "bc_payment_address__is_valid"
    (ptr void @-> returning bool)

let of_b58check { Base58.Bitcoin.payload ; version } =
  let of_hash_version = foreign "bc_create_payment_address_Hash_Version"
      (ptr void @-> int @-> returning (ptr void)) in
  let Hash.Short_hash hash = Hash.short_hash_of_bytes payload in
  let version = Base58.Bitcoin.int_of_version version in
  let ret = of_hash_version hash version in
  if ptr_compare ret null <> 0 && is_valid ret then begin
    Gc.finalise destroy ret ;
    Some (Payment_address ret)
  end else
    None

let of_b58check_exn str =
  match of_b58check str with
  | None -> invalid_arg "Payment_address.of_b58check_exn"
  | Some t -> t

let of_point ?(version=Base58.Bitcoin.P2PKH) (Ec_public.Ec_public pk) =
  match of_point pk (Base58.Bitcoin.int_of_version version) with
  | Some ptr when is_valid ptr ->
    Gc.finalise destroy ptr ;
    Payment_address ptr
  | _ ->
    invalid_arg "Payment_address.of_point"

let of_script ?(version=Base58.Bitcoin.P2SH) (Script.Script script) =
  match of_script script (Base58.Bitcoin.int_of_version version) with
  | None -> invalid_arg "Payment_address.of_script"
  | Some t when not (is_valid t) ->
    destroy t ;
    invalid_arg "Payment_address.of_script: invalid address produced"
  | Some t ->
    Gc.finalise destroy t ;
    Payment_address t

let to_b58check (Payment_address t) =
  let version = foreign "bc_payment_address__version"
      (ptr void @-> returning int) in
  let hash = foreign "bc_payment_address__hash"
      (ptr void @-> returning (ptr void)) in
  let open Base58.Bitcoin in
  let version = version_of_int (version t) in
  let payload = Hash.(short_hash_of_ptr (hash t) |> short_hash_to_bytes) in
  Base58.Bitcoin.create ~version payload

let to_script addr =
  let open Base58.Bitcoin in
  let { version ; payload } as addr = to_b58check addr in
  match version with
    | P2PKH | Testnet_P2PKH -> Script.P2PKH.scriptPubKey addr
    | P2SH | Testnet_P2SH -> Script.P2SH_multisig.scriptPubKey addr
    | _ ->
      invalid_arg "Payment_address.to_script: wrong version (not a payment address)"

let pp ppf addr =
  let b58 = to_b58check addr in
  Base58.Bitcoin.pp ppf b58

let show addr =
  Format.asprintf "%a" pp addr

let is_valid (Payment_address addr_ptr) = is_valid addr_ptr

