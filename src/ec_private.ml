open Ctypes
open Foreign

module Ec_secret = struct
  type t = Ec_secret of unit ptr
  let destroy = foreign "bc_destroy_ec_secret"
      ((ptr void) @-> returning void)

  let of_bytes = foreign "bc_create_ec_secret_Data"
      (string @-> returning (ptr void))

  let of_hex seed_hex =
    let t = of_bytes (Hex.to_string seed_hex) in
    Gc.finalise destroy t ;
    Ec_secret t

  let of_bytes seed =
    let t = of_bytes seed in
    Gc.finalise destroy t ;
    Ec_secret t
end

type version =
  | Mainnet
  | Testnet

let mainnet =
  let f = foreign "bc_ec_private__mainnet" (void @-> returning int) in
  lazy (f ())

let testnet =
  let f = foreign "bc_ec_private__testnet" (void @-> returning int) in
  lazy (f ())

let int_of_version = function
  | Mainnet -> Lazy.force mainnet
  | Testnet -> Lazy.force testnet

type t = Ec_private of unit ptr

let destroy = foreign "bc_destroy_ec_private"
    ((ptr void) @-> returning void)

let of_wif ?(version=Mainnet) wif =
  let create = foreign "bc_create_ec_private_String_Version"
      (string @-> int @-> returning (ptr void)) in
  let t = create wif (int_of_version version) in
  if ptr_compare t null = 0 then None
  else begin
    Gc.finalise destroy t ;
    Some (Ec_private t)
  end

let of_wif_exn ?version wif =
  match of_wif ?version wif with
  | None -> invalid_arg "Ec_private.of_wif_exn"
  | Some t -> t

let of_secret ?(version=Mainnet) ?(compress=true) (Ec_secret.Ec_secret secret) =
  let create = foreign "bc_create_ec_private_Secret_Version"
      ((ptr void) @-> int @-> returning (ptr void)) in
  let create_uncompressed = foreign "bc_create_ec_private_Secret_Version_nocompress"
      ((ptr void) @-> int @-> returning (ptr void)) in
  let create_f = (if compress then create else create_uncompressed) in
  let t = create_f secret (int_of_version version) in
  Gc.finalise destroy t ;
  Ec_private t

let secret (Ec_private t) =
  let secret = foreign "bc_ec_private__secret"
      ((ptr void) @-> returning (ptr void)) in
  Ec_secret.Ec_secret (secret t)

let encode (Ec_private t) =
  let encode = foreign "bc_ec_private__encoded"
      ((ptr void) @-> returning (ptr void)) in
  Data.String.(to_string (of_ptr (encode t)))

let to_wif = encode
