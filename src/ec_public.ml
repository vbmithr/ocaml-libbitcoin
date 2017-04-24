open Ctypes
open Foreign

let of_string = foreign "bc_create_ec_public_String"
    (string @-> returning (ptr void))

let of_private = foreign "bc_create_ec_public_Private"
    ((ptr void) @-> returning (ptr void))

let destroy = foreign "bc_destroy_ec_public"
    ((ptr void) @-> returning void)

let encode = foreign "bc_ec_public__encoded"
    ((ptr void) @-> returning (ptr void))

type t = Ec_public of unit ptr

let of_private (Ec_private.Ec_private priv) =
  let ret = of_private priv in
  Gc.finalise destroy ret ;
  Ec_public ret

let of_hex (`Hex hex) =
  let ret = of_string hex in
  if ptr_compare ret null = 0 then None
  else begin
    Gc.finalise destroy ret ;
    Some (Ec_public ret)
  end

let of_hex_exn hex =
  match of_hex hex with
  | None -> invalid_arg "Ec_public.of_hex_exn"
  | Some t -> t

let encode (Ec_public ec_public_ptr) =
  `Hex Data.String.(to_string (of_ptr (encode ec_public_ptr)))

let to_bytes (Ec_public ec_public_ptr) =
  let to_data = foreign "bc_ec_public__to_data"
      (ptr void @-> ptr void @-> returning bool) in
  let open Data.Chunk in
  let (Chunk chunk_ptr as chunk) =
    of_bytes (String.make 32 '\x00') in
  match to_data ec_public_ptr chunk_ptr with
  | true -> to_bytes chunk
  | false -> failwith "Ec_public.to_bytes: internal error"
