open Ctypes
open Foreign

module Ec_uncompressed = struct
  type t = Ec_uncompressed of unit ptr
  let destroy = foreign "bc_destroy_ec_uncompressed"
      (ptr void @-> returning void)

  let of_bytes = foreign "bc_create_ec_uncompressed_Data"
      (string @-> returning (ptr_opt void))

  let of_hex hex =
    match of_bytes (Hex.to_string hex) with
    | None -> None
    | Some t ->
      Gc.finalise destroy t ;
      Some (Ec_uncompressed t)

  let of_bytes bytes =
    match of_bytes bytes with
    | None -> None
    | Some t ->
      Gc.finalise destroy t ;
      Some (Ec_uncompressed t)
end

let of_string = foreign "bc_create_ec_public_String"
    (string @-> returning (ptr void))

let of_private = foreign "bc_create_ec_public_Private"
    ((ptr void) @-> returning (ptr void))

let of_uncompressed_point = foreign "bc_create_ec_public_UncompPoint"
    (ptr void @-> returning (ptr_opt void))

let of_data = foreign "bc_create_ec_public_Data"
    (ptr void @-> returning (ptr_opt void))

let destroy = foreign "bc_destroy_ec_public"
    ((ptr void) @-> returning void)

let encode = foreign "bc_ec_public__encoded"
    ((ptr void) @-> returning (ptr void))

type t = Ec_public of unit ptr

let of_private (Ec_private.Ec_private priv) =
  let ret = of_private priv in
  Gc.finalise destroy ret ;
  Ec_public ret

let of_bytes bytes =
  let Data.Chunk.Chunk chunk = Data.Chunk.of_bytes bytes in
  match of_data chunk with
  | None -> None
  | Some t ->
    Gc.finalise destroy t ;
    Some (Ec_public t)

let of_bytes_exn bytes =
  match of_bytes bytes with
  | None -> invalid_arg "Ec_public.of_bytes_exn"
  | Some t -> t

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

let to_bytes (Ec_public ec_public_ptr) =
  let to_data = foreign "bc_ec_public__to_data"
      (ptr void @-> ptr void @-> returning bool) in
  let open Data.Chunk in
  let (Chunk chunk_ptr as chunk) =
    of_bytes (String.make 32 '\x00') in
  match to_data ec_public_ptr chunk_ptr with
  | true -> to_bytes chunk
  | false -> failwith "Ec_public.to_bytes: internal error"

let to_hex (Ec_public ec_public_ptr) =
  `Hex Data.String.(to_string (of_ptr (encode ec_public_ptr)))

let of_uncomp_point bytes =
  match Ec_uncompressed.of_bytes bytes with
  | None -> None
  | Some (Ec_uncompressed ptr) ->
    match of_uncompressed_point ptr with
    | None -> None
    | Some ptr -> Some (Ec_public ptr)

let of_uncomp_point_exn bytes =
  match of_uncomp_point bytes with
  | None -> invalid_arg "Ec_public.of_uncomp_point_exn"
  | Some t -> t
