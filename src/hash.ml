open Ctypes
open Foreign

module type S = sig
  type t
  val of_bytes_exn : string -> t
  val of_hex_exn : Hex.t -> t

  val of_bytes : string -> t option
  val of_hex : Hex.t -> t option

  val to_bytes : t -> string
  val to_hex : t -> Hex.t
end

module Make (Size : sig val length : int end) = struct
  type t = string

  let of_bytes_exn b =
    if String.length b <> Size.length then
      invalid_arg "of_bytes_exn"
    else b

  let of_hex_exn hex =
    let b = Hex.to_string hex in
    if String.length b <> Size.length then
      invalid_arg "of_bytes_exn"
    else b

  let of_bytes b =
    try Some (of_bytes_exn b) with _ -> None
  let of_hex hex =
    try Some (of_hex_exn hex) with _ -> None

  let to_bytes t = t
  let to_hex t = Hex.of_string t
end

module Hash20 = Make(struct let length = 20 end)
module Hash32 = Make(struct let length = 32 end)

type hash = Hash of unit ptr
type short_hash = Short_hash of unit ptr

let destroy_hash =
  foreign "bc_destroy_hash_digest"
    ((ptr void) @-> returning void)

let hash ptr =
  Gc.finalise destroy_hash ptr ;
  Hash ptr

let hash_of_bytes str =
  if String.length str <> 32 then
    invalid_arg "Hash.hash_of_bytes: input must be 32 bytes long" ;
  let create =
    foreign "bc_create_hash_digest_Array"
      (string @-> returning (ptr void)) in
  let ret = create str in
  Gc.finalise destroy_hash ret ;
  Hash ret

let hash_of_hex hex =
  hash_of_bytes (Hex.to_string hex)

let hash_to_bytes (Hash t) =
  let cdata =
    foreign "bc_hash_digest__cdata"
      (ptr void @-> returning (ptr char)) in
  let str = cdata t in
  let ret = string_from_ptr str ~length:32 in
  ret

let hash_to_hex t =
  Hex.of_string (hash_to_bytes t)

let destroy_short_hash =
  foreign "bc_destroy_short_hash"
    ((ptr void) @-> returning void)

let create =
  foreign "bc_create_short_hash_Array"
    (string @-> returning (ptr void))

let cdata =
  foreign "bc_short_hash__cdata"
    (ptr void @-> returning (ptr char))

let short_hash ptr =
  Gc.finalise destroy_short_hash ptr ;
  Short_hash ptr

let short_hash_of_bytes str =
  if String.length str <> 20 then
    invalid_arg "Hash.short_hash_of_bytes: input must be 20 bytes long" ;
  let ret = create str in
  Gc.finalise destroy_short_hash ret ;
  Short_hash ret

let short_hash_of_hex hex =
  short_hash_of_bytes (Hex.to_string hex)

let short_hash_to_bytes (Short_hash t) =
  let str = cdata t in
  let ret = string_from_ptr str ~length:20 in
  ret

let short_hash_to_hex t =
  Hex.of_string (short_hash_to_bytes t)
