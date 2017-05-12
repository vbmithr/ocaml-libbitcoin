open Ctypes
open Foreign

module Header = struct
  type header_ptr = unit ptr

  type t = {
    version : int ;
    prev_block_hash : Hash.Hash32.t ;
    merkle : Hash.Hash32.t ;
    timestamp : Ptime.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
    hash : Hash.Hash32.t ;
    header_ptr : header_ptr ;
  }

  let pp ppf { version ; prev_block_hash ; merkle ; timestamp ;
               bits ; nonce ; hash } =
    Format.fprintf ppf
      "{@[<hov 1> version = %d ;@;prev_block_hash = %a ;@;merkle = %a ;\
       @;timestamp = %a ;@;bits = %ld ;@;nonce = %ld ;\
       @;hash = %a }@]"
      version
      Hash.Hash32.pp prev_block_hash
      Hash.Hash32.pp merkle
      Ptime.pp timestamp
      bits nonce
      Hash.Hash32.pp hash

  let show t =
    Format.asprintf "%a" pp t

  let is_valid = foreign "bc_header__is_valid"
      (ptr void @-> returning bool)

  let version = foreign "bc_header__version"
      (ptr void @-> returning int)

  let prev_block_hash = foreign "bc_header__previous_block_hash"
      (ptr void @-> returning (ptr void))

  let merkle = foreign "bc_header__merkle"
      (ptr void @-> returning (ptr void))

  let timestamp = foreign "bc_header__timestamp"
      (ptr void @-> returning int32_t)
  let bits = foreign "bc_header__bits"
      (ptr void @-> returning int32_t)
  let nonce = foreign "bc_header__nonce"
      (ptr void @-> returning int32_t)

  let hash = foreign "bc_header__hash"
      (ptr void @-> returning (ptr void))

  let from_data = foreign "bc_header__from_data"
      (ptr void @-> returning (ptr_opt void))

  let of_ptr ptr =
    if is_null ptr then None
    else if not (is_valid ptr) then None
    else
      let hash =
        Hash.(Hash32.of_bytes_exn (hash_of_ptr (hash ptr) |> hash_to_bytes)) in
      let merkle =
        Hash.(Hash32.of_bytes_exn (hash_of_ptr (merkle ptr) |> hash_to_bytes)) in
      let prev_block_hash =
        Hash.(Hash32.of_bytes_exn (hash_of_ptr (prev_block_hash ptr) |> hash_to_bytes)) in
      match Ptime.of_float_s (Int32.to_float (timestamp ptr)) with
      | None -> invalid_arg "Ptime.of_float_s"
      | Some timestamp -> Some {
          version = version ptr ;
          hash ;
          merkle ;
          prev_block_hash ;
          bits = bits ptr ;
          nonce = nonce ptr ;
          timestamp ;
          header_ptr = ptr
        }

  let of_ptr_exn ptr =
    match of_ptr ptr with
    | None -> invalid_arg "Block.Header.of_ptr_exn"
    | Some h -> h

  let of_bytes bytes =
    let Data.Chunk.Chunk chunk = Data.Chunk.of_bytes bytes in
    match from_data chunk with
    | None -> None
    | Some ptr -> of_ptr ptr

  let of_bytes_exn bytes =
    match of_bytes bytes with
    | None -> invalid_arg "Block.Header.of_bytes_exn"
    | Some t -> t
end

type block_ptr = unit ptr
type t = {
  header : Header.t ;
  transactions : Transaction.List.t ;
  block_ptr : block_ptr ;
}

let pp ppf { header ; transactions } =
  Format.fprintf ppf
    "{@[<hov 1> header = %a ;@;transactions = [@[<hov 0>%a@]] }@]"
    Header.pp header Transaction.List.pp transactions

let show t =
  Format.asprintf "%a" pp t

let factory_from_data = foreign "bc_block__factory_from_data"
    (ptr void @-> returning (ptr_opt void))

let destroy = foreign "bc_destroy_block"
    (ptr void @-> returning void)

let is_valid = foreign "bc_block__is_valid"
    (ptr void @-> returning bool)

let header = foreign "bc_block__header"
    (ptr void @-> returning (ptr void))

let transactions = foreign "bc_block__transactions"
    (ptr void @-> returning (ptr void))

let of_bytes bytes =
  let Data.Chunk.Chunk chunk = Data.Chunk.of_bytes bytes in
  match factory_from_data chunk with
  | Some ptr ->
    Gc.finalise destroy ptr ;
    let header = Header.of_ptr_exn (header ptr) in
    let transactions = Transaction.List.of_ptr (transactions ptr) in
    Some { header ; transactions ; block_ptr = ptr }
  | None -> None

let of_bytes_exn bytes =
  match of_bytes bytes with
  | None -> invalid_arg "Block.of_bytes_exn"
  | Some b -> b

let of_hex hex = of_bytes (Hex.to_string hex)
let of_hex_exn hex = of_bytes_exn (Hex.to_string hex)
