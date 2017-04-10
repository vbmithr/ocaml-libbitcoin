open Ctypes
open Foreign

let create = foreign "bc_create_error_code"
    (int @-> returning (ptr void))

let destroy = foreign "bc_destroy_error_code"
    (ptr void @-> returning void)

let message = foreign "bc_error_code__message"
    (ptr void @-> returning (ptr void))

let equals = foreign "bc_error_code__equals"
    (ptr void @-> int @-> returning bool)

type t = Error of unit ptr

let of_ptr ptr =
  Gc.finalise destroy ptr ;
  Error ptr

let message (Error t) =
  if equals t 0 then None
  else begin
    let string_str = message t in
    let str = Data.String.of_ptr string_str in
    Some (Data.String.to_string str)
  end
