open Ctypes

type t = private Error of unit ptr

val of_ptr : unit ptr -> t
val message : t -> string option
