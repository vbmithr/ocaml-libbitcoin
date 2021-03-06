open Ctypes
open Foreign

module Chunk = struct
  type t = Chunk of unit ptr

  let destroy =
    foreign "bc_destroy_data_chunk"
      (ptr void @-> returning void)

  let create =
    foreign "bc_create_data_chunk"
      (void @-> returning (ptr void))

  let array =
    foreign "bc_create_data_chunk_Array"
      (string @-> size_t @-> returning (ptr void))

  let size =
    foreign "bc_data_chunk__size"
      (ptr void @-> returning int)

  let to_string =
    foreign "bc_data_chunk__data"
      (ptr void @-> returning (ptr char))

  let of_ptr ptr =
    Gc.finalise destroy ptr ;
    Chunk ptr

  let of_ptr_nodestroy ptr = Chunk ptr

  let create () =
    let ret = create () in
    Gc.finalise destroy ret ;
    Chunk ret

  let of_bytes str =
    let chunk = array str
        (Unsigned.Size_t.of_int @@ String.length str) in
    Gc.finalise destroy chunk ;
    Chunk chunk

  let of_hex hex =
    of_bytes (Hex.to_string hex)

  let to_bytes (Chunk t) =
    let str = to_string t in
    let ret = string_from_ptr str ~length:(size t) in
    ret

  let to_hex chunk =
    Hex.of_string (to_bytes chunk)
end

module String = struct
  let of_string =
    foreign "bc_create_string"
      (string @-> returning (ptr void))
  let of_substring =
    foreign "bc_create_string_Length"
      (string @-> size_t @-> returning (ptr void))
  let destroy =
    foreign "bc_destroy_string"
      (ptr void @-> returning void)
  let length =
    foreign "bc_string__length"
      (ptr void @-> returning int)
  let to_string =
    foreign "bc_string__data"
      (ptr void @-> returning (ptr char))

  type t = String of unit ptr

  let of_ptr ptr =
    Gc.finalise destroy ptr ;
    String ptr

  let of_string ?(pos=0) ?len str =
    let len = match len with
      | Some len -> len
      | None -> String.length str - pos in
    if pos < 0 || pos + len > String.length str then invalid_arg "of_string";
    let sub = String.sub str pos len in
    let v = of_string sub in
    Gc.finalise destroy v ;
    String v

  let to_string (String t) =
    let str = to_string t in
    let ret = string_from_ptr str ~length:(length t) in
    ret

  module List = struct
    let create = foreign
        "bc_create_string_list"
        (void @-> returning (ptr void))

    let destroy = foreign "bc_destroy_string_list"
        (ptr void @-> returning void)

    let push_back = foreign "bc_string_list__push_back"
        (ptr void @-> ptr void @-> returning void)

    let size = foreign "bc_string_list__size"
        (ptr void @-> returning int)

    let get_at = foreign "bc_string_list__at"
        (ptr void @-> int @-> returning (ptr void))

    type t = String_list of unit ptr

    let of_list strings =
      let t = create () in
      ListLabels.iter strings ~f:begin fun s ->
        let String s_ptr = of_string s in
        push_back t s_ptr
      end ;
      Gc.finalise destroy t ;
      String_list t

    let to_list (String_list t_ptr) =
      let rec get acc = function
        | n when n < 0 -> acc
        | n -> get (to_string (of_ptr (get_at t_ptr n)) :: acc) (pred n) in
      get [] (pred (size t_ptr))

    let of_ptr ptr =
      Gc.finalise destroy ptr ;
      String_list ptr
  end
end
