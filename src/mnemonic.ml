open Ctypes
open Foreign

type dict =
  | English
  | Spanish
  | Japanese
  | Chinese_simplified
  | Chinese_traditional

let dict_en =
  lazy (foreign "bc_dictionary_en" (void @-> returning (ptr void)) ())
let dict_es =
  lazy (foreign "bc_dictionary_es" (void @-> returning (ptr void)) ())
let dict_ja =
  lazy (foreign "bc_dictionary_ja" (void @-> returning (ptr void)) ())
let dict_zh_simplified =
  lazy (foreign "bc_dictionary_zh_Hans" (void @-> returning (ptr void)) ())
let dict_zh_traditional =
  lazy (foreign "bc_dictionary_zh_Hant" (void @-> returning (ptr void)) ())

let dict_ptr_of_dict = function
  | English -> Lazy.force (dict_en)
  | Spanish -> Lazy.force (dict_es)
  | Japanese -> Lazy.force (dict_ja)
  | Chinese_simplified -> Lazy.force (dict_zh_simplified)
  | Chinese_traditional -> Lazy.force (dict_zh_traditional)

let create_mnemonic = foreign "bc_create_mnemonic_Dict"
    (ptr void @-> ptr void @-> returning (ptr void))

let seed_multiple =
  let seed_multiple = foreign "bc_mnemonic_seed_multiple"
      (void @-> returning int) in
  lazy (seed_multiple ())

let word_multiple =
  let word_multiple = foreign "bc_mnemonic_word_multiple"
      (void @-> returning int) in
  lazy (word_multiple ())

let decode = foreign "bc_decode_mnemonic_Passphrase"
    (ptr void @-> ptr void @-> returning (ptr_opt void))

let of_entropy ?(dict=English) entropy =
  let Data.Chunk.Chunk entropy_ptr = Data.Chunk.of_bytes entropy in
  let dict_ptr = dict_ptr_of_dict dict in
  let word_list = create_mnemonic entropy_ptr dict_ptr in
  (Data.String.List.(to_list (of_ptr word_list)))

let to_seed ?(passphrase="") words =
  let open Data.String in
  let String passphrase = of_string passphrase in
  let List.String_list str_list_ptr = Data.String.List.of_list words in
  match decode str_list_ptr passphrase with
  | None -> None
  | Some long_hash_ptr ->
    Some (Hash.(long_hash_of_ptr long_hash_ptr |> long_hash_to_bytes))

let to_seed_exn ?passphrase words =
  match to_seed ?passphrase words with
  | None -> invalid_arg "Mnemonic.to_seed_exn"
  | Some seed -> seed

