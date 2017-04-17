type dict =
  | English
  | Spanish
  | Japanese
  | Chinese_simplified
  | Chinese_traditional

val of_entropy : ?dict:dict -> string -> string list
val to_seed : ?passphrase:string -> string list -> string option
