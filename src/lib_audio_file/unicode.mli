(* Unicode Helpers *)

val is_ascii : string -> bool
val is_utf8 : string -> bool
val has_utf16_bom : string -> bool

val utf8_length : string -> int

val transcode_utf16 : string -> string
val asciify : string -> string

val sort_key_utf_8 : string -> string
val compare_utf_8 : string -> string -> int

val search_key_utf_8 : string -> string
val contains_utf_8 : inner: string -> string -> bool
val contains_utf_8_caseless : inner: string -> string -> bool
