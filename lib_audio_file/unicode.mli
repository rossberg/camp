(* Unicode Helpers *)

val is_ascii : string -> bool
val is_utf8 : string -> bool
val has_utf16_bom : string -> bool

val utf8_length : string -> int

val transcode_utf16 : string -> string
val asciify : string -> string
