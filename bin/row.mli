open Data

val editor_row_cx_to_rx : editor_row -> int -> int
val editor_update_row : editor_row -> editor_row
val editor_insert_row : int -> string -> int -> unit
val editor_free_row : editor_row -> editor_row
val editor_del_row : int -> unit
val editor_row_insert_char : editor_row -> int -> char -> editor_row
val editor_row_del_char : editor_row -> int -> editor_row
val editor_row_append_string : editor_row -> string -> int -> editor_row
