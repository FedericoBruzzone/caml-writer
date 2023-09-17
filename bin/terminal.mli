val die : string -> in_channel
val disable_row_mode : Unix.terminal_io -> unit
val enable_row_mode : unit -> unit
val editor_read_key : unit -> int option
val get_cursor_position : unit -> int * int
val get_window_size : unit -> int * int
