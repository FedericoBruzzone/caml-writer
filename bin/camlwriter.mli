(* module Caml_writer : sig *)

(* === Utils === *)
val caml_writer_version : string
val caml_writer_tab_stop : int
val caml_writer_quit_times : int

val is_control_char : int -> bool
val ctrl_key : char -> int

val backspace_key : int
val arrow_left    : int
val arrow_right   : int
val arrow_up      : int
val arrow_down    : int
val del_key       : int
val home_key      : int
val end_key       : int
val page_up       : int
val page_down     : int

(* === Data === *)
type editor_row
val render_free : editor_row -> editor_row
val chars_free : editor_row -> editor_row

type editor_config
val quit_times : int ref
val e : (editor_config option ref)

val get_orig_termio : unit -> Unix.terminal_io
val get_screenrows : unit -> int
val get_screencols : unit -> int
val get_cx : unit -> int
val get_cy : unit -> int
val get_numrows : unit -> int
val get_erow : unit -> editor_row array
val get_erow_at : int -> editor_row
val get_erow_chars : int -> string
val get_erow_size : int -> int
val get_erow_render : int -> string
val get_erow_rsize : int -> int
val get_rowoff : unit -> int
val get_coloff : unit -> int
val get_rx : unit -> int
val get_filename : unit -> string
val get_statusmsg : unit -> string
val get_statusmsg_time : unit -> float
val get_dirty : unit -> int

val filename_free : editor_config -> editor_config
val editor_set_status_message : string -> unit

(* === Terminal === *)
val die : string -> unit
val disable_row_mode : Unix.terminal_io -> unit
val enable_row_mode : unit -> unit
val editor_read_key : unit -> int option
val get_cursor_position : unit -> int * int
val get_window_size : unit -> int * int

(* === Row operations === *)
val editor_row_cx_to_rx : editor_row -> int -> int
val editor_update_row : editor_row -> editor_row
val editor_insert_row : int -> string -> int -> unit
val editor_free_row : editor_row -> editor_row
val editor_del_row : int -> unit
val editor_row_insert_char : editor_row -> int -> char -> editor_row
val editor_row_del_char : editor_row -> int -> editor_row
val editor_row_append_string : editor_row -> string -> int -> editor_row

(* === Editor operations === *)
val editor_insert_char : char -> unit
val editor_insert_new_line : unit -> unit
val editor_del_char : unit -> unit

(* === File i/o === *)
val editor_rows_to_string : unit -> string
val editor_open : string -> unit
val editor_save : unit -> unit

(* === Append buffer === *)
type abuf

val abuf_init : abuf
val ab_append : abuf ref -> string -> int -> unit
val ab_free : abuf ref -> unit

(* === Output === *)
val editor_scroll : unit -> unit
val editor_draw_rows : abuf ref -> unit
val editor_draw_status_bar : abuf ref -> unit
val editor_draw_message_bar : abuf ref -> unit
val editor_refresh_screen : unit -> unit

(* === Input === *)
val editor_prompt : string -> abuf ref option
val editor_move_cursor : int -> unit
val editor_process_keypress : unit -> unit

(* === Init === *)
val init_editor : unit -> unit
val loop : unit -> unit
val main : unit -> unit

(* end *)
