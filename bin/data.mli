type editor_row = { chars : string; size : int; render : string; rsize : int }

val render_free : editor_row -> editor_row
val chars_free : editor_row -> editor_row

type editor_config = {
  orig_termio : Unix.terminal_io;
  screenrows : int;
  screencols : int;
  cx : int;
  cy : int;
  erow : editor_row array;
  numrows : int;
  rowoff : int;
  coloff : int;
  rx : int;
  filename : string;
  statusmsg : string;
  statusmsg_time : float;
  dirty : int;
}

val e : editor_config option ref
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
