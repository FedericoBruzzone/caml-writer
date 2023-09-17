open Buffer

val editor_scroll : unit -> unit
val editor_draw_rows : abuf ref -> unit
val editor_draw_status_bar : abuf ref -> unit
val editor_draw_message_bar : abuf ref -> unit
val editor_refresh_screen : unit -> unit
