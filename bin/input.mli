open Buffer

(* val editor_prompt : string -> abuf ref option *)
val editor_move_cursor : int -> unit
val editor_process_keypress : unit -> unit
