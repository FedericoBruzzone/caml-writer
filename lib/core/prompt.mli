open Buffer

val prompt_buf : abuf ref
val editor_prompt : string -> unit
val editor_process_prompt_keypress : unit -> bool
