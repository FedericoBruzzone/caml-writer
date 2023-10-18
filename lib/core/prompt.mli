open Buffer

val prompt_buf : abuf ref
val editor_prompt : string -> (int -> unit) -> unit
val editor_process_prompt_keypress : (int -> unit) -> bool


