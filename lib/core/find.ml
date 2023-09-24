open Prompt
open Data

let editor_find () =
    editor_prompt ("(ESC to cancel) Search: ");
    if !prompt_buf.b != "" then
        editor_set_status_message ("Is there a " ^ !prompt_buf.b ^ "?")

