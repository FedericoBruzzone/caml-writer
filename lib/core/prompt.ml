open Utils
open Data
open Terminal
open Buffer
open Output

let prompt_buf : abuf ref = ref { b = ""; len = 0 }

let editor_process_prompt_keypress () : bool =
  let c = editor_read_key () in
  if c != None then
    match c with
    | _
      when Option.get c = del_key
           || Option.get c = ctrl_key 'h'
           || Option.get c = backspace_key ->
        if !prompt_buf.len != 0 then
          prompt_buf :=
            {
              b = String.sub !prompt_buf.b 0 (!prompt_buf.len - 1);
              len = !prompt_buf.len - 1;
            };
        false
    | _ when Option.get c = Char.code '\x1b' ->
        editor_set_status_message "";
        ab_free prompt_buf;
        true
    | _ when Option.get c = Char.code '\r' ->
        if !prompt_buf.len != 0 then editor_set_status_message "";
        true
    | _ when (not (is_control_char (Option.get c))) && Option.get c < 128 ->
        let new_string = String.make 1 (Char.chr (Option.get c)) in
        ab_append prompt_buf new_string (String.length new_string);
        false
    | _ -> false
  else
    false

let editor_prompt (prompt : string) =
  let rec editor_prompt' (prompt : string) =
    flush stdout;
    editor_set_status_message (prompt ^ !prompt_buf.b);
    editor_refresh_screen ();
    let _ = Printf.printf "%s" (String.make 1 (Char.chr (Char.code '\x1b'))) in
    if editor_process_prompt_keypress () then
      ()
    else
      editor_prompt' prompt
  in
  editor_prompt' prompt

