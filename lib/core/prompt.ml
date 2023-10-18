open Utils
open Data
open Terminal
open Buffer
open Output

let prompt_buf : abuf ref = ref { b = ""; len = 0 }

let editor_process_prompt_keypress (callback : int -> unit) : bool =
  let c = editor_read_key () in
  if c != None then (
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
        (if callback != ignore then
           let _ = print_endline "1b1b1b" in
           callback (Option.get c));
        ab_free prompt_buf;
        true
    | _ when Option.get c = Char.code '\r' ->
        if !prompt_buf.len != 0 then editor_set_status_message "";
        (if callback != ignore then
           let _ = print_endline "rrr" in
           callback (Option.get c));
        true
    | _ when (not (is_control_char (Option.get c))) && Option.get c < 128 ->
        let new_string = String.make 1 (Char.chr (Option.get c)) in
        ab_append prompt_buf new_string (String.length new_string);
        false
    | _ ->
        (if callback != ignore then
           let _ = print_endline "___" in
           callback (Option.get c));
        false
  ) else
    false

let editor_prompt (prompt : string) (callback : int -> unit) : unit =
  let rec editor_prompt' (prompt : string) =
    flush stdout;
    editor_set_status_message (prompt ^ !prompt_buf.b);
    editor_refresh_screen ();
    if editor_process_prompt_keypress callback then
      ()
    else
      editor_prompt' prompt
  in
  editor_prompt' prompt
