open Data
open Terminal
open Row

let editor_rows_to_string () : string =
    let rec editor_rows_to_string' (rows : editor_row array) (index : int) (acc : string) =
        if index = get_numrows () then
            acc
        else
            editor_rows_to_string' rows (index + 1) (acc ^ rows.(index).chars ^ "\n")
    in
    editor_rows_to_string' (get_erow ()) 0 ""
;;

let editor_open (file_name : string) =
    e := Some (filename_free (Option.get !e));
    e := Some { (Option.get !e) with filename = file_name };
    let fp =
        try
            open_in file_name
        with
            Sys_error _ -> die "open_in"
    in
    let rec open_file' (fp : in_channel) =
        try
            let line = input_line fp in
            let line_len = String.length line in
            editor_insert_row (get_numrows()) line line_len;
            open_file' fp
        with
            End_of_file -> ()
    in
    open_file' fp;
    close_in fp;
    e := Some { (Option.get !e) with dirty = 0 }
;;


open Utils
open Data
open Terminal
open Editor
open Buffer
open Output

let prompt_buf : abuf ref = ref { b = ""; len = 0 } ;;

let editor_process_prompt_keypress () : bool =
    let c = editor_read_key () in
    if c != None then
        match c with
        | _ when Option.get c = Char.code '\r' ->
            if !prompt_buf.len != 0 then
                editor_set_status_message "";
                true
        | _ when (not (is_control_char (Option.get c))) && (Option.get c) < 128 ->
            let new_string = String.make 1 (Char.chr (Option.get c)) in
            ab_append prompt_buf new_string (String.length new_string);
            false
        | _ when Option.get c = Char.code '\x1b' ->
            editor_set_status_message "";
            ab_free prompt_buf;
            true
        | _ -> false
    else
        false
;;

let editor_prompt (prompt : string) =
    let rec editor_prompt'(prompt : string) =
        flush stdout;
        editor_set_status_message (prompt ^ !prompt_buf.b);
        editor_refresh_screen ();
        let _ = Printf.printf "%s" (String.make 1 (Char.chr (Char.code '\x1b'))) in
        if editor_process_prompt_keypress () then
            ()
        else
            editor_prompt' prompt;
    in
    editor_prompt' prompt;
;;

let save_file (file_name : string) =
    let buf = editor_rows_to_string () in
    let fp =
        try
            Some ( open_out file_name )
        with
        | Sys_error err ->
            editor_set_status_message (Printf.sprintf "Can't save! I/O error: %s" err);
            None
    in
    output_string (Option.get fp) buf;
    e := Some { (Option.get !e) with dirty = 0 };
    editor_set_status_message (file_name ^ " " ^ (string_of_int (String.length buf)) ^ " bytes written to disk");
    close_out (Option.get fp)
;;

let editor_save () =
    let file_name = get_filename () in
    if file_name = "" then (
        editor_prompt "Save as: %s (ESC to cancel)";
        if !prompt_buf.len == 0 then (
            editor_set_status_message "Save aborted";
        ) else (
            e := Some { (Option.get !e) with filename = !prompt_buf.b };
            save_file !prompt_buf.b;
        )
    ) else
        save_file file_name
;;

