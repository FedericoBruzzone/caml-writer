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

let editor_save () =
    let file_name = get_filename () in
    if file_name = "" then
        let new_file_name = "TEST" in
        (* let new_file_name = editor_prompt "Save as: %s (ESC to cancel)" in *)
        if new_file_name = "" then
            editor_set_status_message "Save aborted"
        else
            e := Some { (Option.get !e) with filename = new_file_name }
    else
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

