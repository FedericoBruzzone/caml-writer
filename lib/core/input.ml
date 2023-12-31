open Utils
open Data
open Terminal
open FileIO
open Editor
open Find

let editor_move_cursor c =
  let row =
    if get_cy () >= get_numrows () then
      ""
    else
      get_erow_chars (get_cy ())
  in
  let _ =
    match c with
    | _ when c = arrow_left ->
        if get_cx () <> 0 then
          e := Some { (Option.get !e) with cx = get_cx () - 1 }
        else if get_cy () > 0 then
          e :=
            Some
              {
                (Option.get !e) with
                cy = get_cy () - 1;
                cx = get_erow_size (get_cy () - 1);
              }
    | _ when c = arrow_right ->
        if row <> "" && get_cx () < String.length row then
          e := Some { (Option.get !e) with cx = get_cx () + 1 }
        else if get_cy () < get_numrows () - 1 then
          e := Some { (Option.get !e) with cy = get_cy () + 1; cx = 0 }
    | _ when c = arrow_up ->
        if get_cy () <> 0 then
          e := Some { (Option.get !e) with cy = get_cy () - 1 }
    | _ when c = arrow_down ->
        if get_cy () < get_numrows () - 1 then
          e := Some { (Option.get !e) with cy = get_cy () + 1 }
    | _ -> ()
  in
  let row =
    if get_cy () >= get_numrows () then
      ""
    else
      get_erow_chars (get_cy ())
  in
  let rowlen = String.length row in
  if get_cx () > rowlen then e := Some { (Option.get !e) with cx = rowlen }

let editor_process_keypress () =
  let c = editor_read_key () in
  let editor_process_keypress' c =
    match c with
    | _ when c = ctrl_key 'q' ->
        let check_exit =
          if get_dirty () <> 0 && !quit_times > 0 then (
            editor_set_status_message
              (Printf.sprintf
                 "WARNING!!! File has unsaved changes. Press Ctrl-Q %d more \
                  times to quit."
                 !quit_times);
            decr quit_times;
            true
          ) else
            false
        in
        if check_exit then
          ()
        else (
          (* Clear screen *)
          output_string stdout "\x1b[2J";
          (* Reposition cursor *)
          output_string stdout "\x1b[H";
          exit 0
        )
    | _ when c = ctrl_key 'f' -> editor_find ()
    | _ when c = ctrl_key 's' -> editor_save ()
    | _ when c = arrow_left -> editor_move_cursor c
    | _ when c = arrow_right -> editor_move_cursor c
    | _ when c = arrow_up -> editor_move_cursor c
    | _ when c = arrow_down -> editor_move_cursor c
    | _ when c = home_key -> e := Some { (Option.get !e) with cx = 0 }
    | _ when c = end_key ->
        if get_cy () < get_numrows () then
          e := Some { (Option.get !e) with cx = get_erow_size (get_cy ()) }
    | _ when c = page_up ->
        e := Some { (Option.get !e) with cy = get_rowoff () };
        let times = get_screenrows () in
        for _ = 0 to times do
          editor_move_cursor arrow_up
        done
    | _ when c = page_down ->
        if get_rowoff () + get_screenrows () - 1 < get_numrows () then
          e :=
            Some
              {
                (Option.get !e) with
                cy = get_rowoff () + get_screenrows () - 1;
              }
        else
          e := Some { (Option.get !e) with cy = get_numrows () - 1 };
        let times = get_screenrows () in
        for _ = 0 to times do
          editor_move_cursor arrow_down
        done
    | _ when c = Char.code '\r' -> editor_insert_new_line ()
    | _ when c = backspace_key ->
        editor_move_cursor arrow_right;
        editor_del_char ()
    | _ when c = ctrl_key 'h' ->
        editor_move_cursor arrow_right;
        editor_del_char ()
    | _ when c = del_key ->
        editor_move_cursor arrow_right;
        editor_del_char ()
    | _ when c = ctrl_key 'l' -> ()
    | _ when c = Char.code '\x1b' -> ()
    | _ ->
        editor_insert_char (Char.chr c);
        quit_times := caml_writer_quit_times
  in
  match c with
  | None -> ()
  | Some c ->
      editor_process_keypress' c
