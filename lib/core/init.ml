open Data
open Terminal
open Output
open Input

let init_editor () : unit =
  let columns, rows = get_window_size () in
  e :=
    Some
      {
        orig_termio = get_orig_termio ();
        screenrows = rows - 2;
        screencols = columns;
        cx = 0;
        cy = 0;
        erow = Array.make 0 { chars = ""; size = 0; render = ""; rsize = 0 };
        numrows = 0;
        rowoff = 0;
        coloff = 0;
        rx = 0;
        filename = "";
        statusmsg = "";
        statusmsg_time = 0.0;
        dirty = 0;
      }

let check_window_size () : unit =
  let columns, rows = get_window_size () in
  if rows = get_screenrows () - 2 && columns = get_screencols () then
    ()
  else
    e :=
      Some { (Option.get !e) with screenrows = rows - 2; screencols = columns }

let loop () : unit =
  let rec loop' () =
    flush stdout;
    check_window_size ();
    editor_refresh_screen ();
    editor_process_keypress ();
    loop' ()
  in
  editor_set_status_message "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find";
  loop' ()
