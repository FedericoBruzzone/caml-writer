open Data
open Terminal
open Output
open Input

let init_editor () : unit =
    let (columns, rows) = get_window_size() in
    e := Some {
        orig_termio = get_orig_termio ();
        screenrows  = rows - 2;
        screencols  = columns;
        cx          = 0;
        cy          = 0;
        erow        = Array.make 0 {
            chars  = "";
            size   = 0;
            render = "";
            rsize  = 0;
        };
        numrows     = 0;
        rowoff      = 0;
        coloff      = 0;
        rx          = 0;
        filename    = "";
        statusmsg   = "";
        statusmsg_time = 0.0;
        dirty       = 0;
    }
;;

let loop () : unit =
    let rec loop' () =
        flush stdout;
        editor_refresh_screen();
        editor_process_keypress ();
        loop' ()
    in
    editor_set_status_message "HELP: Ctrl-S = save | Ctrl-Q = quit";
    loop' ();
;;
