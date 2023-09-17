type editor_row = {
    chars  : string;
    size   : int;
    render : string;
    rsize  : int;
}

let render_free (row : editor_row) : editor_row =
    let new_row = { row with render = "" }
    in new_row
;;

let chars_free (row : editor_row) : editor_row =
    let new_row = { row with chars = "" }
    in new_row
;;

type editor_config = {
    orig_termio    : Unix.terminal_io;
    screenrows     : int;
    screencols     : int;
    cx             : int;
    cy             : int;
    erow           : editor_row array;
    numrows        : int;
    rowoff         : int;
    coloff         : int;
    rx             : int;
    filename       : string;
    statusmsg      : string;
    statusmsg_time : float;
    dirty          : int;
}

let e : (editor_config option ref) = ref None ;;

let get_orig_termio () : Unix.terminal_io = (Option.get !e).orig_termio ;;
let get_screenrows () : int               = (Option.get !e).screenrows ;;
let get_screencols () : int               = (Option.get !e).screencols ;;
let get_cx () : int                       = (Option.get !e).cx ;;
let get_cy () : int                       = (Option.get !e).cy ;;
let get_numrows () : int                  = (Option.get !e).numrows ;;
let get_erow () : editor_row array        = (Option.get !e).erow ;;
let get_erow_at (i : int) : editor_row =
    if i < 0 || i >= (get_numrows ()) then { chars = ""; size = 0; render = ""; rsize = 0; }
    else (Option.get !e).erow.(i)
;;
let get_erow_chars (i : int) : string =
    if i < 0 || i >= (get_numrows ()) then ""
    else (Option.get !e).erow.(i).chars
;;
let get_erow_size (i : int) : int =
    if i < 0 || i >= (get_numrows ()) then 0
    else (Option.get !e).erow.(i).size
;;
let get_erow_render (i : int) : string =
    if i < 0 || i >= (get_numrows ()) then ""
    else (Option.get !e).erow.(i).render
;;
let get_erow_rsize (i : int) : int =
    if i < 0 || i >= (get_numrows ()) then 0
    else (Option.get !e).erow.(i).rsize
;;
let get_rowoff () : int           = (Option.get !e).rowoff ;;
let get_coloff () : int           = (Option.get !e).coloff ;;
let get_rx () : int               = (Option.get !e).rx ;;
let get_filename () : string      = (Option.get !e).filename ;;
let get_statusmsg () : string     = (Option.get !e).statusmsg ;;
let get_statusmsg_time () : float = (Option.get !e).statusmsg_time ;;
let get_dirty () : int            = (Option.get !e).dirty ;;

let filename_free (config : editor_config) : editor_config =
    let new_config = { config with filename = "" }
    in new_config
;;

let editor_set_status_message (statusmsg : string) : unit =
    e := Some { (Option.get !e) with
        statusmsg = statusmsg;
        statusmsg_time = Unix.time ()
    };
;;
