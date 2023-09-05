open Caml_writer
(* open Unix *)

(* === Utils === *)
let caml_writer_version = "0.0.1" ;;
let caml_writer_tab_stop = 8 ;;

(* === Constants === *)

let is_control_char (* iscntrl() *) (c : int) : bool = c < 32 || c = 127 ;;

let ctrl_key (c : char) = (Char.code c) land 0x1f ;;

let backspace_key = 127 ;;
let arrow_left    = 1000 ;;
let arrow_right   = 1001 ;;
let arrow_up      = 1002 ;;
let arrow_down    = 1003 ;;
let del_key       = 1004 ;;
let home_key      = 1005 ;;
let end_key       = 1006 ;;
let page_up       = 1007 ;;
let page_down     = 1008 ;;

(* === Data === *)
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
}

let e : (editor_config option ref) = ref None ;;

let ( >>> ) (e : 'a option) (f : 'a -> 'b) : 'b =
    match e with
    | None   -> assert false
    | Some x -> f x
;;

let get_orig_termio () : Unix.terminal_io = !e >>> (fun config -> config.orig_termio) ;;
let get_screenrows () : int               = !e >>> (fun config -> config.screenrows) ;;
let get_screencols () : int               = !e >>> (fun config -> config.screencols) ;;
let get_cx () : int                       = !e >>> (fun config -> config.cx) ;;
let get_cy () : int                       = !e >>> (fun config -> config.cy) ;;
let get_numrows () : int                  = !e >>> (fun config -> config.numrows) ;;
let get_erow () : editor_row array        = !e >>> (fun config -> config.erow) ;;
let get_erow_at (i : int) : editor_row    = !e >>> (fun config -> config.erow.(i)) ;;
let get_erow_chars (i : int) : string     = !e >>> (fun config -> config.erow.(i).chars) ;;
let get_erow_size (i : int) : int         = !e >>> (fun config -> config.erow.(i).size) ;;
let get_erow_render (i : int) : string    = !e >>> (fun config -> config.erow.(i).render) ;;
let get_erow_rsize (i : int) : int        = !e >>> (fun config -> config.erow.(i).rsize) ;;
let get_rowoff () : int                   = !e >>> (fun config -> config.rowoff) ;;
let get_coloff () : int                   = !e >>> (fun config -> config.coloff) ;;
let get_rx () : int                       = !e >>> (fun config -> config.rx) ;;
let get_filename () : string              = !e >>> (fun config -> config.filename) ;;
let get_statusmsg () : string             = !e >>> (fun config -> config.statusmsg) ;;

let filename_free (config : editor_config) : editor_config =
    let new_config = { config with filename = "" }
    in new_config
;;

let editor_set_status_message (statusmsg : string) =
    e := Some { (Option.get !e) with
        statusmsg = statusmsg;
        statusmsg_time = Unix.time ()
    };
;;

(* === Terminal === *)
let die (s : string) =
    output_string stdout "\x1b[2J";  (* Clear screen *)
    output_string stdout "\x1b[H";  (* Reposition cursor *)
    Printf.eprintf "%s\r\n" s;     (* Print error message *)
    exit 1
;;

let disable_row_mode (orig_termio : Unix.terminal_io) : unit =
    try
        Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH orig_termio
    with
        Unix.Unix_error (err, func, arg) ->
            die (Printf.sprintf "tcsetattr(%s, %s, %s)"
                    (Unix.error_message err) func arg)
;;

let enable_row_mode () : unit =
    let orig_termio : Unix.terminal_io =
        try
            Unix.tcgetattr Unix.stdin
        with
            Unix.Unix_error (err, func, arg) ->
                die (Printf.sprintf "tcgetattr(%s, %s, %s)"
                        (Unix.error_message err) func arg)
    in
    e := Some {
        orig_termio = orig_termio;
        screenrows  = 0;
        screencols  = 0;
        cx          = 0;
        cy          = 0;
        erow        = Array.make 0 {
            chars = "";
            size  = 0;
            render = "";
            rsize = 0;
        };
        numrows     = 0;
        rowoff      = 0;
        coloff      = 0;
        rx          = 0;
        filename    = "";
        statusmsg   = "";
        statusmsg_time = 0.0;
    };
    at_exit(fun () -> disable_row_mode (get_orig_termio ()));
    let new_termio = { (get_orig_termio ()) with
                         Unix.c_echo   = false; (* Disable print character *)
                         Unix.c_icanon = false; (* Read byte-by-byte, not line-by-line *)
                         Unix.c_isig   = false; (* Disable Ctrl-C (now is 3 byte) and Ctrl-Z (now is 26 byte) *)
                         Unix.c_ixon   = false; (* Disable Ctrl-S (now is 19 byte) and Ctrl-Q (now is 17 byte) *)
                         (* (* Ctrl-V (22 byte) and Ctrl-O (15 byte) are already disable *) *)
                         Unix.c_icrnl  = false; (* Enable Ctrl-M (now is 13 byte) and Enter (now is 13 byte) *)
                         Unix.c_opost  = false; (* Enable output processing *)
                         Unix.c_brkint = false; (* SIGINT signal to be sent to the program *)
                         Unix.c_inpck  = false; (* Enable parity checking *)
                         Unix.c_istrip = false; (* 8th bit of each input byte to be stripped, meaning it will set it to 0. *)
                         Unix.c_csize  = 8;     (* Character size (now is 8 bit) *)
                         Unix.c_vmin   = 0;     (* Minimum number of reaebytes of input needed before input function returns *)
                         Unix.c_vtime  = 1;     (* Maximum amount of time to wait before input function returns *)
                     }
    in
    try
        (*
        The second argument of `Unix.tcsetattr` indicates when the status change takes place:
        immediately (TCSANOW),
        when all pending output has been transmitted (TCSADRAIN), or
        after flushing all input that has been received but not read (TCSAFLUSH).

        TCSADRAIN is recommended when changing the output parameters;
        TCSAFLUSH, when changing the input parameters.
        *)
        Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH new_termio
    with
        Unix.Unix_error (err, func, arg) ->
            die (Printf.sprintf "tcsetattr(%s, %s, %s)"
                    (Unix.error_message err) func arg)
;;

let editor_read_key () : int option =
    let c =
        try
            Some ( input_byte stdin )
        with
            | Sys_blocked_io -> die "input_byte"
            | Sys_error _ -> die "input_byte"
            | End_of_file -> None
    in
    if c = Some (Char.code '\x1b') then
    (* if Char.chr c = '\x1b' then *)
        let c' = input_byte stdin in
        let c'' = input_byte stdin in
        if Char.chr c' = '[' then
            if Char.chr c'' >= '0' && Char.chr c'' <= '9' then
                let c''' = input_byte stdin in
                if Char.chr c''' = '~' then
                    match Char.chr c'' with
                    | '1' -> Some home_key
                    | '3' -> Some del_key
                    | '4' -> Some end_key
                    | '5' -> Some page_up
                    | '6' -> Some page_down
                    | '7' -> Some home_key
                    | '8' -> Some end_key
                    | _ -> Some ( Char.code '\x1b' )
                else
                    Some ( Char.code '\x1b' )
            else
                match Char.chr c'' with
                | 'A' -> Some arrow_up
                | 'B' -> Some arrow_down
                | 'C' -> Some arrow_right
                | 'D' -> Some arrow_left
                | 'H' -> Some home_key
                | 'F' -> Some end_key
                | _   -> Some ( Char.code '\x1b' )
        else if Char.chr c' = 'O' then
                match Char.chr c'' with
                | 'H' -> Some home_key
                | 'F' -> Some end_key
                | _   -> Some ( Char.code '\x1b' )
        else
            Some ( Char.code '\x1b' )
    else
       c
;;

let get_cursor_position () : (int * int) =
    let buf = Bytes.create 32 in
    let rec get_cursor_position' (count : int) =
        if count >= 31 then
            ()
        else
            Bytes.set buf count (input_char stdin);
            if Bytes.get buf count = 'R' then
                ()
            else
                get_cursor_position' (count + 1)
    in
    get_cursor_position' 0;
    Bytes.set buf 32 '\000';
    if Bytes.get buf 0 <> '\x1b' || Bytes.get buf 1 <> '[' then
        (-1,-1)
    else
        let (_, _) = Scanf.sscanf (Bytes.to_string buf) "\x1b[%d;%d" (fun x y -> (x,y)) in
        (0,0)
;;

let get_window_size () : (int * int) =
    let columns = Window_size.get_columns() in
    let rows = Window_size.get_rows() in
    match (columns, rows) with
    | (Some columns, Some rows) -> (columns, rows)
    | _ -> get_cursor_position();
;;

(* === Row operations === *)
let editor_row_cx_to_rx(row : editor_row) (cx : int) : int =
    let rec editor_row_cx_to_rx' row index cx rx =
        if index = cx then
            rx
        else
            match row.chars.[index] with
            | '\t' -> editor_row_cx_to_rx' row (index + 1) cx (rx + (caml_writer_tab_stop - (rx mod caml_writer_tab_stop)))
            | _ -> editor_row_cx_to_rx' row (index + 1) cx (rx + 1)
    in
    editor_row_cx_to_rx' row 0 cx 0
;;

let editor_update_row (row : editor_row) =
    let tabs = ref 0 in
    for i = 0 to String.length row.chars - 1 do
        if row.chars.[i] = '\t' then
            tabs := !tabs + 1
    done;
    let rf_row = render_free (row) in
    let new_render =
        let rec new_render' row index acc =
            if index = String.length row.chars then
                acc
            else
                match row.chars.[index] with
                        | '\t' -> new_render' row (index + 1) (acc ^ (String.make (caml_writer_tab_stop - (index mod caml_writer_tab_stop)) ' '))
                        | _ -> new_render' row (index + 1) (acc ^ (String.make 1 row.chars.[index]))
    in
        new_render' rf_row 0 ""
        in
    let updated_row = { rf_row with
        render = new_render;
        rsize = row.size + (!tabs * (caml_writer_tab_stop - 1));
    }
    in updated_row
;;

let editor_append_row (s : string) (len : int) =
    let row = {
        chars = s;
        size  = len;
        render = "";
        rsize = 0;
    } in
    let updated_row = editor_update_row (row) in
    e := Some { (Option.get !e) with
        erow = Array.append (get_erow ()) [| updated_row |];
        numrows = (get_numrows ()) + 1;
    }
;;

let editor_row_insert_char (row : editor_row) (at : int) (c : char) =
    let at = if at < 0 || at > row.size then row.size else at in
    let new_row = {
        chars = (String.sub row.chars 0 at) ^ (String.make 1 c) ^ (String.sub row.chars at (String.length row.chars - at));
        size  = row.size + 1;
        render = "";
        rsize = 0;
    } in
    let updated_row = editor_update_row (new_row) in
    updated_row
;;

(* === Editor operations === *)
let editor_insert_char (c : char) =
    let cy = get_cy () in
    let cx = get_cx () in
    let erow = get_erow_at cy in
    let new_erow = editor_row_insert_char erow cx c in
    let updated_erow_array = Array.copy (get_erow ()) in
    updated_erow_array.(cy) <- new_erow;
    e := Some { (Option.get !e) with
        erow = updated_erow_array;
        numrows = (get_numrows ()) + 1;
        cx = (get_cx ()) + 1;
    };
;;

(* === File i/o === *)
let editor_rows_to_string () : string =
    let rec editor_rows_to_string' (rows : editor_row array) (index : int) (acc : string) =
        if index = Array.length rows then
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
            editor_append_row line line_len;
            open_file' fp
        with
            End_of_file -> ()
    in
    open_file' fp;
    close_in fp
;;

let editor_save () =
    let file_name = get_filename () in
    if file_name = "" then ()
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
        editor_set_status_message (file_name ^ " " ^ (string_of_int (String.length buf)) ^ " bytes written to disk");
        close_out (Option.get fp)
;;

(* === Append buffer === *)
type abuf = {
    b : string;
    len : int;
}

let abuf_init = {
    b = "";
    len = 0;
}

let ab_append (ab : abuf ref) (s : string) (len : int) =
    let ab' = {
        b = (!ab.b ^ s);
        len = !ab.len + len;
    }
    in
    ab := ab'
;;

let ab_free (ab : abuf ref) =
    ab := {
        b = "";
        len = 0;
    }
;;

(* === Output === *)
let editor_scroll () =
    e := Some { (Option.get !e) with
        rx = editor_row_cx_to_rx (get_erow_at (get_cy ())) (get_cx ());
    };
    match get_cy () with
    | _ when (get_cy ()) < (get_rowoff ()) ->
        e := Some { (Option.get !e) with rowoff = get_cy () }
    | _ when (get_cy ()) >= (get_rowoff ()) + (get_screenrows ()) ->
        e := Some { (Option.get !e) with rowoff = (get_cy ()) - (get_screenrows ()) + 1 }
    | _ -> ();
    match get_rx () with
    | _ when (get_rx ()) < (get_coloff ()) ->
        e := Some { (Option.get !e) with coloff = get_rx () }
    | _ when (get_rx ()) >= (get_coloff ()) + (get_screencols ()) ->
        e := Some { (Option.get !e) with coloff = (get_rx ()) - (get_screencols ()) + 1 }
    | _ -> ()
;;

let editor_draw_rows (ab : abuf ref) =
    for i = 0 to get_screenrows () - 1 do
        let filerow = i + (get_rowoff ()) in
        let _ = match filerow with
        | _ when filerow >= get_numrows () ->
            if get_numrows() = 0 && i = get_screenrows () / 3 then
                let welcome = "Caml Writer -- version " ^ caml_writer_version in
                let welcome_len = if String.length welcome > get_screencols ()
                    then get_screencols ()
                    else String.length welcome in
                let padding = (get_screencols () - welcome_len) / 2 in
                if padding <> 0 then
                    ab_append ab "~" 1;
                for _ = 0 to padding - 1 do
                    ab_append ab " " 1;
                done;
                ab_append ab welcome welcome_len;
            else
                ab_append ab "~" 1;
        | __ when filerow < get_numrows () ->
            let len = match get_erow_rsize filerow - get_coloff () with
                      | _ when get_erow_rsize filerow - get_coloff () > get_screencols () ->
                          get_screencols ()
                      | _ when get_erow_rsize filerow - get_coloff () < 0 ->
                            0
                      | _ -> get_erow_rsize filerow - get_coloff ()
            in
            let sub_row =
                try
                    (String.sub (get_erow_render filerow) (get_coloff ()) len)
                with
                    Invalid_argument _ -> ""
            in
            ab_append ab sub_row len;
        | _ -> assert false
        in
        ab_append ab "\x1b[K" 3;
        (* if i < get_screenrows () - 1 then *)
        ab_append ab "\r\n" 2
    done
;;

let editor_draw_status_bar (ab : abuf ref) =
    ab_append ab "\x1b[7m" 4;
    let status =
        let lines = Printf.sprintf "%d lines" (get_numrows ()) in
        let filename = if (get_filename ()) = "" then "[No Name]" else (get_filename ()) in
        let rstatus = Printf.sprintf "%d/%d - %d/%d" (get_cy () + 1) (get_numrows ()) (get_cx () + 1) (get_erow_size (get_cy ()) + 1) in
        let len = String.length lines + String.length filename + String.length rstatus + 1 in
        if len > get_screencols () then
            lines ^ " " ^ (String.sub filename 0 (get_screencols () - String.length lines - 1))
        else
            lines ^ " " ^ filename ^ (String.make (get_screencols () - len) ' ') ^ rstatus
    in
    ab_append ab status (String.length status);
    ab_append ab "\x1b[m" 3;
    ab_append ab "\r\n" 2
;;

let editor_draw_message_bar (ab : abuf ref) =
    ab_append ab "\x1b[K" 3;
    ab_append ab "\x1b[4m" 4;
    let msglen = String.length (get_statusmsg ()) in
    if msglen > get_screencols () then
        ab_append ab (String.sub (get_statusmsg ()) 0 (get_screencols ())) (get_screencols ())
    else
        ab_append ab (get_statusmsg ()) msglen;
    ab_append ab "\x1b[m" 3;
;;

let editor_refresh_screen () =
    editor_scroll ();
    let ab : abuf ref = ref abuf_init in
    ab_append ab "\x1b[?25l" 6; (* Hide cursor *)
    ab_append ab "\x1b[H" 3;    (* Reposition cursor *)

    editor_draw_rows(ab);
    editor_draw_status_bar(ab);
    editor_draw_message_bar(ab);

    let buf = Printf.sprintf "\x1b[%d;%dH" (get_cy () - get_rowoff() + 1) (get_rx () - get_coloff() + 1) in
    ab_append ab buf (String.length buf);

    (* ab_append ab "\x1b[H" 3;    (* Reposition cursor *) *)
    ab_append ab "\x1b[?25h" 6; (* Show cursor *)
    output_string stdout !ab.b;
    ab_free ab;
;;

(* === Input === *)
let editor_move_cursor c  =
    let row = if (get_cy ()) >= (get_numrows ()) then "" else (get_erow_chars (get_cy ())) in
    let _ = match c with
    | _ when c = arrow_left ->
        if (get_cx ()) <> 0 then
            e := Some { (Option.get !e) with cx = (get_cx ()) - 1 }
        else if (get_cy ()) > 0 then
            e := Some { (Option.get !e) with
                cy = (get_cy ()) - 1;
                cx = (get_erow_size ((get_cy ()) - 1));
            }
    | _ when c = arrow_right ->
        if row <> "" && (get_cx ()) < (String.length row) then
            e := Some { (Option.get !e) with cx = (get_cx ()) + 1 }
        else if (get_cy ()) < (get_numrows () - 1) then
            e := Some { (Option.get !e) with
                cy = (get_cy ()) + 1;
                cx = 0;
            }
    | _ when c = arrow_up ->
        if (get_cy ()) <> 0 then
            e := Some { (Option.get !e) with cy = (get_cy ()) - 1 }
    | _ when c = arrow_down ->
        if (get_cy ()) < (get_numrows () - 1) then
            e := Some { (Option.get !e) with cy = (get_cy ()) + 1 }
    | _ -> ()
    in
    let row = if (get_cy ()) >= (get_numrows ()) then "" else (get_erow_chars (get_cy ())) in
    let rowlen = String.length row in
    if (get_cx ()) > rowlen then
        e := Some { (Option.get !e) with cx = rowlen }
;;

let editor_process_keypress () =
    let c = editor_read_key() in
    let editor_process_keypress' c =
        match c with
        | _ when c = ctrl_key 'q' ->
            output_string stdout "\x1b[2J"; (* Clear screen *)
            output_string stdout "\x1b[H";  (* Reposition cursor *)
            exit 0
        | _ when c = ctrl_key 's' -> editor_save ()
        | _ when c = arrow_left   -> editor_move_cursor c
        | _ when c = arrow_right  -> editor_move_cursor c
        | _ when c = arrow_up     -> editor_move_cursor c
        | _ when c = arrow_down   -> editor_move_cursor c
        | _ when c = home_key     -> e := Some { (Option.get !e) with cx = 0 }
        | _ when c = end_key ->
            if get_cy () < get_numrows () then
                e := Some { (Option.get !e) with cx = (get_erow_size (get_cy ())) }
        | _ when c = page_up ->
            e := Some { (Option.get !e) with cy = (get_rowoff ()) };
            let times = get_screenrows () in
            for _ = 0 to times do
                editor_move_cursor arrow_up
            done
        | _ when c = page_down ->
            if get_rowoff () + get_screenrows () - 1 < get_numrows () then
                e := Some { (Option.get !e) with cy = (get_rowoff ()) + (get_screenrows ()) - 1 }
            else
                e := Some { (Option.get !e) with cy = get_numrows () };
            let times = get_screenrows () in
            for _ = 0 to times do
                editor_move_cursor arrow_down
            done
        | _ when c = Char.code '\r' -> () (* TODO *)
        | _ when c = backspace_key -> () (* TODO *)
        | _ when c = ctrl_key 'h' -> () (* TODO *)
        | _ when c = home_key -> () (* TODO *)
        | _ when c = ctrl_key 'l' -> ()
        | _ when c = Char.code '\x1b' -> ()
        | _ -> editor_insert_char (Char.chr c);
    in
    match c with
    | None -> ()
    | Some c -> editor_process_keypress' c;
;;

(* === Init === *)
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
    }
;;

let loop () : unit =
    let rec loop' () =
        flush stdout;
        (* update rows and columns *)
        editor_refresh_screen();
        editor_process_keypress ();
        loop' ()
    in
    editor_set_status_message "HELP: Ctrl-S = save | Ctrl-Q = quit";
    loop' ();
;;

let main () =
    enable_row_mode ();
    init_editor ();
    if Array.length Sys.argv > 1 then
        editor_open (Sys.argv.(1));
    loop ();
;;

