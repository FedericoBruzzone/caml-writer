open Camlwriterlibs
open Utils
open Data

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
        dirty       = 0;
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
