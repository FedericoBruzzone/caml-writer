open Caml_writer
(* open Unix *)

(* === Utils === *)
let is_control_char (* iscntrl() *) (c : int) : bool = c < 32 || c = 127 ;;

let ctrl_key (c : char) = (Char.code c) land 0x1f ;;

(* === Data === *)
type editor_config = {
    orig_termio : Unix.terminal_io;
    screenrows  : int;
    screencols  : int;
}

let e : (editor_config option ref) = ref None;;

let get_orig_termio () : Unix.terminal_io =
    match !e with
    | None -> assert false
    | Some config -> config.orig_termio
;;

let get_screenrows () : int =
    match !e with
    | None -> assert false
    | Some config -> config.screenrows
;;

let get_screencols () : int =
    match !e with
    | None -> assert false
    | Some config -> config.screencols
;;

(* === Terminal === *)
let die (s : string) =
    output_string stdout "\x1b[2J"; (* Clear screen *)
    
    output_string stdout "\x1b[H";  (* Reposition cursor *)

    Printf.eprintf "%s\r\n" s;
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
    let config : editor_config = {
        orig_termio = orig_termio;
        screenrows  = -1;        
        screencols  = -1;
    } 
    in
    e := Some config;
    at_exit(fun () -> disable_row_mode (get_orig_termio ()));
    let new_termio = { (get_orig_termio ()) with
                         Unix.c_echo   = false; (* Disable print character *)
                         Unix.c_icanon = false; (* Read byte-by-byte, not line-by-line *)
                         Unix.c_isig   = false; (* Disable Ctrl-C (now is 3 byte) and Ctrl-Z (now is 26 byte) *)
                         Unix.c_ixon   = false; (* Disable Ctrl-S (now is 19 byte) and Ctrl-Q (now is 17 byte) *)
                         (* Ctrl-V (22 byte) and Ctrl-O (15 byte) are already disable *)
                         Unix.c_icrnl  = false; (* Enable Ctrl-M (now is 13 byte) and Enter (now is 13 byte) *)
                         Unix.c_opost  = false; (* Enable output processing *)
                         Unix.c_brkint = false; (* SIGINT signal to be sent to the program *)
                         Unix.c_inpck  = false; (* Enable parity checking *)
                         Unix.c_istrip = false; (* 8th bit of each input byte to be stripped, meaning it will set it to 0. *)
                         Unix.c_csize  = 8;     (* Character size (now is 8 bit) *)
                         Unix.c_vmin   = 0;     (* Minimum number of bytes of input needed before input function returns *)
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

let editor_read_key () : int =
    let c = 
        try 
            input_byte stdin
        with
            | Sys_blocked_io -> die "input_byte"
            | Sys_error _ -> die "input_byte" 
            (* | End_of_file -> exit 0 *)
            | _ -> 0
    in 
    c
;;

let get_cursor_position () : (int * int) =
    output_string stdout "\x1b[6n";
    (* flush stdout; *)
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
    Printf.printf "\r\n position: %c \r\n" (Bytes.get buf 1);
    flush stdout;
    let _ = editor_read_key() in
    (0,0)
;;
        

let get_window_size () : (int * int) =
    let columns = Window_size.get_columns() in
    let rows    = Window_size.get_rows() in
    match (columns, rows) with
    | (Some columns, Some rows) -> (columns, rows)
    | _ -> get_cursor_position();
;;
    
(* === Input === *)
let editor_process_keypress () =
    let c = editor_read_key() in
    let editor_process_keypress' c = 
        match c with
        | _ when c = ctrl_key 'q' -> 
            output_string stdout "\x1b[2J"; (* Clear screen *)
            output_string stdout "\x1b[H";  (* Reposition cursor *)
            exit 0
        | _ -> () (* output_string stdout (string_of_int c) *)
    in
    editor_process_keypress' c;
;;

(* === Output === *)
let editor_draw_rows () =
    for _ = 0 to get_screenrows () do
        output_string stdout "~\r\n"
    done
;;

let editor_refresh_screen () =
    output_string stdout "\x1b[2J"; (* Clear screen *)
    output_string stdout "\x1b[H";  (* Reposition cursor *)

    editor_draw_rows();

    output_string stdout "\x1b[H";  (* Reposition cursor *)
;;

(* === Init === *)
let init_editor () : unit =
    let (columns, rows) = get_window_size() in
    match !e with
    | None -> assert false
    | Some config -> 
        let config' : editor_config = {
            orig_termio = config.orig_termio;
            screenrows  = rows;
            screencols  = columns;
        }
    in
    e := Some config'
;;

let loop () : unit = 
    while true do 
        flush stdout; (* TODO: REMOVE? *)
        editor_refresh_screen();
        editor_process_keypress();
    done
;;

let main () = 
    enable_row_mode();
    init_editor();
    loop ();
;;
