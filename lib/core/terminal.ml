open Windowsize
open Utils
open Data

let die (s : string) =
  (* Clear screen *)
  output_string stdout "\x1b[2J";
  (* Reposition cursor *)
  output_string stdout "\x1b[H";
  Printf.eprintf "%s\r\n" s;
  exit 1

let disable_row_mode (orig_termio : Unix.terminal_io) : unit =
  try Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH orig_termio
  with Unix.Unix_error (err, func, arg) ->
    die
      (Printf.sprintf "tcsetattr(%s, %s, %s)" (Unix.error_message err) func arg)

let enable_row_mode () : unit =
  let orig_termio : Unix.terminal_io =
    try Unix.tcgetattr Unix.stdin
    with Unix.Unix_error (err, func, arg) ->
      die
        (Printf.sprintf "tcgetattr(%s, %s, %s)" (Unix.error_message err) func
           arg)
  in
  e :=
    Some
      {
        orig_termio;
        screenrows = 0;
        screencols = 0;
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
      };
  at_exit (fun () -> disable_row_mode (get_orig_termio ()));
  let new_termio =
    {
      (get_orig_termio ()) with
      Unix.c_echo = false;
      Unix.c_icanon = false;
      Unix.c_isig = false;
      Unix.c_ixon = false;
      Unix.c_icrnl = false;
      Unix.c_opost = false;
      Unix.c_brkint = false;
      Unix.c_inpck = false;
      Unix.c_istrip = false;
      Unix.c_csize = 8;
      Unix.c_vmin = 0;
      Unix.c_vtime = 1;
    }
  in
  try Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH new_termio
  with Unix.Unix_error (err, func, arg) ->
    die
      (Printf.sprintf "tcsetattr(%s, %s, %s)" (Unix.error_message err) func arg)

let editor_read_key () : int option =
  let c =
    try Some (input_byte stdin) with
    | Sys_blocked_io -> die "input_byte"
    | Sys_error _ -> die "input_byte"
    | End_of_file -> None
  in
  if c = Some (Char.code '\x1b') then
    let c' = try input_byte stdin with _ -> Char.code '\x1b' in
    let c'' = try input_byte stdin with _ -> Char.code '\x1b' in
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
          | _ -> Some (Char.code '\x1b')
        else
          Some (Char.code '\x1b')
      else
        match Char.chr c'' with
        | 'A' -> Some arrow_up
        | 'B' -> Some arrow_down
        | 'C' -> Some arrow_right
        | 'D' -> Some arrow_left
        | 'H' -> Some home_key
        | 'F' -> Some end_key
        | _ -> Some (Char.code '\x1b')
    else if Char.chr c' = 'O' then
      match Char.chr c'' with
      | 'H' -> Some home_key
      | 'F' -> Some end_key
      | _ -> Some (Char.code '\x1b')
    else
      Some (Char.code '\x1b')
  else
    c

let get_cursor_position () : int * int =
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
    (-1, -1)
  else
    let _, _ =
      Scanf.sscanf (Bytes.to_string buf) "\x1b[%d;%d" (fun x y -> (x, y))
    in
    (0, 0)

let get_window_size () : int * int =
  let columns = Window_size.get_columns () in
  let rows = Window_size.get_rows () in
  match (columns, rows) with
  | Some columns, Some rows -> (columns, rows)
  | _ -> get_cursor_position ()
