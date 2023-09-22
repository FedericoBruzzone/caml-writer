open Utils
open Data
open Row
open Buffer

let editor_scroll () =
  e :=
    Some
      {
        (Option.get !e) with
        rx = editor_row_cx_to_rx (get_erow_at (get_cy ())) (get_cx ());
      };
  match get_cy () with
  | _ when get_cy () < get_rowoff () ->
      e := Some { (Option.get !e) with rowoff = get_cy () }
  | _ when get_cy () >= get_rowoff () + get_screenrows () ->
      e :=
        Some { (Option.get !e) with rowoff = get_cy () - get_screenrows () + 1 }
  | _ -> (
      match get_rx () with
      | _ when get_rx () < get_coloff () ->
          e := Some { (Option.get !e) with coloff = get_rx () }
      | _ when get_rx () >= get_coloff () + get_screencols () ->
          e :=
            Some
              {
                (Option.get !e) with
                coloff = get_rx () - get_screencols () + 1;
              }
      | _ -> ())

let editor_draw_rows (ab : abuf ref) =
  for i = 0 to get_screenrows () - 1 do
    let filerow = i + get_rowoff () in
    let _ =
      match filerow with
      | _ when filerow >= get_numrows () ->
          if get_numrows () = 0 && i = get_screenrows () / 3 then (
            let welcome = "Caml Writer -- version " ^ caml_writer_version in
            let welcome_len =
              if String.length welcome > get_screencols () then
                get_screencols ()
              else
                String.length welcome
            in
            let padding = (get_screencols () - welcome_len) / 2 in
            if padding <> 0 then ab_append ab "~" 1;
            for _ = 0 to padding - 1 do
              ab_append ab " " 1
            done;
            ab_append ab welcome welcome_len
          ) else
            ab_append ab "~" 1
      | _ when filerow < get_numrows () ->
          let len =
            match get_erow_rsize filerow - get_coloff () with
            | _ when get_erow_rsize filerow - get_coloff () > get_screencols ()
              ->
                get_screencols ()
            | _ when get_erow_rsize filerow - get_coloff () < 0 -> 0
            | _ -> get_erow_rsize filerow - get_coloff ()
          in

          (* let sub_row = String.sub (get_erow_render filerow) (get_coloff ()) (len) *)
          (* in *)
          (* ab_append ab sub_row len *)

          (* let sub_row = ref "" in *)
          (* let erow_render = get_erow_render filerow in *)
          (* for i = get_coloff () to len - 1 do *)
          (*   sub_row := !sub_row ^ String.make 1 erow_render.[i] *)
          (* done; *)
          (* ab_append ab !sub_row len *)

          (* let _ = print_endline (string_of_int (get_rx ())) in *)
          let sub_row = get_erow_render filerow in
          ab_append ab sub_row (len - 1)
      | _ -> assert false
    in
    ab_append ab "\x1b[K" 3;
    ab_append ab "\r\n" 2
  done

let editor_draw_status_bar (ab : abuf ref) =
  ab_append ab "\x1b[7m" 4;
  let status =
    let lines = Printf.sprintf "%d lines" (get_numrows ()) in
    let filename =
      if get_filename () = "" then
        "[No Name]"
      else
        get_filename ()
    in
    let dirty =
      if get_dirty () <> 0 then
        "(modified)"
      else
        ""
    in
    let rstatus =
      Printf.sprintf "%d/%d - %d/%d"
        (get_cy () + 1)
        (get_numrows ())
        (get_cx () + 1)
        (get_erow_size (get_cy ()) + 1)
    in
    let len =
      String.length lines
      + (String.length filename + 1)
      + (String.length dirty + 1)
      + String.length rstatus
    in
    if len > get_screencols () then
      lines ^ " "
      ^ String.sub filename 0 (get_screencols () - String.length lines - 1)
    else
      lines ^ " " ^ filename ^ " " ^ dirty
      ^ String.make (get_screencols () - len) ' '
      ^ rstatus
  in
  ab_append ab status (String.length status);
  ab_append ab "\x1b[m" 3;
  ab_append ab "\r\n" 2

let editor_draw_message_bar (ab : abuf ref) =
  ab_append ab "\x1b[K" 3;
  ab_append ab "\x1b[4m" 4;
  let msglen = String.length (get_statusmsg ()) in
  if msglen > get_screencols () then
    ab_append ab
      (String.sub (get_statusmsg ()) 0 (get_screencols ()))
      (get_screencols ())
  else
    ab_append ab (get_statusmsg ()) msglen;
  ab_append ab "\x1b[m" 3

let editor_refresh_screen () =
  editor_scroll ();
  let ab : abuf ref = ref abuf_init in
  (* Hide cursor *)
  ab_append ab "\x1b[?25l" 6;
  (* Reposition cursor *)
  ab_append ab "\x1b[H" 3;
  editor_draw_rows ab;
  editor_draw_status_bar ab;
  editor_draw_message_bar ab;

  let buf =
    Printf.sprintf "\x1b[%d;%dH"
      (get_cy () - get_rowoff () + 1)
      (get_rx () - get_coloff () + 1)
  in
  ab_append ab buf (String.length buf);

  (* Show cursor *)
  ab_append ab "\x1b[?25h" 6;
  output_string stdout !ab.b;
  ab_free ab
