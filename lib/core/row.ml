open Utils
open Data

let editor_row_cx_to_rx (row : editor_row) (cx : int) : int =
  let rx = ref 0 in
  for i = 0 to cx - 1 do
    if row.chars.[i] = '\t' then
      rx := !rx + (caml_writer_tab_stop - 1 - (!rx mod caml_writer_tab_stop));
    rx := !rx + 1
  done;
  !rx

let editor_row_rx_to_cx (row : editor_row) (rx : int) : int =
  let cur_rx = ref 0 in
  let cx = ref 0 in
  while !cx < row.size && !cur_rx < rx do
    if row.chars.[!cx] = '\t' then
      cur_rx :=
        !cur_rx + (caml_writer_tab_stop - 1 - (!cur_rx mod caml_writer_tab_stop));
    cur_rx := !cur_rx + 1;
    cx := !cx + 1
  done;
  !cx

let editor_update_row (row : editor_row) : editor_row =
  let tabs = ref 0 in
  for i = 0 to String.length row.chars - 1 do
    if row.chars.[i] = '\t' then tabs := !tabs + 1
  done;
  let rf_row = render_free row in
  let new_render = ref "" in
  let index = ref 0 in
  for j = 0 to row.size - 1 do
    if row.chars.[j] = '\t' then (
      new_render :=
        !new_render ^ " "
        ^ String.make
            (caml_writer_tab_stop - (!index mod caml_writer_tab_stop) - 1)
            ' ';
      index :=
        !index + (caml_writer_tab_stop - (!index mod caml_writer_tab_stop))
    ) else (
      new_render := !new_render ^ String.make 1 row.chars.[j];
      index := !index + 1
    )
  done;
  let updated_row =
    {
      rf_row with
      render = !new_render;
      rsize = !index (*row.size + (!tabs * (caml_writer_tab_stop - 1));*);
    }
  in
  updated_row

let editor_insert_row (at : int) (s : string) (len : int) =
  if at < 0 || at > get_numrows () + 1 then
    ()
  else
    let updated_erow_array =
      Array.make
        (get_numrows () + 1)
        { chars = ""; size = 0; render = ""; rsize = 0 }
    in
    Array.blit (get_erow ()) 0 updated_erow_array 0 at;
    let new_row = { chars = s; size = len; render = ""; rsize = 0 } in
    let updated_row = editor_update_row new_row in
    updated_erow_array.(at) <- updated_row;
    Array.blit (get_erow ()) at updated_erow_array (at + 1) (get_numrows () - at);
    e :=
      Some
        {
          (Option.get !e) with
          erow = updated_erow_array;
          numrows = get_numrows () + 1;
          dirty = get_dirty () + 1;
        }

let editor_free_row (row : editor_row) =
  let new_row = chars_free row in
  let new_row = render_free new_row in
  new_row

let editor_del_row (at : int) =
  if at < 0 || at >= get_numrows () then
    ()
  else
    let updated_erow_array = Array.copy (get_erow ()) in
    updated_erow_array.(at) <- editor_free_row updated_erow_array.(at);
    Array.blit updated_erow_array (at + 1) updated_erow_array at
      (get_numrows () - at - 1);
    e :=
      Some
        {
          (Option.get !e) with
          erow = updated_erow_array;
          numrows = get_numrows () - 1;
          dirty = get_dirty () + 1;
        }

let editor_row_insert_char (row : editor_row) (at : int) (c : char) =
  let at =
    if at < 0 || at > row.size then
      row.size
    else
      at
  in
  let new_row =
    {
      chars =
        String.sub row.chars 0 at ^ String.make 1 c
        ^ String.sub row.chars at (String.length row.chars - at);
      size = row.size + 1;
      render = "";
      rsize = 0;
    }
  in
  let updated_row = editor_update_row new_row in
  e := Some { (Option.get !e) with dirty = get_dirty () + 1 };
  updated_row

let editor_row_del_char (row : editor_row) (at : int) =
  if at < 0 || at >= row.size then
    row
  else
    let new_row =
      {
        chars =
          String.sub row.chars 0 at
          ^ String.sub row.chars (at + 1) (String.length row.chars - at - 1);
        size = row.size - 1;
        render = "";
        rsize = 0;
      }
    in
    let updated_row = editor_update_row new_row in
    e := Some { (Option.get !e) with dirty = get_dirty () + 1 };
    updated_row

let editor_row_append_string (row : editor_row) (s : string) (len : int) =
  let new_row =
    { chars = row.chars ^ s; size = row.size + len; render = ""; rsize = 0 }
  in
  let updated_row = editor_update_row new_row in
  e := Some { (Option.get !e) with dirty = get_dirty () + 1 };
  updated_row
