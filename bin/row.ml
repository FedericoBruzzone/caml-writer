open Utils
open Data

let editor_row_cx_to_rx (row : editor_row) (cx : int) : int =
  let rec editor_row_cx_to_rx' row index cx rx =
    if index = cx then
      rx
    else
      match row.chars.[index] with
      | '\t' ->
          editor_row_cx_to_rx' row (index + 1) cx
            (rx + (caml_writer_tab_stop - (rx mod caml_writer_tab_stop)))
      | _ -> editor_row_cx_to_rx' row (index + 1) cx (rx + 1)
  in
  editor_row_cx_to_rx' row 0 cx 0

let editor_update_row (row : editor_row) : editor_row =
  let tabs = ref 0 in
  for i = 0 to String.length row.chars - 1 do
    if row.chars.[i] = '\t' then tabs := !tabs + 1
  done;
  let rf_row = render_free row in
  let new_render =
    let rec new_render' row index acc =
      if index = String.length row.chars then
        acc
      else
        match row.chars.[index] with
        | '\t' ->
            new_render' row (index + 1)
              (acc
              ^ String.make
                  (caml_writer_tab_stop - (index mod caml_writer_tab_stop))
                  ' ')
        | _ ->
            new_render' row (index + 1) (acc ^ String.make 1 row.chars.[index])
    in
    new_render' rf_row 0 ""
  in
  let updated_row =
    {
      rf_row with
      render = new_render;
      rsize = row.size + (!tabs * (caml_writer_tab_stop - 1));
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
