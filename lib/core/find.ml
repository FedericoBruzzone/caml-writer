open Prompt
open Buffer
open Row
open Data
open Utils

let editor_find () =
  editor_prompt "(ESC to cancel) Search: ";
  if !prompt_buf.b != "" then (
    try
      for i = 0 to get_numrows () - 1 do
        let row = get_erow_at i in
        let match_idx = strstr row.render !prompt_buf.b in
        if match_idx != None then (
          e :=
            Some
              {
                (Option.get !e) with
                cy = i;
                cx = editor_row_rx_to_cx row (Option.get match_idx);
                rowoff = get_numrows ();
              };
          raise Exit
        )
      done
    with Exit ->
      ();
      ab_free prompt_buf
  )
