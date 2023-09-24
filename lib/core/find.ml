open Prompt
open Buffer
open Row
open Data
open Utils

let editor_find () =
  editor_prompt "(ESC to cancel) Search: ";
  if !prompt_buf.b != "" then
    (* editor_set_status_message ("Is there a " ^ !prompt_buf.b ^ "?") *)
    for i = 0 to get_numrows () - 1 do
      let row = get_erow_at i in
      let match_idx = strstr row.render !prompt_buf.b in
      if match_idx != None then e := Some { (Option.get !e) with
        cy = i;
        cx = editor_row_rx_to_cx row (Option.get match_idx);
        rowoff = get_numrows ()
      }
    done;
  ab_free prompt_buf;
