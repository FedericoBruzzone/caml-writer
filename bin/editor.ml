open Data
open Row

let editor_insert_char (c : char) =
    if (get_cy ()) = (get_numrows ()) then
        let _ = editor_insert_row (get_numrows ()) (String.make 1 c) 1 in
        e := Some { (Option.get !e) with
            cx = (get_cx ()) + 1;
        }
    else
        let new_erow = editor_row_insert_char (get_erow_at (get_cy ())) (get_cx ()) c in
        let updated_erow_array = Array.copy (get_erow ()) in
        updated_erow_array.(get_cy ()) <- new_erow;
        e := Some { (Option.get !e) with
            erow = updated_erow_array;
            numrows = (get_numrows ());
            cx = (get_cx ()) + 1;
        };
;;

let editor_insert_new_line () =
    if (get_cx ()) = 0 then (
        let _ = editor_insert_row (get_cy ()) "" 0 in
        e := Some { (Option.get !e) with
            cy = (get_cy ()) + 1;
            cx = 0;
        }
    )
    else
        let row = get_erow_at (get_cy ()) in
        let _ = editor_insert_row ((get_cy ()) + 1) (String.sub row.chars (get_cx ()) (row.size - (get_cx ()))) (row.size - (get_cx ())) in
        let remaining_row = String.sub row.chars 0 (get_cx ()) in
        let new_erow = {
            chars = remaining_row;
            size  = String.length remaining_row;
            render = "";
            rsize = 0;
        } in
        let updated_new_row = editor_update_row (new_erow) in
        let updated_erow_array = Array.copy (get_erow ()) in
        updated_erow_array.(get_cy ()) <- updated_new_row;
        e := Some { (Option.get !e) with
            erow = updated_erow_array;
            numrows = (get_numrows ());
            cy = (get_cy ()) + 1;
            cx = 0;
        }
;;

let editor_del_char () =
    if (get_cy ()) = (get_numrows ()) then
        ()
    else if (get_cx ()) = 0 && (get_cy ()) = 0 then
        ()
    else if (get_cx ()) > 0 then
        let new_erow = editor_row_del_char (get_erow_at (get_cy ())) (get_cx () - 1) in
        let updated_erow_array = Array.copy (get_erow ()) in
        updated_erow_array.(get_cy ()) <- new_erow;
        e := Some { (Option.get !e) with
            erow = updated_erow_array;
            numrows = (get_numrows ());
            cx = (get_cx ()) - 1;
        };
    else
        let new_row = editor_row_append_string (get_erow_at ((get_cy ()) - 1)) (get_erow_chars (get_cy ())) (get_erow_size (get_cy ())) in
        let updated_erow_array = Array.copy (get_erow ()) in
        updated_erow_array.(get_cy () - 1) <- new_row;
        e := Some { (Option.get !e) with
            erow = updated_erow_array;
            numrows = (get_numrows ());
            cy = (get_cy ()) - 1;
            cx = (get_erow_size ((get_cy ()) - 1));
        };
        editor_del_row ((get_cy ()) + 1);
;;

