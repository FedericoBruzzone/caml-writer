let caml_writer_version : string = "0.0.1"
let backspace_key : int = 127
let arrow_left : int = 1000
let arrow_right : int = 1001
let arrow_up : int = 1002
let arrow_down : int = 1003
let del_key : int = 1004
let home_key : int = 1005
let end_key : int = 1006
let page_up : int = 1007
let page_down : int = 1008
let caml_writer_tab_stop : int = 8
let caml_writer_quit_times : int = 3
let quit_times : int ref = ref caml_writer_quit_times
let is_control_char (c : int) : bool = c < 32 || c = 127
let ctrl_key (c : char) : int = Char.code c land 0x1f

let strstr (haystack : string) (needle : string) : int option =
  let rec aux (i : int) : int option =
    if i >= String.length haystack then
      None
    else
      let len = String.length needle in
      if i + len > String.length haystack then
        None
      else
        let sub = String.sub haystack i len in
        if sub = needle then
          Some i
        else
          aux (i + 1)
  in
  aux 0
