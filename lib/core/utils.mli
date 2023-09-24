(** Utils module. *)

val caml_writer_version : string
(** [caml_writer_version] is the version of the caml_writer.
    @return [string] the version of the caml_writer.
 *)

val backspace_key : int
(** [backspace_key] is the key code for the backspace key.
    @return [int] the key code for the backspace key.
 *)

val arrow_left : int
(** [arrow_left] is the key code for the left arrow key.
    @return [int] the key code for the left arrow key.
 *)

val arrow_right : int
(** [arrow_right] is the key code for the right arrow key.
    @return [int] the key code for the right arrow key.
 *)

val arrow_up : int
(** [arrow_up] is the key code for the up arrow key.
    @return [int] the key code for the up arrow key.
 *)

val arrow_down : int
(** [arrow_down] is the key code for the down arrow key.
    @return [int] the key code for the down arrow key.
 *)

val del_key : int
(** [del_key] is the key code for the delete key.
    @return [int] the key code for the delete key.
 *)

val home_key : int
(** [home_key] is the key code for the home key.
    @return [int] the key code for the home key.
 *)

val end_key : int
(** [end_key] is the key code for the end key.
    @return [int] the key code for the end key.
 *)

val page_up : int
(** [page_up] is the key code for the page up key.
    @return [int] the key code for the page up key.
 *)

val page_down : int
(** [page_down] is the key code for the page down key.
    @return [int] the key code for the page down key.
 *)

val caml_writer_tab_stop : int
(** [caml_writer_tab_stop] is the number of spaces used for a tabulation.
    @return [int] the number of spaces used for a tabulation.
 *)

val caml_writer_quit_times : int
(** [caml_writer_quit_times] is the number of times the user has to press the quit key to quit the caml_writer.
    @return [int] the number of times the user has to press the quit key to quit the caml_writer.
 *)

val quit_times : int ref
(** [quit_times] is the number of times the user has pressed the quit key.
    It is modified during the execution of the caml_writer.
    @return [int ref] the number of times the user has pressed the quit key.
 *)

val is_control_char : int -> bool
(** [is_control_char] checks if the given character is a control character.
    A control character is a character that is not printable.
    @param [int] the character to check.
    @return [bool] true if the given character is a control character, false otherwise.
 *)

val ctrl_key : char -> int
(** [ctrl_key] returns the key code for the given character.
    @param [char] the character to get the key code for.
    @return [int] the key code for the given character.
 *)

val strstr : string -> string -> int option
(** [strstr] finds the first occurrence of the given string in the given string.
    @param [string] the string to search in.
    @param [string] the string to search for.
    @return [int option] if the given string is found, the index of the first character of the given string in the given string, None otherwise.
 *)

