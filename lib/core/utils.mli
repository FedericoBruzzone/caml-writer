(** Utils module for the caml_writer.
    This module contains some utility functions and constants used by the caml_writer.*)

val caml_writer_version : string
(** {e caml_writer_version} is the version of the caml_writer.
    @return [string] the version of the caml_writer.
 *)

val backspace_key : int
(** {e backspace_key} is the key code for the backspace key.
    @return [int] the key code for the backspace key.
 *)

val arrow_left : int
(** {e arrow_left} is the key code for the left arrow key.
    @return [int] the key code for the left arrow key.
 *)

val arrow_right : int
(** {e arrow_right} is the key code for the right arrow key.
    @return [int] the key code for the right arrow key.
 *)

val arrow_up : int
(** {e arrow_up} is the key code for the up arrow key.
    @return [int] the key code for the up arrow key.
 *)

val arrow_down : int
(** {e arrow_down} is the key code for the down arrow key.
    @return [int] the key code for the down arrow key.
 *)

val del_key : int
(** {e del_key} is the key code for the delete key.
    @return [int] the key code for the delete key.
 *)

val home_key : int
(** {e home_key} is the key code for the home key.
    @return [int] the key code for the home key.
 *)

val end_key : int
(** {e end_key} is the key code for the end key.
    @return [int] the key code for the end key.
 *)

val page_up : int
(** {e page_up} is the key code for the page up key.
    @return [int] the key code for the page up key.
 *)

val page_down : int
(** {e page_down} is the key code for the page down key.
    @return [int] the key code for the page down key.
 *)

val is_control_char : int -> bool
(** {e is_control_char} checks if the given character is a control character.
    A control character is a character that is not printable.
    @param [int] the character to check.
    @return [bool] true if the given character is a control character, false otherwise.
 *)

val ctrl_key : char -> int
(** {e ctrl_key} returns the key code for the given character.
    @param [char] the character to get the key code for.
    @return [int] the key code for the given character.
 *)

val caml_writer_tab_stop : int
(** {e caml_writer_tab_stop} is the number of spaces used for a tabulation.
    @return [int] the number of spaces used for a tabulation.
 *)

val caml_writer_quit_times : int
(** {e caml_writer_quit_times} is the number of times the user has to press the quit key to quit the caml_writer.
    @return [int] the number of times the user has to press the quit key to quit the caml_writer.
 *)

val quit_times : int ref
(** {e quit_times} is the number of times the user has pressed the quit key.
    It is modified during the execution of the caml_writer.
    @return [int ref] the number of times the user has pressed the quit key.
 *)
