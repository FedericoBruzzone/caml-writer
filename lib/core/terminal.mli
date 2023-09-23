val die : string -> in_channel
(** [die] prints an error message and exits the program.

    It clears the screen sending the escape sequence "\x1b\[2J" to the
    terminal and then repositions the cursor to the top left corner sending the
    escape sequence "\x1b\[H" to the terminal. Thus, the error message is the
    only thing printed on the screen.
    Exit code 1 indicates that the program exited with an error.

    @param s the error message to be printed.
    @return the exit code 1.
*)

val disable_row_mode : Unix.terminal_io -> unit
(** [disable_row_mode] disable raw mode for the terminal.

    Essentially, it just calls {b tcsetattr} function provided by {b Unix}
    module with the original {b termios} structure saved by {b enable_row_mode}.

    {b at_exit} is a special function provided by OCaml that takes a function as an
    argument and calls it when the program exits. This is useful for restoring
    the terminal to its original state when the program exits.

    @param termios the original {b termios} structure saved by
           {b enable_row_mode}.
*)

val enable_row_mode : unit -> unit
(** [enable_row_mode] enables raw mode for the terminal.

    It first saves the original {b termios} structure using {b tcgetattr}
    function provided by {b Unix} module.

    To enable raw mode, it needs to turn off a few flags in the terminal.
    {b Unix} module in OCaml provides a function to do this, in particular
    [enable_row_mode] function turns off the following flags:
    - {b c_echo} flag: this flag causes each key you type to be printed to the
      terminal. In raw mode, this flag is turned off.
    - {b c_icanon} flag: this flag causes input to be processed line-by-line.
      In raw mode, this flag is turned off, so input is processed byte-by-byte.
    - {b c_isig} flag: this flag causes Ctrl-C and Ctrl-Z to be interpreted as
      signals and not as control characters. In raw mode, this flag is turned
      off, so Ctrl-C and Ctrl-Z can be read as bytes, specifically 3 and 26.
    - {b c_ixon} flag: this flag causes Ctrl-S and Ctrl-Q to be interpreted as
      signals and not as control characters. In raw mode, this flag is turned
      off, so Ctrl-S and Ctrl-Q can be read as bytes, specifically 19 and 17.
    - {b c_icrnl} flag: this flag causes carriage return (\r) to be interpreted
      as a newline character (\n). In raw mode, this flag is turned off, so
      carriage return (\r) can be read as a byte, specifically 13. It also
      ensures that Ctrl-M and Enter are read as the same byte.
    - {b c_opost} flag: this flag causes output to be processed byte-by-byte.
      In raw mode, this flag is turned off, so output is processed
      line-by-line. Basically, the (\n) and (\r\n) translation is the only output
      processing featre turned on by default.
    - {b c_brkint} flag: this flag causes a break condition to cause a SIGINT
      signal to be sent to the program. In raw mode, this flag is turned off.
    - {b c_inpck} flag: this flag enables parity checking. In raw mode, this
      flag is turned off.
    - {b c_istrip} flag: this flag causes the 8th bit of each input byte to be
      stripped. In raw mode, this flag is turned off.
    - {b c_csize} value: this value specifies the character size. In raw mode,
      this value is set to 8 bits per byte.
    - {b c_vmin} value: this value specifies the minimum number of bytes of
      input needed before read function can return. In raw mode, this value is
      set to 0, so read function can return as soon as there is any input to be
      read.
    - {b c_vtime} value: this value specifies the maximum amount of time to
      wait before read function can return. In raw mode, this value is set to
      1/10 of a second (100 milliseconds), so read function can return as soon
      as there is any input to be read.

    Note that Ctrl-V and Ctrl-O are already interpreted as control characters,
    specifically 22 and 15, so we don't need to turn off any flags to read them
    as bytes.

    In order to turn off these flags, we need to use {b tcsetattr} function
    provided by {b Unix} module. This function takes a file descriptor, a flag
    specifying which flags to change, and a {b termios} structure. The second
    argument indicates when the status change takes place:
    - {b TCSANOW}: the change takes place immediately.
    - {b TCSADRAIN}: the change takes place after all output written to the
      file descriptor has been transmitted.
    - {b TCSAFLUSH}: the change takes place when all input has been received but
      not read yet. [default]
*)

val editor_read_key : unit -> int option
(** [editor_read_key] reads a keypress from the terminal.

    It is responsable for reading a keypress from the terminal and eventually
    checking if it is an escape sequence. If it is an escape sequence, it reads
    the next two bytes and interprets them returning the corresponding key.
    Otherwise, it returns the key as an integer.

    @return the key as an integer.
*)

val get_cursor_position : unit -> int * int
(** [get_cursor_position] gets the cursor position.

    It sends the escape sequence "\x1b\[6n" to the terminal and then reads the
    response from the terminal. The response is a string of the form
    "\x1b\[\{row\};\{column\}R" where \{row\} and \{column\} are the cursor position.

    @return the cursor position as a pair of integers.
*)

val get_window_size : unit -> int * int
(** [get_window_size] gets the window size.

    It use the {b windowsize} module to get the window size.

    @return the window size as a pair of integers.
*)
