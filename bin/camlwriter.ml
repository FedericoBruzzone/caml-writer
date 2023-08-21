let enable_row_mode () =
    let termio = Unix.tcgetattr Unix.stdin in
    let new_termio = { termio with Unix.c_icanon = false } in
    (*
    The second argument indicates when the status change takes place: 
        immediately (TCSANOW), 
        when all pending output has been transmitted (TCSADRAIN), or 
        after flushing all input that has been received but not read (TCSAFLUSH). 

        TCSADRAIN is recommended when changing the output parameters; 
        TCSAFLUSH, when changing the input parameters.
    *)
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH new_termio
;;

(* Loop with recursion *)
let loop() = 
    let rec loop' () = 
        let c = input_char stdin in
        print_char c;
    loop' () in
    try loop' () with End_of_file -> ()

let loop_while () = 
    while true do
        (* let c = input_char stdin in *)
        (* if c = 'q' then exit 0; *)
        match input_char stdin with
        | 'q' -> exit 0
        | _ -> ()
    done
;;

(* Loop with for *)

let main () = 
    enable_row_mode();
    loop_while();;
