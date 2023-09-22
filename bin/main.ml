open Core

(* open Utils *)
(* open Data *)
open Terminal

(* open Row *)
(* open Editor *)
open FileIO

(* open Buffer *)
(* open Output *)
(* open Input *)
open Init

let main () =
  enable_row_mode ();
  init_editor ();
  if Array.length Sys.argv > 1 then editor_open Sys.argv.(1);
  loop ()

let () = main ()
