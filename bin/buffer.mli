type abuf = {
    b : string;
    len : int;
}

val abuf_init : abuf
val ab_append : abuf ref -> string -> int -> unit
val ab_free : abuf ref -> unit
