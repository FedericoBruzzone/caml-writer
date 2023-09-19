type abuf = { b : string; len : int }

let abuf_init = { b = ""; len = 0 }

let ab_append (ab : abuf ref) (s : string) (len : int) =
  let ab' = { b = !ab.b ^ s; len = !ab.len + len } in
  ab := ab'

let ab_free (ab : abuf ref) = ab := { b = ""; len = 0 }
