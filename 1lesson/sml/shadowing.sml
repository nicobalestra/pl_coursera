val a = 10

val b = a * 2
val a = 5 (* this is not an assignment *)

(* a->5  b-> 20 *)
val c = b

(* a->5  b-> 20  c->20*)

val d = a
(* ... d => 5 *)
val a = a + 1

(* we get 6 and get a new variable in the environment a -> 6 *)

val f = a * 2
(* f -> 12 *)
