datatype md = TwoInts of int * int
       | Str of string
       | Pizza

(*md -> int *)
fun f (x : md) =
    case x of
        Pizza => 3
     | Str s  => String.size s
     | TwoInts(i1, i2) => i1 + i2

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval (x : exp) =
    case x of
      Constant c =>  c
      | Negate c => ~(eval c)
      | Add (i1, i2) => (eval i1) + (eval i2)
      | Multiply (i1, i2) => (eval i1) * (eval i2)

fun number_of_adds x =
    case x of
        Constant c => 0
      | Negate e => number_of_adds x
      | Add(x1, x2) => 1 + (number_of_adds x1) + (number_of_adds x2)
