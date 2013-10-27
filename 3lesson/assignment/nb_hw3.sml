(* Nico Balestra - Coursera Programming Languages, Homework 3 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* Problem 1
   Given a list of strings, return a list of all strings starting by capital letter*)
fun only_capitals str_list =
    List.filter (fn x => if x = "" then false else Char.isUpper(String.sub(x, 0))) str_list

(* Problem 2
   Given a list of strings return the longest string in the list. In case two or more strings are the longest, return the one closest to the beginning of the list *)
fun longest_string1 str_list =
    List.foldl (fn (acc, curr_val) =>  if String.size(acc) > String.size(curr_val) then acc else curr_val) "" str_list

(* Problem 3
   Given a list of strings return the longest string in the list. In case two or more strings are the longest, return the one closest to the end of the list *)
fun longest_string2 str_list =
    List.foldl (fn (acc, curr_val) =>  if String.size(curr_val) > String.size(acc) then curr_val else acc) "" str_list

(* Problem 4
Same as problem 2 and 3 but using a generalized longest_string_helper to do the
*)

fun test (x, y) = x > y

fun longest_string_helper test_fn =
    List.foldl (fn (acc, curr_val) => if (test_fn (String.size(acc), String.size(curr_val))) then acc else curr_val) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


(*
  Problem 5
  Given a list of strings, return the longest string starting by a capital letter.If two strings are the longest
  return the one closest to the beginning of the list
*)

val longest_capitalized =  longest_string3 o (List.filter (fn x => if x = "" then false else Char.isUpper(String.sub(x, 0))))

(*
  Problem 6
  Given a string returns the same string but in reverse order
*)

val  rev_string = String.implode o rev o String.explode

(*
  Problem 7
*)

fun first_answer f list =
    case (((List.foldl (fn (curr_elem, acc) =>
                           case (acc, curr_elem) of
                               (_, NONE) => acc
                             | (SOME(x), SOME(Y)) => SOME (x)
                             | (_, SOME(x)) => SOME(x) ) NONE) o (List.map f)) list) of
        NONE => raise NoAnswer
      | SOME (x) => x

(* Problem 8
TO BE DOUBLE CHECKED
*)
fun all_answers f list =
    let fun lst_to_SOME some_list =
            case some_list of
                [] => []
              | (SOME(x))::xs => x @ lst_to_SOME(xs)
        val temp = List.map f list
    in
        if List.exists (fn x => case x of SOME(x) => false | NONE => true) temp then NONE else SOME(lst_to_SOME temp)
    end


(* Problem 9a *)
fun count_wildcards pattern =
        g (fn _ => 1) (fn _ => 0) pattern

(* Problem 9b *)
fun count_wild_and_variable_lengths pattern =
        g (fn _ => 1) (fn x => String.size(x)) pattern

(* Problem 9c *)
fun count_some_var (str, pattern) =
    g (fn _ => 0) (fn x => if x = str then 1 else 0) pattern

(* Problem 10 *)
 fun check_pat pattern =
    let fun extract_strings pattern=
            case pattern of
                    Variable(x) => [x]
                  | TupleP ps  => (List.foldl(fn (curr_pat, acc) => acc@(extract_strings curr_pat) ) [] ps)
                  | _ => []
        fun count_reps str list =
            List.foldl (fn (curr_str, acc) => if str = curr_str then acc + 1 else acc) 0 list
        val strings = extract_strings pattern
    in
       List.foldl (fn (curr_str, currstatus) => currstatus andalso ((count_reps curr_str strings) = 1)) true strings
    end

(* Problem 11 *)
 fun match (value :  valu, pat : pattern) =
     case (value, pat) of
         (_, Wildcard) => SOME []
       | (v, Variable (s)) => SOME [(s, v)]
       | (Unit, UnitP) => SOME[]
       | (Const (x), ConstP(y)) => if (x = y) then SOME [] else NONE
       | (Tuple (x), TupleP (y))  => if List.length(x) = List.length(y)
                                     then all_answers (fn (v, p) => match(v, p)) (ListPair.zip (x, y))
                                     else NONE
       | (Constructor(cname, v), ConstructorP(cnameP, p)) => if cname = cnameP
                                                             then match (v, p)
                                                             else NONE
       | _ => NONE

(* Problem 12*)
fun first_match v patterns =
    SOME(first_answer
             (fn (v, pat) => match (v, pat))
             (List.map (fn pat => (v, pat)) patterns)) handle NoAnswer => NONE
