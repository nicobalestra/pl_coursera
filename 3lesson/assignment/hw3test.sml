(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "nb_hw3.sml";

val test1 =  only_capitals["A","B","C"] = ["A","B","C"]
val test11 = only_capitals["abc", "Abcd", "Cdea", "dddd"] = ["Abcd", "Cdea"]
val test12 = only_capitals["abcd", "hdffh", "asdjf"] = [];

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 [] = ""
val test22 = longest_string1 ["abc", "abcd", "abcde", "1234", "54321"] = "abcde"
val test23 = longest_string1 ["abc", "abcd", "abcdeF", "1234", "Abcdef"] = "abcdeF"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 [] = ""
val test32 = longest_string2 ["abc", "abcd", "abcde", "1234", "54321"] = "54321"
val test33 = longest_string2 ["abc", "abcd", "abcde", "1234", "Abcdef", "abcdeF"] = "abcdeF"

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 [] = ""
val test4a2 = longest_string3 ["abc", "abcd", "abcde", "1234", "54321"] = "abcde"
val test4a3 = longest_string3 ["abc", "abcd", "abcdeF", "1234", "Abcdef"] = "abcdeF"


val test4b= longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 [] = ""
val test4b2 = longest_string4 ["abc", "abcd", "abcde", "1234", "54321"] = "54321"
val test4b3 = longest_string4 ["abc", "abcd", "abcde", "1234", "Abcdef", "abcdeF"] = "abcdeF"


val test5 = longest_capitalized ["A","bc","C"] = "A";
val test51 = longest_capitalized ["a","bc","c"] = "";
val test52 = longest_capitalized ["Abcde","12345","abcdefgsksdkf", "Abcdef", ""] = "Abcdef";
val test51 = longest_capitalized [] = "";

val test6 = rev_string "abc" = "cba";
val test61 = rev_string "" = "";
val test62 = rev_string "12345" = "54321";
val test63 = rev_string "a" = "a";


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test71 = first_answer (fn x => if x > 4 then SOME x else NONE) [1,2,3,4,5] = 5
val test72 = first_answer (fn x => if x > 1 then SOME x else NONE) [1,2,3,4,5] = 2
val test73 = (first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5] handle NoAnswer => ~1) = ~1

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test82 = all_answers (fn x => if x > 2 then SOME [x] else NONE) [1,2,3,4,5] = NONE
val test83 = all_answers (fn x => if x > 2 then SOME [x] else NONE) [3,4,5] = SOME [3,4,5];

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (TupleP [Wildcard, Wildcard, (TupleP [Wildcard, Variable("s"), ConstP(1)])]) = 3
val test9a2 = count_wildcards (TupleP [Variable("s"), UnitP, (TupleP [Variable("ddd"), Variable("s"), ConstP(1)])]) = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (TupleP[Wildcard, Variable("a"), (TupleP [Wildcard, Variable("ABCD")])]) = 7
val test9b2 = count_wild_and_variable_lengths (TupleP[]) = 0

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9c1 = count_some_var ("x", (TupleP [Wildcard, Variable("x"), (ConstructorP("gigi", (TupleP [ ConstP(1), Variable("x")])))])) = 2;
val test9c2 = count_some_var ("x", (TupleP [Wildcard, UnitP, (ConstructorP("gigi", (TupleP [ ConstP(1), Wildcard])))])) = 0;
val test9c3 = count_some_var ("x", (UnitP)) = 0;

val test10 = check_pat (Variable("x")) = true
val test101 = check_pat (TupleP[Variable("x"), Wildcard, (TupleP[Variable("x"), Wildcard])]) = false
val test102 = check_pat (TupleP[Variable("x"), Wildcard, (TupleP[Variable("y"), Wildcard])]) = true
val test103 = check_pat (TupleP[Variable("x"), Wildcard, (TupleP[ConstructorP("x", Variable("j")), Variable("x"), Wildcard])]) = false
val test104 = check_pat (TupleP[Variable("x"), Wildcard, (TupleP[ConstructorP("x", Variable("j")), Variable("y"), Wildcard])]) = true


val test11 = match (Const(1), UnitP) = NONE
val test111 = match (Const 1, Variable("x")) = SOME [("x", Const 1)]
val test112 = match (Const 1, ConstP 1) = SOME []
val test113 = match (Const 1, ConstP 2) = NONE
val test114 = match (Unit, ConstP 2) = NONE
val test115 = match (Unit, UnitP) = SOME []
val test116 = match ((Tuple [Unit, Const 2, Tuple [Constructor ("const", Tuple [Const 1, Unit])]]),
                    (TupleP [])) = NONE

val test117 = match ((Tuple [Unit, Const 2, Tuple [Constructor ("const", Tuple [Const 1, Unit])]]),
                    (TupleP [Variable "x", Variable "y", (TupleP [ConstructorP ("ciccio", TupleP [Variable "y", Wildcard])])])) = NONE

val test118 = match ((Tuple [Unit, Const 2, Tuple [Constructor ("const", Tuple [Const 1, Unit])]]),
                    (TupleP [Variable "x", Variable "y", (TupleP [ConstructorP ("const", TupleP [Variable "y", Wildcard])])])) = SOME[("x", Unit),("y", Const 2), ("y", Const 1)]

val test12 = first_match Unit [UnitP] = SOME []
