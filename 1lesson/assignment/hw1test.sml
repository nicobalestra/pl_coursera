(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "nb1.sml";

val test1 = is_older((1,2,3),(2,3,4)) = true
val test12 = is_older((2013, 3, 5), (2013, 3, 4)) = false

val test2  = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test21 = number_in_month([(2012,2,28),(2013, 2, 1), (2014, 1, 31), (2013,12,1)],2) = 2

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test31 = number_in_months([(2012,2,28), (2011,2,11), (2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 4
val test32 = number_in_months([(2012,2,28), (2011,2,11), (2013,12,1),(2011,3,31),(2011,4,28)],[5,6,7]) = 0

val test4  = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test41 = dates_in_month([(2012,2,12), (2012,2,28), (2012,1,28), (2011,2,10), (2013,12,1)],2) = [(2012,2,12), (2012,2,28), (2011,2,10)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test61 = get_nth(["hi", "there", "how", "are", "you"], 6) = ""

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test71 = date_to_string((2011, 2, 28)) = "February 28, 2011"


val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test81 = number_before_reaching_sum(1, [1,2,3,4,5]) = 0
val test82 = number_before_reaching_sum(3, [1,2,3,4,5]) = 1
val test83 = number_before_reaching_sum(15, [1,2,3,4,5,6]) = 4

val test9 = what_month(70) = 3
val test91 = what_month(1) = 1
val test92 = what_month(10) = 1
val test93 = what_month(365) = 12

val test10 = month_range(31, 34) = [1,2,2,2]
val test102 = month_range(1, 10) = [1,1,1,1,1,1,1,1,1,1]
val test103 = month_range(363, 365) = [12,12,12]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test111 = oldest([(2012,2,28),(2012,2,27),(2012,2,26)]) = SOME (2012,2,26)

val challenge11 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,1,2,2,2,3,3,3,3,3,1,1,1,2,2,4,4,4,4]) = 3
val challenge12 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val challenge13 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,2,2,2,2,2]) = 1

val challenge21 = dates_in_months_challenge([(2011, 1, 1), (2011, 2, 1), (2011, 3, 1), (2011, 4, 1)], [1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,1,1,1]) = [(2011, 1, 1), (2011, 2, 1), (2011, 3, 1)]
val challenge212 = dates_in_months_challenge([(2011, 1, 1), (2011, 2, 1), (2011, 3, 1), (2011, 4, 1)], [1,2,3]) = [(2011, 1, 1), (2011, 2, 1), (2011, 3, 1)]


val challenge31 = reasonable_date(2001, 1, 1) = true
val challenge32 = reasonable_date(2000, 2, 28) = true
val challenge33 = reasonable_date(2000, 2, 29) = true
val challenge34 = reasonable_date(2000, 2, 30) = false
val challenge35 = reasonable_date(2001, 2, 29) = false
val challenge36 = reasonable_date(400, 2, 29) = true
val challenge37 = reasonable_date(600, 2, 29) = false
val challenge38 = reasonable_date(2000, 12, 31) = true
val challenge39 = reasonable_date(2013, 3,31 ) = true
val challenge310 = reasonable_date(2013, 4, 31) = false
