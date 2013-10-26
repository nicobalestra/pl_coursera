(* Artem Abramov 
Date format: int*int*int, which is year*month*day *)

(* Problem 1 *)
(* Takes two dates and evaluates to true or false.  It evaluates to true if
the first argument is a date that comes before the second argument. 
If the two dates are the same, the result is false. *)
fun is_older(date1: int*int*int, date2: int*int*int) =
    let
        val year1 = #1 date1
        val year2 = #1 date2
        val month1 = #2 date1
        val month2 = #2 date2
        val day1 = #3 date1
        val day2 = #3 date2
    in
        if year1 = year2 then
            if month1 = month2 then
                day1 < day2
            else
                month1 < month2
        else
            year1 < year2
    end

(* Problem 2 *)
(* Write a function number_in_month that takes 
a list of dates and a month (i.e., an int) and 
returns how many dates in the list are in the given month. *)
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates then 0
    else
        if #2 (hd dates) = month then
            1 + number_in_month(tl dates, month)
        else
            number_in_month(tl dates, month)

(* Problem 3 *)
(* Write a function number_in_months that takes 
a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that 
are in any of the months in the list of months.
Assume the list of months has no number repeated. 
Hint: Use your answer to the previous problem. *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null dates then 0 else
    if null months then 0 else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Problem 4 *)
(* Write a function dates_in_month that takes a list of dates and 
a month (i.e., an int) and returns a list holding the dates from 
the argument list of dates that are in the month. The returned list 
should contain dates in the order they were originally given. *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates then [] else
    if #2 (hd dates) = month then
        (hd dates)::dates_in_month(tl dates, month)
    else
        dates_in_month(tl dates, month)

(* Problem 5 *)
(* Write a function dates_in_months that takes a list of dates and a list of 
months (i.e., an int list) and returns a list holding the dates from the 
argument list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to 
the previous problem and SML’s list-append operator (@). *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null dates orelse null months then [] else
        dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

(* Problem 6 *)
(* Write a function get_nth that takes a list of strings and an int n and 
returns the nth element of the list where the head of the list is 1st.
Do not worry about the case where the list has too few elements: 
your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(sourceStrings: string list, position: int) =
    if null sourceStrings orelse 
       position < 1 orelse
       position > (length sourceStrings) then ""
    else
       if position = 1 then hd sourceStrings
       else
            get_nth(tl sourceStrings, position - 1)

(* Problem 7 *)
(* Write a function date_to_string that takes a date and returns a string of 
the form January 20, 2013 (for example). Use the operator ^ for concatenating 
strings and the library function Int.toString for converting an int to a string.
For producing the month part, do not use a bunch of conditionals. 
Instead, use a list holding 12 strings and your answer to the previous problem.
For consistency, put a comma following the day and use capitalized English 
month names: January, February, March, April, May, June, July, 
August, September, October, November, December. *)
fun date_to_string(date: (int*int*int)) =
    let
        val monthsNames = ["January", "February", "March", "April",
                           "May", "June", "July", "August",
                           "September", "October", "November", "December"]
        val monthVariable = #2 date
    in
        get_nth(monthsNames, monthVariable)^
        " "^
        (Int.toString (#3 date))^
        ", "^
        (Int.toString (#1 date))
    end

(* Problem 8 *)
(* Write a function number_before_reaching_sum that takes an int called sum, 
which you can assume is positive, and an int list, which you can assume 
contains all positive numbers, and returns an int. 
You should return an int n such that the first n elements of the list 
add to less than sum, but the first n + 1 elements of the list add to sum 
or more. Assume the entire list sums to more than the passed in value; 
it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
    let
        fun helper_sum(tempsum: int, numbers: int list, index: int) =
            if tempsum > sum then index
            else
                helper_sum(tempsum + hd numbers, tl numbers, index + 1)
    in
        helper_sum(hd numbers, tl numbers, 0)
    end


(* Problem 9 *)
(* Write a function what_month that takes a day of year (i.e., an int between 1 
and 365) and returns what month that day is in (1 for January, 
2 for February, etc.). Use a list holding 12 integers and your answer 
to the previous problem. *)

fun what_month(dateOfTheYear: int) =
    let
        val daysInMonth = [31, 28, 31, (* Jan, Feb, Mar *)
                           30, 31, 30, (* Apr, May, Jun *)
                           31, 31, 30, (* Jul, Aug, Sep *)
                           31, 30, 31] (* Oct, Nov, Dec *)
    in
        number_before_reaching_sum(dateOfTheYear, daysInMonth)+1
    end

(* Problem 10 *)
(* Write a function month_range that takes two days of the year day1 and 
day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, 
m2 is the month of day1+1, ..., and mn is the month of day day2. Note the 
result will have length day2 - day1 + 1 or length 0 if day1>day2. *)
fun month_range(day1: int, day2: int) =
    if day1 > day2 then []
    else
        what_month(day1)::month_range(day1 + 1, day2)

(* Problem 11 *)
(* Write a function oldest that takes a list of dates and evaluates to 
an (int*int*int) option. It evaluates to NONE if the list has no dates 
and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates: (int*int*int) list) =
    if null dates then NONE else
    let
        fun helper(possibleOldest: (int*int*int), 
                   otherDates: (int*int*int) list) =
            if null otherDates then SOME possibleOldest else
                if is_older(possibleOldest, hd otherDates) then
                    helper(hd otherDates, tl otherDates)
                else
                    helper(possibleOldest, tl otherDates)

    in
        helper(hd dates, tl dates)
    end


(* Challenges *)

(* Problem 12 *)
(* Challenge Problem: Write functions number_in_months_challenge and 
dates_in_months_challenge that are like your solutions to problems 3 and 5 
except having a month in the second argument multiple times has no more 
effect than having it once. (Hint: Remove duplicates, then use previous work.) *)

fun removeDuplicates(from: int list, to: int list) =
    let
        fun findElementInList(whereList: int list, what: int) =
            if null whereList then false else
            if hd whereList = what then true else findElementInList(tl whereList, what)
    in
        if null from then [] else
        if findElementInList(to, hd from) then removeDuplicates(tl from, to)
        else
            (hd from)::removeDuplicates(tl from, hd from::to)
    end

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    number_in_months(dates, removeDuplicates(tl months, []))


fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    dates_in_months(dates, removeDuplicates(tl months, []))

(* Problem 13 *)
(* Write a function reasonable_date that takes a date and determines if it 
describes a real date in the common era. A “real date” has a positive year 
(year 0 did not exist), a month between 1 and 12, and a day appropriate for 
the month. Solutions should properly handle leap years. Leap years are years 
that are either divisible by 400 or divisible by 4 but not divisible by 100. 
(Do not worry about days possibly lost in the conversion to the Gregorian 
calendar in the Late 1500s.) *)

fun reasonable_date(date: int*int*int) =
    let
        val day = #3 date
        val month = #2 date
        val year = #1 date
        val longMonths = [1, 3, 5, 7, 8, 10, 12]
        val shortMonths = [4, 6, 9, 11]
        fun findElementInList(whereList: int list, what: int) =
            if null whereList then false else
            if hd whereList = what then true else findElementInList(tl whereList, what)
        fun isLeapYear(year: int) =
            (* assuming that year is correct here *)
            if ((year mod 400 = 0) orelse (year mod 4 = 0)) andalso
                (year mod 100 <> 0) then true else false
    in
        if  (year <= 0  orelse month <=0 orelse month > 12 orelse day <= 0) then
            NONE
        else
        if (findElementInList(longMonths, month)) andalso
            day > 31 then NONE 
        else
        if (findElementInList(shortMonths, month)) andalso
            day > 30 then NONE
        else
        if (isLeapYear year andalso month = 2 andalso day > 29) then NONE 
        else 
        if (not (isLeapYear year) andalso month = 2 andalso day > 28) then NONE
        else SOME true
    end


















