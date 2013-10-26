(*
  Exercise 1:
  Param: d1 is the first date
         d2 is the second date
  Return: True if d1 is a date older than d2
  pre-condition: d1 and d2 are valid dates.
  Solution: Considering all months of 31 days, determines the number of days for each dates. Then it's just a matter of
  comparing the number of days.
*)
fun is_older(d1 : (int * int * int), d2 : (int * int * int)) =
    let
        (* Transorm the date into number of days. A month is considered to be 31 instead of 30 days to prevent edge cases where consecutive dates
           end up with the same number of days. ex: 2011, 08, 31 - 2011, 09, 01*)
        fun in_days(day: (int * int * int)) =
            (#1 day) * (12 * 31) + (31 * (#2 day)) + (#3 day)
    in
        in_days(d1) < in_days(d2)
    end

(*
   Exercise 2:
   Param: Dates list of dates (year, month, day)

   Solution: Define a nested function that recursively scan the list and for each element
   returns 1 if the month matches, 0 otherwise. The recursive step simply adds the result of
   the current date in the list with the result of the tale of the list passed as param.

*)
fun number_in_month (dates : (int * int * int) list, month : int) =
    let
        (* Business logic to determine whether a date matches the given month*)
       fun month_matches (date: (int * int * int), m : int) =
           if (#2 date) = m
           then 1
           else 0

        fun next_el(l : (int * int * int) list) =
            if null l
            then 0
            else
            if null (tl l)
            then month_matches ((hd l), month)
            else
                let
                    val curr_val = month_matches((hd l), month)
                in
                    curr_val + next_el(tl l)
                end
    in
        next_el(dates)
    end

(* Exercise number 3:
   Params: dates - List of dates to check the months
           months -List of months to check in the list of dates
   Returns:
   Integer representing the number of dates containing any of the months in the list of months
*)

fun number_in_months(dates : (int * int * int) list, months: int list) =
    if null months
    then 0
    else
        number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

(*
  Exercise 4 - dates_in_month
  Params : dates - A list of dates
           month - Integer representing the month to search in the list of dates
  Returns: A list of all dates (tuples) that are in the given month. The returned
  dates should be in the same order as provided by the "dates" argument.

  Solution: Same algorithm as number_in_months
*)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    let
        (* Business logic to determine whether a date matches the given month*)
       fun month_matches (date: (int * int * int), m : int) =
           if (#2 date) = m
           then SOME(date)
           else NONE

        fun next_el(l : (int * int * int) list) =
            if null l
            then []
            else
                let
                    val curr_val = month_matches((hd l), month)
                in
                    if isSome(curr_val)
                    then valOf(curr_val) :: next_el(tl l)
                    else next_el(tl l)
                end
    in
        next_el(dates)
    end


(*
  Exercise 5 - dates_in_months
  Params : dates - A list of dates
           months - A list of Integers representing the months to search in the list of dates
  Returns: A list of all dates (tuples) that are in any of the  given months of the incoming "months" list.
          The returned dates should be in the same order as provided by the "dates" argument.
*)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    let
       fun each_month(months_list : int list) =
           if null months_list
           then []
           else
               dates_in_month(dates, (hd months_list))  @ each_month(tl months_list);
    in
        each_month(months)
    end

(* Exercise 6
   Params: strings - List of strings
                     n - Integer
   Return: the nth element of the input list
*)
fun get_nth(strings : string list, n: int) =
    let
        fun return_sub_list (l : string list, curr: int) =
            if null l
            then ""
            else if curr = n
                 then hd l
                 else return_sub_list ((tl l), (curr + 1))
    in
        return_sub_list (strings, 1)
    end

(* Exercise 7
   Params: Date - Tuple in the form of (year, month, day)
   Return:
   String representing the given date in the form MMMM dd, yyyy
*)
fun date_to_string(date: (int*int*int)) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

    in
        get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(*
  Exercise n. 8
  Params:
         sum - A positive integer
         l - A list of positive integers
  Return:
  An integer "n" such that the sum of the first "n" elements of the list is < "sum" but
  the sum of the first "n+1" elements of the list is >= sum;

  Solution: Scan the given list and accumulate the sum of the current head with the sum accumulated so far.
            Then check what happens if we sum the next element in the list. If this sum is > of the passed "sum"
            then return the current accumulated sum otherwise scan the next element.
*)
fun number_before_reaching_sum(sum : int, l: int list) =
    let
        fun check(curr_l : int list, acc : int, curr_pos: int) =
            let
                val currsum = (hd curr_l) + acc;
            in
                (* If we reach the last element of the list  it means the sum of all elements in the list is < sum *)
                if null (tl curr_l)
                then 0
                else
                    if currsum < sum andalso currsum + (hd (tl curr_l)) >= sum
                    then curr_pos
                    else check((tl curr_l), currsum, curr_pos + 1)
            end
    in
        check (l, 0, 1)
    end


(* Exercise n. 9
   Params:  day - Int representing the day of a year 0 < day < 365
   Returns:
   Integer representing the month in which that day is*)
fun what_month(day : int) =
    let
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, months)
    end

(* Exercise 10
   Param : day1 - Int representing the first day of the to take into account.
           day2 - Int representing the second day of the year to take into account.
   Return:
   List of integers representing the month of each day comprised between day1 and day2 inclusive*)
fun month_range(day1 : int, day2 : int) =
    let
        fun recurse(d1 : int, d2 :  int, return : int list) =
            let
                val curr_month = what_month(d2) :: return;
            in
                if d2 < d1 then [] else
                if d1 = d2
                then curr_month
                else
                    recurse (d1, (d2 - 1), curr_month)
            end
    in
        recurse(day1, day2, [])
    end

(* Exercise 11
   Params: dates : List of tuples (int*int*int) representing dates.
   Return:
   Option (int*int*int) *)

fun oldest(dates : (int * int * int) list) =
    let
        fun search (curr_list : ( int * int * int ) list, curr_oldest : (int * int * int)) =
            let val curr_date = hd curr_list
                val new_oldest = if is_older (curr_oldest, curr_date) then curr_oldest else curr_date
            in

                if null (tl curr_list)
                then new_oldest
                else search (tl curr_list, new_oldest)
            end
    in
        if null dates
        then NONE
        else
            SOME(search(tl dates, hd dates))
    end

(* Search the element "what" into the list "src" and return true if the element is found *)
fun is_in(what: int, src: int list)=
    if null src then false else
    if (hd src) = what then true
    else is_in(what, tl src);
(* Given the source list "from" return a list containing only the unique values of that list*)
fun remove_duplicates(from : int list, uniques: int list) =
    if null from
    then uniques
    else
        if is_in(hd from, uniques)
        then remove_duplicates(tl from, uniques)
        else remove_duplicates(tl from,  uniques @ [(hd from)])

(* Challenge 1.1
   Function: number_in_months_challenge
   Description: As number_in_months except having duplicates in the list of months doesn't make any difference.
*)
fun number_in_months_challenge(dates : (int * int * int) list, months: int list) =
    (* Apply the earlier defined number_in_months but to a list of months with duplicates removed*)
    number_in_months(dates, remove_duplicates(months, []))



(* Challenge 1.2
   Function: dates_in_months_challenge
   Description: As dates_in_months except having duplicates in the list of months doesn't make any difference.
*)
fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months, []))

(*
   Challenge 2
   Function : reasonable_date
   Param: date - (int * int * int)
   Return : True if date represents a real (existing) date, false otherwise.
   Year must be > 0
   Month between 1 and 12
   Day must be appropriate for the given month (i.e 30 of February is NOT appropriate)
   Must handle Leap years (divisible by 400 or by 4 but not by 100
*)

fun reasonable_date(date: (int * int * int)) =
  let
      (* INNER FUNCTIONS *)
      fun get(arr : int list, el : int) =
          (* No need to check whether el is a valid number since the check is already done in the main function *)
          if el = 1
          then hd arr
          else get(tl arr, el -  1)

      fun is_leap_year (year : int) =
          (year mod 400 = 0 orelse (year mod 100 <> 0 andalso year mod 4 = 0))

      (* INNER BINDINGS *)
      val leap_months     = [31,29,31,30,31,30,31,31,30,31,30,31]
      val non_leap_months = [31,28,31,30,31,30,31,31,30,31,30,31]

      val year   = (#1 date)
      val month  = (#2 date)
      val day    = (#3 date)
      val months = if (is_leap_year year) then leap_months else non_leap_months
  in
      (* check for unreasonable values... *)
      if (month < 1 orelse month > 12 orelse year < 0 orelse day < 1 orelse day > 31)
      then false
      else
          (* now check that values are correct for the given month*)
          if day > get(months, month)
          then false
          else true
  end
