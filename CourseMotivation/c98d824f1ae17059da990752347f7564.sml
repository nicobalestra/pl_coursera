fun is_older (date1: int*int*int, date2: int*int*int) =
    (#1 date1 < #1 date2) orelse
    ((#1 date1 = #1 date2) andalso (#2 date1 < #2 date2)) orelse
    ((#1 date1 = #1 date2) andalso (#2 date1 = #2 date2) andalso
     (#3 date1 < #3 date2))

fun number_in_month (datelist: (int*int*int) list, month: int) =
    if null datelist
    then 0
    else if #2 (hd(datelist)) = month
    then 1 + number_in_month (tl datelist, month)
    else number_in_month (tl datelist, month)

fun number_in_months (datelist: (int*int*int) list, monthlist: int list) =
    if null monthlist
    then 0
    else number_in_month(datelist, hd monthlist) + number_in_months(datelist, tl monthlist)

fun dates_in_month (datelist: (int*int*int) list, month: int) =
    if null datelist
    then []
    else if #2 (hd(datelist)) = month
    then hd(datelist) :: dates_in_month(tl datelist, month)
    else dates_in_month(tl datelist, month)

fun dates_in_months (datelist: (int*int*int) list, monthlist: int list) =
    if null monthlist
    then []
    else dates_in_month(datelist, hd monthlist) @ dates_in_months(datelist, tl monthlist)

fun get_nth (wordlist: string list, num: int) =
    if null wordlist
    then ""
    else if num = 1
    then hd wordlist
    else get_nth(tl wordlist, num - 1)

fun date_to_string (date: int*int*int) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August",
		      "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end	
						   
fun number_before_reaching_sum (sum: int, numlist: int list) =
    if sum <= hd numlist
    then 0
    else 1 + number_before_reaching_sum(sum - hd numlist, tl numlist)

fun what_month (day: int) =
    let
	val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	if day < 1 orelse day > 365
	then 0
	else 1 + number_before_reaching_sum(day, days_of_months)
    end

fun month_range (day1: int, day2: int) =
    if (day2 - day1) < 0
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (datelist: (int*int*int) list) =
    if null datelist
    then NONE
    else let fun check_datelist (dlist: (int*int*int) list) =
		 if null (tl(dlist))
		 then hd dlist
		 else let
		     val temp = check_datelist(tl dlist)
		 in
		     if is_older(hd dlist, temp)
		     then hd dlist
		     else temp
		 end
	 in
	     SOME(check_datelist(datelist))
	 end

fun contains (xs: int list, x: int) =
    if null xs
    then false
    else if hd xs = x
    then true
    else contains(tl xs, x)

fun remove_duplicates (xs: int list, ys: int list) =
    if null xs
    then []
    else if contains(ys , hd xs)
    then remove_duplicates(tl xs, ys)
    else hd xs :: remove_duplicates(tl xs, hd xs::ys)

fun number_in_months_challenge (datelist: (int*int*int) list, monthlist: int list) =
    number_in_months(datelist, remove_duplicates(monthlist, []))

fun dates_in_months_challenge (datelist: (int*int*int) list, monthlist: int list) =
    dates_in_months(datelist, remove_duplicates(monthlist, []))



fun reasonable_date (date: int*int*int) =
    let 
	fun is_leap_year (x: int) =
	    (((x mod 400) = 0) orelse ((x mod 4) = 0)) andalso (x mod 100 <> 0) 

	fun day_in_month (days_in_month: int list, month: int) =
	    if month = 1
	    then hd days_in_month
	    else day_in_month(tl days_in_month, month - 1)

	val reg_year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31]
	val leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30 ,31, 30, 31]
    in
	(#1 date > 0) andalso (#2 date > 0) andalso (#2 date < 13) andalso
	if is_leap_year(#1 date)
	then (#3 date <= day_in_month(leap_year, #2 date))
	else (#3 date <= day_in_month(reg_year, #2 date))
    end
