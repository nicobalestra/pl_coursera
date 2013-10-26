

fun is_older (date1:int*int*int,date2:int*int*int) = 
         if #1 date1 < #1 date2 then true 
    else if #1 date1 > #1 date2 then false 
    else if #2 date1 < #2 date2 then true 
    else if #2 date1 > #2 date2 then false
    else #3 date1 < #3 date2

fun number_in_month (ldates: (int*int*int) list, month) = 
   let fun loop (ldates: (int*int*int) list,acc) = 
      if null ldates
      then acc
      else loop (tl ldates, acc + (if #2 (hd ldates) = month then 1 else 0))
   in loop (ldates,0) end

fun number_in_months (ldates: (int*int*int) list, lmonths:int list )=
   if null lmonths
   then 0
   else number_in_month(ldates, hd lmonths) + number_in_months(ldates, tl lmonths)

fun dates_in_month (ldates: (int*int*int) list, month) = 
   let fun loop (ldates: (int*int*int) list,acc) = 
      if null ldates
      then acc
      else loop (tl ldates,  (if #2 (hd ldates) = month then (hd ldates)::acc else acc))
   in loop (ldates,[])
   end

fun dates_in_months(ldates: (int*int*int) list, lmonths:int list) =
  if null lmonths
  then []
  else dates_in_month(ldates, hd lmonths) @ dates_in_months(ldates, tl lmonths)

fun get_nth (l:string list, n:int) =
  if n = 1 then hd l
  else get_nth(tl l, n-1)

fun date_to_string (date:int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in (get_nth (months,#2 date)) ^ " " ^ (Int.toString (#3 date)) ^ ", " ^  (Int.toString (#1 date))
    end

fun number_before_reaching_sum (sum, lint: int list) =
    let fun loop (lint:int list, acc, index) = 
	    if null lint  then acc
	    else  if  acc + hd lint >= sum then index
		  else loop (tl lint, acc + hd lint, index+1)
    in loop(lint,0, 0)  end

fun what_month(day) = 
    1+ number_before_reaching_sum (day,[31,29,31,30,31,30,31,31,30,31,30,31])

fun month_range(date1,date2) =
    let fun loop (date, acc) = 
	    if (date<date1) then acc
	    else loop (date-1, what_month(date)::acc)
    in loop(date2,[]) end

fun oldest(ldates : (int*int*int) list) =
    if null ldates then NONE
    else let fun loop (ldates,oldest) = 
		 if null ldates then oldest
		 else loop (tl ldates, if is_older(hd ldates, oldest) then hd ldates else oldest)
	 in SOME (loop (tl ldates, hd ldates)) end

fun number_in_months_challenge(ldates: (int*int*int) list, lmonths:int list) = 
   let fun find (l,e) = 
      if null l then false 
      else if e = hd l then true
           else find (tl l, e)
   in let fun unique (l,u) =
	 if null l then u
	 else if find(u, hd l) then unique(tl l, u) else unique(tl l, hd l::u)
      in let val lmonths = unique(lmonths,[])
         in number_in_months(ldates, lmonths)
	 end
      end
   end
