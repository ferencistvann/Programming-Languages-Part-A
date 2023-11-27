(* Write a function is_older that takes two dates and evaluates to true or false.
It evaluates to true if the first argument is a date that comes before the second argument.
(If the two dates are the same, the result is false. *)
fun is_older (d1 : int * int * int, d2 : int * int * int) =
    if #1 d1 < #1 d2
    then true
    else if #2 d1 < #2 d2 andalso #1 d1 <= #1 d2
    then true
    else if #3 d1 < #3 d2 andalso #2 d1 <= #2 d2 andalso #1 d1 <= #1 d2
    then true
    else false


(* Write a function number_in_month that takes a list of dates and a month (i.e., an int)
and returns how many dates in the list are in the given month. *)	    
fun number_in_month (xs : (int * int * int) list, m : int) =
    if null xs
    then 0
    else if #2 (hd xs) = m
    then 1 + number_in_month (tl xs, m)
    else number_in_month (tl xs, m)

			
(* Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (xs : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month (xs, hd ms) + number_in_months (xs, tl ms)


(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month (xs : (int * int * int) list, m : int) =
    if null xs
    then []
    else if #2 (hd xs) = m
    then hd xs :: dates_in_month (tl xs, m)
    else dates_in_month (tl xs, m)
			
	
(* Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and the list-append operator @. *)
fun dates_in_months (xs : (int * int * int) list, ms : int list) =
    if null ms
    then []	
    else dates_in_month (xs, hd ms) @ dates_in_months (xs, tl ms)


(* Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay *)				     
fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth (tl xs, n-1)


(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
fun date_to_string (d : int * int * int) =
    let
	val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	(get_nth (month_list, #2 d) ^ " " ^ (Int.toString(#3 d)) ^ ", " ^ (Int.toString(#1 d)))
    end


(* Unfortunately the funtion below can not be used in the next excercise
(* Integer (Listof Integer) -> Integer *)
(* Read the homework pdf to see what the function does *)
fun number_before_reaching_sum (sum_0 : int, xs_0 : int list) =
    let
	fun number_before_reaching_sum (sum : int, xs : int list, current_number : int) =
	    if (current_number * (current_number + 1) div 2) >= sum
	    then current_number - 1
	    else number_before_reaching_sum (sum, xs, (current_number + 1))
    in
	number_before_reaching_sum (sum_0, xs_0, 1)
    end
*)	


(* Integer (Listof Integer) -> Integer *)
(* Read the homework pdf to see what the function does *)
fun number_before_reaching_sum (sum : int, xs_0 : int list) =
    let
	fun number_before_reaching_sum (xs : int list, count : int, sum_so_far : int) =
	    if hd xs + sum_so_far >= sum
	    then count
	    else number_before_reaching_sum (tl xs, count + 1, sum_so_far + hd xs)
    in
	number_before_reaching_sum (xs_0, 0, 0)
    end


(* Integer -> Integer *)
(* Take a day of the year (int[1, 365]), return which month it is (int[1, 12]) *)
fun what_month (d : int) =
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (d, months) + 1
    end


(* Integer Integer -> (Listof Integer) *)
(* Return an int list where 1st  element is the month of d1, 2nd is month of d1+1, last is month of d2 *)
fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else what_month (d1) :: month_range (d1+1, d2)


(* Could not do on own:					
(*
fun oldest (xs : (int * int * int) list) =
    if null xs
    then NONE
    else
	let
	    val tl_max = oldest (tl xs)
	in
	    if isSome tl_max andalso is_older (valOf tl_max, hd xs)
	    then tl_max
	    else SOME (hd xs)
	end
*)
					
(*
fun oldest (xs : (int * int * int) list) =
    if null xs
    then NONE
    else
	let
	    fun max_nonempty (xs : (int * int * int) list ) =
		if null (tl xs)
		then hd xs
		else
		    let
			val tl_max = max_nonempty (tl xs)
		    in
			if is_older (hd xs, tl_max)
			then hd xs
			else tl_max
		    end
			
	in
	    SOME (max_nonempty (xs))
	end
*)	    
	




					

