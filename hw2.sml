fun same_string(s1 : string, s2 : string) =
    s1 = s2
type fullname =	 {first : string, middle : string, last : string}
		     
(* Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines. *)
fun all_except_option (str, strs) =
    let
	fun contains (strs, str) =
	    case strs of
		[] => false
	      | x :: xs' => if same_string (x, str)
			    then true
			    else contains (xs', str)

					  
	fun list_without_one (strs, str) =
	    case strs of
		[] => []
	      | x :: xs' => if same_string (x, str)
			    then list_without_one (xs', str)
			    else x :: list_without_one (xs', str)
    in
	if contains (strs, str)
	then SOME (list_without_one (strs, str))
	else NONE
    end

	
(* Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result.
 Assume each list in substitutions has no repeats. *)
fun get_substitutions1 (strss, str) =
    case strss of
	[] => []
      | x :: xs' => case all_except_option (str, x) of
			SOME str_list => str_list @ get_substitutions1 (xs', str)
		      | NONE => get_substitutions1 (xs', str)
							     
								    
(* Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun get_substitutions2 (strss, str) =
    let
	fun helper (strss, acc) =
	    case strss of
		[] => acc
	      | x :: xs' => case all_except_option (str, x) of
				SOME str_list => helper (xs', acc @ str_list)
			      | NONE => helper (xs', acc)
    in
	helper (strss, [])
    end
	

(* Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names).
Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is
around 10 lines.*)
fun similar_names (strss, {first=a, middle=b, last=c}) =
    let
	fun substitute (strs, {first=a, middle=b, last=c}) =
	    case strs of
		[] => []
	      | x :: xs' => {first=x, middle=b, last=c} :: substitute (xs', {first=a, middle=b, last=c})
    in
	{first=a, middle=b, last=c} :: substitute (get_substitutions2 (strss, a), {first=a, middle=b, last=c})
    end
	

datatype suit = Clubs | Diamonds | Hearts | Spades	
datatype rank = Jack | Queen | King | Ace | Num of int
						       
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough. *)	      
fun card_color c =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red
		 

(* Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value c =
    case c of
	(_, Ace) => 11
      | (_, Num i) => i
      | _ => 10


(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =. *)	  
fun remove_card (cs, c, ex) =
    let
	fun helper (cs, removed, rsf) =
	    case cs of
		[] => if removed = true
		      then rsf
		      else raise ex
	      | x :: xs' => if (x = c) andalso (removed = false)
			    then helper (xs', true, rsf)
			    else helper (xs', removed, x::rsf)
    in
	helper (cs, false, [])
    end


(* Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
pattern-matching in the lectures *)
fun all_same_color cs =
    case cs of
	[] => true
      | _ :: [] => true
      | x :: y :: z => card_color x = card_color y
		       andalso all_same_color (y::z)
		       


(* Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. (Calls use a constant amount of stack space is a
requirement for this problem.) *)
fun sum_cards cs =
    let
	fun helper (cs, rsf) =
	    case cs of
		[] => rsf
	      | x :: xs' => helper (xs', rsf + (card_value x))
    in
	helper (cs, 0)
    end
	
	
(* (Card list) Integer -> Integer *)
(* Calculates the score as described in the pdf *)
fun score (cs, g) =
    let
	val sum = sum_cards cs
	val same_color = all_same_color cs
    in
	if sum > g
	then if same_color
	     then 3 * (sum-g) div 2
	     else 3 * (sum-g)
	else if same_color
	then (g-sum) div 2
	else g-sum
    end
	

(* (Card list) (Move list) Integer -> Integer *)
(* Processes the moves in the move list and calculates the score at the end *)
fun officiate (cs, ms, g) =
    let
	fun game (held_cards, remaining_cards, moves) =
	    
    in
	game ([], cs, ms)
    end
	

	
