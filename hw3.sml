(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


(* Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals strs =
    List.filter (fn str => Char.isUpper (String.sub (str, 0))) strs


(* Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)
fun longest_string1 strs =
    List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" strs


(* Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size. *)
fun longest_string2 strs =
    List.foldl (fn (x,y) => if String.size y > String.size x then y else x) "" strs


(* Write functions longest_string_helper, longest_string3, and longest_string4 such that:
- longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
- longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
- If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.
- longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper. *)
fun longest_string_helper f strs =
    List.foldl (fn (x,y) => if f (String.size x, String.size y) then x else y) "" strs

	       
val longest_string3 = longest_string_helper (fn (x,y) => x>y)

					    
val longest_string4 = longest_string_helper (fn (x,y) => x>=y) 

	       
fun longest_capitalized strs =
    let
	val longest_cap = longest_string1 o only_capitals
    in
	longest_cap strs
    end		
(*	      
fun longest_capitalized strs =
    (longest_string1 o only_capitals) strs
*)  

fun rev_string s =
    (String.implode o List.rev o String.explode) s
	       
   
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME y => y
		    | NONE => first_answer f xs'


fun all_answers f xs =
    let
	fun helper xs acc =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      SOME y => helper xs' (acc @ y)
			    | NONE => NONE   
    in
	helper xs []
    end

	
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

	
fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p
    

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size x) p


fun count_some_var (str, p) =
    g (fn () => 0) (fn x => if str = x then 1 else 0) p

(*
fun has_duplicate strs =
    case strs of
	[] => false
     | x::xs' => (List.exists (fn y => y = x) xs') orelse has_duplicate xs' 
*)


					   