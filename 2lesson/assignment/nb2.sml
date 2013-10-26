(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 2.a
   Given a list of strings and a string, return NONE If the string is not found, SOME of the list of strings
   passed as input excluding the one found.
 *)
fun all_except_option (s, strings) =
    let fun recurse lstrings =
            case lstrings of
                [] => []
              | x::xs => if same_string(x,  s) then xs else x::recurse(xs)
    in
        let val res = recurse(strings)
        in
            if res = strings then NONE
            else SOME(res)
        end
    end


(*
  Problem 2.b
  Given a list of list of strings and a string, return the list of strings which is the concatenation of calling all_except_option for
  each element of the input list, and the given string.
  Solution: Call recursively get_substitutions1 matching the list of strings and returning at each step the result of the previoust step "cons"
  the result of calling all_except_option at the current step.
  Return when there are no more lists to consume.
*)
fun get_substitutions1 (lstrings, s) =
    case lstrings of
        []   => []
      | x::xs => case all_except_option(s, x) of
                     NONE => get_substitutions1(xs, s)
                   | SOME(y) => y @ get_substitutions1(xs, s)

(*
  Problem 2.c
  Same as Problem 2.b but using a helper function with tail recursion.
  Solution: The helper function consumes two lists. One containing the lists to check and the other containing
  the results.
  At each step take one element from the first list (a list of strings) and call all_except_option.
  Pattern match the result. If NONE then the string hasn't been found so call helper recursively skipping to the next element
  in the input list and without adding anything to the result list.
  If SOME is returned then add this result to the result list and make another recursive call with the new result list.

*)
fun get_substitutions2 (lsts, s) =
    let fun helper x =
            case x of
               ([], []) => []
             | ([], x::xs) => x::xs
             | (x::xs, y) => let val exclusions = all_except_option(s, x)
                             in
                                 case exclusions of
                                     NONE => helper(xs, y)
                                   | SOME(res) => helper(xs, y @ res)
                             end
    in
        helper(lsts, [])
    end

(* Problem 2.d
   Given a list of strings and a record of type {first="firstname", last="Lastname", middle="Middlename"} return
   a list of records containing the specified record as first element and records where the first name is substituted
   with the strings in the input lists (using get_substitutions function).

  Solution: Using a helper function. It's called the first time by substituting the strings using get_substitutions.
  Then recurse the list of substitutions and generate a list containing the requested records.
*)

fun similar_names (listlist, {first=f, last=l, middle=m}) =
    let
        fun substitute lst =
            case lst of
                ([], r) => r
              | (x::xs, r) => substitute(xs, r @ [{first=x, last=l, middle=m}])
    in
        substitute(get_substitutions2(listlist, f), [{first=f, last=l, middle=m}])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem 2.a
   Returns the color given a card *)
fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color _ = Red

(*
  Problem 2.b
  Takes a card and return its value.
  If the number of points is specified then return it otherwise Aces is 11 and everything else is 10
*)

fun card_value (_, Num (x)) = x
  | card_value (_, Ace) = 11
  | card_value (_,_) = 10

(* Problem 2.c
   Takes a list of cards cs, a card c and an exception e and returns a list that as all elements of cs except for c.
   If c is in the list more than once, remove only the first occurrence. If c is not found, throw exxception e.
*)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs => if x = c then xs
                 else x :: remove_card(xs, c,e)

(*
  Problem 2.d
  Takes a list of cards and returns true if all cards are the same color
*)
fun all_same_color lst =
    case lst of
        [] => true
      | x::[] => true
      | x::(xs::xxs) => card_color(x) = card_color(xs)  andalso all_same_color(xs::xxs)

(* Problem 2.e *)
fun sum_cards lst =
    let fun helper (x, s) =
            case x of
                [] => s
             | c::rest => helper(rest, s + (card_value(c)))
    in
        helper (lst, 0)
    end

(* Problem 2.f *)
fun score (lst, goal) =
    let fun calc_preliminary cards =
            let val sum = sum_cards(cards)
            in
                if sum > goal then 3 * (sum - goal)
                else (goal - sum)
            end
        val preliminary = calc_preliminary(lst)
    in
        if all_same_color(lst)
        then preliminary div 2
        else preliminary
    end

datatype move = Discard of card | Draw

(* Problem 2.g *)

fun officiate (cards_list, move_list, goal) =
    let fun helper (held_cards, rest_move, curr_card_list) =
            case (held_cards, rest_move, curr_card_list) of
               (x, [], _) =>  score(x, goal)
             | (x, ((Discard(c))::rest), y ) => helper(remove_card(x, c, IllegalMove), rest, y)
             | (x, (Draw)::rest, []) => score(x, goal)
             | (x, (Draw)::rest, y::rest_cards) => if sum_cards(x) > goal
                                                   then score(x, goal)
                                                   else helper(y::x, rest, rest_cards)
    in
        helper([], move_list, cards_list)
    end
