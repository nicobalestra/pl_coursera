use "nb2.sml";
val test11 = all_except_option("sir", ["This", "is", "my", "first", "test", "sir"]) = SOME( ["This", "is", "my", "first", "test"]);
val test12 = all_except_option("is", ["This", "is", "my", "first", "test", "sir"]) = SOME( ["This", "my", "first", "test", "sir"]);
val test13 = all_except_option("blablabl", ["This", "is", "my", "first", "test", "sir"]) = NONE
val test14 = all_except_option("This", ["This"]) = SOME([]);
val test15 = all_except_option("This", ["This", "is", "my", "first", "test", "sir"]) = SOME( ["is", "my", "first", "test", "sir"]);

val test21 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"];
val test22 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "F") = ["Freddie","Fred"];
val test23 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Blablabl") = [];
val test24 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Elizabeth") = ["Betty"];
val test25 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fredrick") = ["Fred"];
val test26 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F", "Fred"]], "Fred") = ["Fredrick", "Freddie", "F", "Fred"];

val test31 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"];
val test32 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "F") = ["Freddie","Fred"];
val test33 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Blablabl") = [];
val test34 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Elizabeth") = ["Betty"];
val test35 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fredrick") = ["Fred"];
val test36 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F", "Fred"]], "Fred") = ["Fredrick", "Freddie", "F", "Fred"];


val test41 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
             [{first="Fred",last="Smith",middle="W"},
              {first="Fredrick",last="Smith",middle="W"},
              {first="Freddie",last="Smith",middle="W"},
              {first="F",last="Smith",middle="W"}];

val test42 = similar_names([["Fred","Fredrick"],["Fred", "Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Freddie", middle="W", last="Smith"}) =              [{first="Freddie",last="Smith",middle="W"},
              {first="Fred",last="Smith",middle="W"},
              {first="F",last="Smith",middle="W"}];

val test43 = similar_names([["Fred","Fredrick"],["Fred", "Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Blablabla", middle="W", last="Smith"}) =              [{first="Blablabla",last="Smith",middle="W"}];

val test44 = similar_names([["Fred","Fredrick"],["Fred", "Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Elizabeth", middle="W", last="Smith"}) =              [{first="Elizabeth",last="Smith",middle="W"},                                                                                                          {first="Fred",last="Smith",middle="W"},
                {first="Betty",last="Smith",middle="W"}];


val test51 = card_color (Clubs, Queen) = Black;
val test52 = card_color (Spades, King) = Black;
val test53 = card_color (Diamonds, Ace) = Red;
val test54 = card_color (Hearts, Num(1)) = Red;

val test61 = card_value (Clubs, Num(1))= 1
val test62 = card_value (Diamonds, Num(2))= 2
val test63 = card_value (Hearts, Num(3))= 3
val test64 = card_value (Spades, Num(4))= 4
val test65 = card_value (Clubs, Num(5))= 5
val test66 = card_value (Diamonds, Num(6))= 6
val test67 = card_value (Hearts, Num(7))= 7
val test68 = card_value (Hearts, Num(8))= 8
val test69 = card_value (Diamonds, Num(9))= 9
val test610 = card_value (Hearts, Ace)= 11
val test611 = card_value (Spades, Jack)= 10
val test612 = card_value (Clubs, Queen)= 10
val test613 = card_value (Diamonds, King)= 10

val test71 = (remove_card([(Clubs, Queen), (Diamonds, Ace), (Hearts, Num(1))], (Diamonds, Queen), IllegalMove) handle IllegalMove => []) = [];
val test72 = remove_card([(Clubs, Queen), (Diamonds, Ace), (Hearts, Num(1))], (Diamonds, Ace), IllegalMove) = [(Clubs, Queen), (Hearts, Num(1))];
val test73 = remove_card([(Clubs, Queen), (Diamonds, Ace), (Hearts, Num(1))], (Hearts, Num(1)), IllegalMove) = [(Clubs, Queen), (Diamonds, Ace)];
val test73 = remove_card([(Clubs, Queen), (Diamonds, Ace), (Hearts, Num(1))], (Clubs, Queen), IllegalMove) = [(Diamonds, Ace), (Hearts, Num(1))];

val test81 = all_same_color([ (Diamonds,Queen), (Clubs,Queen), (Clubs, Queen)]) = false;
val test82 = all_same_color([ (Clubs,Queen), (Clubs, Queen)]) = true;
val test83 = all_same_color([ (Diamonds,Queen)]) = true;
val test84 = all_same_color([]) = true;
val test85 = all_same_color([ (Clubs,Queen), (Clubs,Queen), (Clubs, Queen)]) = true;
val test86 = all_same_color([ (Spades,Queen), (Clubs,Queen), (Clubs, Queen)]) = true;
val test87 = all_same_color([(Diamonds ,Queen), (Spades,Queen), (Clubs,Queen), (Clubs, Queen)]) = false;
val test88 = all_same_color([(Spades ,Queen), (Spades,Queen), (Clubs,Queen), (Clubs, Queen)]) = true;

(* ace = 11*)
val test91 = sum_cards([(Clubs, Num(2)), (Clubs, Ace), (Clubs, Queen),(Clubs, Num(3))]) = 26;
val test92 = sum_cards([]) = 0;
val test93 = sum_cards([(Clubs, Num(2))]) = 2;
val test94 = sum_cards([(Clubs, Ace)]) = 11;
val test95 = sum_cards([(Clubs, Queen)]) = 10;
val test96 = sum_cards([(Clubs, Ace), (Clubs, Num (10))]) = 21;
val test97 = sum_cards([(Clubs, Ace), (Clubs, Num (0))]) = 11;

(* Ace = 11, Everything else=10 *)
(* Sum = 32
32 > 10 => 3 * (32 -10) = 66 *)
val testf1 = score([(Clubs, Ace), (Spades, Num (1)), (Diamonds, Queen), (Spades, Queen)], 10) = 66;
(* Sum = 32
32 > 10 => 3 * (32 - 10) => 66
same color => 66 / 2 = 33 *)
val testf2 = score([(Clubs, Ace), (Clubs, Num (1)), (Clubs, Queen), (Clubs, Queen)], 10) = 33;


(* Sum = 32
32 < 100 => 100 - 32 => 68
same color => 68 / 2 = 34 *)
val testf3 = score([(Clubs, Ace), (Clubs, Num (1)), (Clubs, Queen), (Clubs, Queen)], 100) = 34;

(* Sum = 32
32 < 100 => 100 - 32 => 68  *)
val testf4 = score([(Diamonds, Ace), (Clubs, Num (1)), (Clubs, Queen), (Clubs, Queen)], 100) = 68;

val testG1 = officiate([(Clubs, Queen), (Spades, Num(1)), (Diamonds, Num (9)), (Clubs, Ace)], [Draw, Discard (Clubs, Queen), Draw, Discard (Spades, Num(1)), Draw, Draw], 200) = 180;

val testG2 = officiate([(Clubs, Queen)], [Draw, Discard (Clubs, Queen)], 200) = score([], 200);

val testG3 = officiate([(Clubs,Queen)], [Draw], 10) = score([(Clubs, Queen)], 10);

val testG4 = officiate([(Clubs,Queen)], [Draw], 9) = score([(Clubs, Queen)], 9);

val testG5 = officiate([(Clubs,Queen)], [Draw, Discard(Clubs, Queen)], 9) = score([], 9);

val testG6 = (officiate([(Clubs,Queen), (Clubs, Queen)], [Discard (Diamonds, Queen), Draw], 9) handle IllegalMove => 0) = 0;

val testG7 = officiate([(Clubs,Queen), (Clubs, Queen)], [Draw, Discard (Clubs, Queen),  Draw], 20) = score ([(Clubs, Queen)], 20);

val testG8 = officiate([(Clubs,Queen), (Diamonds, King)], [Draw, Discard (Clubs, Queen),  Draw], 9) = score ([(Diamonds, King)], 9);

val testG9 = officiate([(Clubs,Queen), (Diamonds, King)], [Draw, Discard (Clubs, Queen),  Draw], 20) = score ([(Diamonds, King)], 20);
