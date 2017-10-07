(* This is a rather crude implementation of Tic-tac-toe, including an
   AI that plays perfectly. *)

datatype player = X | O;

fun opponent X = O
  | opponent O = X;

type field = player option;

datatype board = Board of field * field * field *
                          field * field * field *
                          field * field * field;

datatype position = Position of player * board;

fun player_of (Position (p, _)) = p;

(* A crude evaluation function that returns SOME p if player p has
   won, and NONE otherwise. *)

fun value_of (Position (p, Board (f1, f2, f3,
                                  f4, f5, f6,
                                  f7, f8, f9))) =
  let
    fun same_player (SOME p) (SOME q) (SOME r) = (p=q) andalso (p=r)
      | same_player _ _ _ = false
  in
    (* rows *)
    if same_player f1 f2 f3 orelse
       same_player f4 f5 f6 orelse
       same_player f7 f8 f9 orelse
    (* columns *)
       same_player f1 f4 f7 orelse
       same_player f2 f5 f8 orelse
       same_player f3 f6 f9 orelse
    (* diagonals *)
       same_player f1 f5 f9 orelse
       same_player f3 f5 f7 then
      (* the opponent made the winning move *)
      SOME (opponent p)
    else
      NONE
  end;

datatype move = One | Two | Three | Four | Five | Six | Seven | Eight | Nine;

fun moves_of (p as Position (_, Board (f1, f2, f3,
                                       f4, f5, f6,
                                       f7, f8, f9))) =
  if isSome (value_of p) then
    []
  else
    (if f1=NONE then [One] else []) @
    (if f2=NONE then [Two] else []) @
    (if f3=NONE then [Three] else []) @
    (if f4=NONE then [Four] else []) @
    (if f5=NONE then [Five] else []) @
    (if f6=NONE then [Six] else []) @
    (if f7=NONE then [Seven] else []) @
    (if f8=NONE then [Eight] else []) @
    (if f9=NONE then [Nine] else []);

fun make_move (Position (p, Board (f1, f2, f3,
                                   f4, f5, f6,
                                   f7, f8, f9))) move =
  Position (opponent p, Board
    (case move of
    One   => (SOME p, f2, f3, f4, f5, f6, f7, f8, f9)
  | Two   => (f1, SOME p, f3, f4, f5, f6, f7, f8, f9)
  | Three => (f1, f2, SOME p, f4, f5, f6, f7, f8, f9)
  | Four  => (f1, f2, f3, SOME p, f5, f6, f7, f8, f9)
  | Five  => (f1, f2, f3, f4, SOME p, f6, f7, f8, f9)
  | Six   => (f1, f2, f3, f4, f5, SOME p, f7, f8, f9)
  | Seven => (f1, f2, f3, f4, f5, f6, SOME p, f8, f9)
  | Eight => (f1, f2, f3, f4, f5, f6, f7, SOME p, f9)
  | Nine  => (f1, f2, f3, f4, f5, f6, f7, f8, SOME p)));

(* SOME O < NONE < SOME X *)

fun compare (x, y) =
  if x=y then
    EQUAL
  else if x=SOME O orelse y=SOME X then
    LESS
  else
    GREATER;

(* A crude version of the minimax algorithm, extended to return the
   best move (if there is one) as well as the position's value. This
   version expands the entire game tree. *)

fun minimax position =
  let
    fun extremum cmp [] = raise Match
      | extremum cmp [(m, v)] = (SOME m, v)
      | extremum cmp ((m1, v1)::(m2, v2)::xs) =
        if compare (v1, v2) = cmp then
          extremum cmp ((m1, v1)::xs)
        else
          extremum cmp ((m2, v2)::xs)
    val max = extremum GREATER
    val min = extremum LESS
    val moves = moves_of position
  in
    if null moves then
      (NONE, value_of position)
    else
      let
        val positions = map (make_move position) moves
        val moves_and_values = map minimax positions
        val values = map (fn (_, v) => v) moves_and_values
      in
        (if player_of position = X then max else min)
          (ListPair.zipEq (moves, values))
      end
  end;

(* ASCII output. *)

fun string_of_player X = "X"
  | string_of_player O = "O";

fun string_of_field NONE = " "
  | string_of_field (SOME p) = string_of_player p;

fun string_of_board (Board (f1, f2, f3,
                            f4, f5, f6,
                            f7, f8, f9)) =
  string_of_field f1 ^ "|" ^ string_of_field f2 ^ "|" ^ string_of_field f3 ^ "\n" ^
  "-----\n" ^
  string_of_field f4 ^ "|" ^ string_of_field f5 ^ "|" ^ string_of_field f6 ^ "\n" ^
  "-----\n" ^
  string_of_field f7 ^ "|" ^ string_of_field f8 ^ "|" ^ string_of_field f9 ^ "\n";

fun string_of_position (Position (p, b)) =
  string_of_board b ^
  "Player to move: " ^ string_of_player p ^ "\n";

fun string_of_move One = "1"
  | string_of_move Two = "2"
  | string_of_move Three = "3"
  | string_of_move Four = "4"
  | string_of_move Five = "5"
  | string_of_move Six = "6"
  | string_of_move Seven = "7"
  | string_of_move Eight = "8"
  | string_of_move Nine = "9";

(* We let the minimax AI play against itself. *)

fun tic_tac_toe position =
let
  val _ = print "======================\n"
  val _ = print (string_of_position position)
in
  if null (moves_of position) then
    case value_of position of
      NONE =>
        print "The game is a draw.\n"
    | SOME p =>
        print ("The winner is: " ^ string_of_player p ^ "\n")
  else
    let
      val (m_opt, v) = minimax position
      val m = valOf m_opt
    in
      print ("Best move: " ^ string_of_move m ^ "\n");
      print ("Predicted winner: " ^
        (case v of NONE => "draw" | SOME p => string_of_player p) ^ "\n");
      tic_tac_toe (make_move position m)
    end
end;

val initial_position = Position (X, Board (NONE, NONE, NONE,
                                           NONE, NONE, NONE,
                                           NONE, NONE, NONE));

tic_tac_toe initial_position;
