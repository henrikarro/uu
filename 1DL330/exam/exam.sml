structure Reversi_AI =
struct

type board = (int * player option) vector vector
type T = player * board

val author = "Henrik Arro"

val nickname = "Hendrix"

(* ============================== *)
(* Miscellaneous helper functions *)
(* ============================== *)

fun between m n = if n < m then [] else m :: (between (m + 1) n);

fun repeat x 0 = []
  | repeat x n = x :: repeat x (n - 1)

(* insertUnique comp x xs
   TYPE: ('a * 'a -> order) -> 'a -> 'a list -> 'a list
   PRE: xs is sorted in ascending order according to comp
   POST: the result of inserting x into xs, with any duplicates of x removed
   SIDE EFFECTS:
   EXAMPLES: insertUnique Int.compare 2 [1, 2, 2, 3, 3] = [1, 2, 3, 3];
 *)
fun insertUnique comp x [] = [x]
  | insertUnique comp x (y::ys) =
    case comp (x, y) of
	LESS => x :: (y::ys)
      | EQUAL => insertUnique comp x ys
      | GREATER => y :: (insertUnique comp x ys);

(* sortWithDuplicatesRemoved comp xs
   TYPE: ('a * 'a -> order) -> 'a list -> 'a list
   PRE: true
   POST: xs sorted in ascending order according to comp
   SIDE EFFECTS:
   EXAMPLES: sortWithDuplicatesRemoved Int.compare [3, 1, 1, 2, 3, 1, 1, 2, 2, 2] = [1, 2, 3];
 *)
fun sortWithDuplicatesRemoved comp [] = []
  | sortWithDuplicatesRemoved comp (x::xs) = insertUnique comp x (sortWithDuplicatesRemoved comp xs);

val randomSeedTimer = Timer.totalRealTimer ()
(* Inspired by nextrandom at https://www.cl.cam.ac.uk/~lp15/MLbook/programs/sample3.sml *)
fun nextRandom max =
    let
	fun nextRandom' seed =
	    let val a = 16807.0
		val m = 2147483647.0
		val t = a*seed
	    in
		t - m * real(floor(t/m))
	    end
	val r = nextRandom' (Time.toReal (Timer.checkRealTimer randomSeedTimer))
    in
	round r mod max
    end

exception IllegalMove of player * move

fun nextPlayer Black = White
  | nextPlayer White = Black

(* ================================== *)
(* Functions to create an empty board *)
(* ================================== *)

val dummyNode = (~1, NONE);

fun createRow n =
    let
	fun createRow' n =
	    map (fn pos =>
		    if pos = 28 orelse pos = 35 then (pos, SOME Black)
		    else if (pos = 27 orelse pos = 36) then (pos, SOME White)
		    else (pos, NONE))
		(between n (n + 7))
    in
	(dummyNode :: createRow' n) @ [dummyNode]
    end

val emptyBoard =
    Vector.fromList
	[Vector.fromList (repeat dummyNode 10),
	 Vector.fromList (createRow 0),
	 Vector.fromList (createRow 8),
	 Vector.fromList (createRow 16),
	 Vector.fromList (createRow 24),
	 Vector.fromList (createRow 32),
	 Vector.fromList (createRow 40),
	 Vector.fromList (createRow 48),
	 Vector.fromList (createRow 56),
	 Vector.fromList (repeat dummyNode 10)]


fun init p = (p, emptyBoard)

(* ====================================== *)
(* Lookup and update functions for boards *)
(* ====================================== *)

fun lookupByRowAndColumn row col board =
    let
	val row = Vector.sub (board, row)
    in
	Vector.sub (row, col)
    end

fun rowNum pos = if pos < 0 orelse pos > 63 then raise Domain else pos div 8 + 1
fun colNum pos = if pos < 0 orelse pos > 63 then raise Domain else pos mod 8 + 1;

fun lookup pos board = lookupByRowAndColumn (rowNum pos) (colNum pos) board;

fun updateBoard pos (player, board) =
    let
	val row = Vector.sub (board, rowNum pos)
	val newRow = Vector.update (row, colNum pos, (pos, (SOME player)))
    in
	Vector.update (board, rowNum pos, newRow)
    end

fun updateBoardMultiplePositions [] (player, board) = board
  | updateBoardMultiplePositions (pos::otherPositions) (player, board) =
    let
	val newBoard = updateBoard pos (player, board)
    in
	updateBoardMultiplePositions otherPositions (player, newBoard)
    end

(* ================================= *)
(* Functions for finding legal moves *)
(* ================================= *)

fun searchOneDirection row col (player, board) updatePosition =
    let
	fun search (newRow, newColumn) positionsToTurn =
	    let
		val (pos, field) = lookupByRowAndColumn newRow newColumn board
	    in
		if field = NONE orelse pos < 0 then []
		else if field = (SOME player) then positionsToTurn
		else search (updatePosition (newRow, newColumn)) (pos :: positionsToTurn)
	    end
    in
	search (updatePosition (row, col)) []
    end

fun positionsToTurn pos (player, board) =
    let
	val row = rowNum pos
	val col = colNum pos
    in
	(searchOneDirection row col (player, board) (fn (row, col) => (row, col - 1))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row, col + 1))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row - 1, col))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row + 1, col))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row - 1, col - 1))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row - 1, col + 1))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row + 1, col - 1))) @
	(searchOneDirection row col (player, board) (fn (row, col) => (row + 1, col + 1)))
    end

fun makeMove Pass (player, board) = (player, board)
  | makeMove (Move pos) (player, board) =
    let
	val positionsToTurn = positionsToTurn pos (player, board)
    in
	if null positionsToTurn then raise IllegalMove (player, (Move pos))
	else (player, (updateBoardMultiplePositions (pos :: positionsToTurn) (player, board)))
    end

fun neighbors pos board =
    let
	val previousRow = Vector.sub (board, rowNum pos - 1)
	val neighborsPreviousRow = [Vector.sub (previousRow, colNum pos - 1),
				    Vector.sub (previousRow, colNum pos),
				    Vector.sub (previousRow, colNum pos + 1)]
	val thisRow = Vector.sub (board, rowNum pos)
	val neighborsThisRow = [Vector.sub (thisRow, colNum pos - 1),
				Vector.sub (thisRow, colNum pos + 1)]
	val nextRow = Vector.sub (board, rowNum pos + 1)
	val neighborsNextRow = [Vector.sub (nextRow, colNum pos - 1),
				Vector.sub (nextRow, colNum pos),
				Vector.sub (nextRow, colNum pos + 1)]
	val allNeighbors = List.concat [neighborsPreviousRow, neighborsThisRow, neighborsNextRow];
    in
	List.filter (fn (i, _) => i >= 0) allNeighbors
    end

fun isPositionTakenByPlayer player NONE = false
  | isPositionTakenByPlayer player (SOME p) = player = p

fun findAllPositionsTakenByPlayer (player, board) =
    let
	fun findAllPositionsTakenByPlayerInRow row =
	    Vector.foldl
		(fn ((pos, field), rest) => if isPositionTakenByPlayer player field then (pos, field) :: rest else rest) [] row
    in
	Vector.foldl (fn (row, rest) => findAllPositionsTakenByPlayerInRow row @ rest) [] board
    end

fun findAllFreePositionsWithNeighbor (player, board) =
    let
	val positionsTakenByPlayer = findAllPositionsTakenByPlayer (player, board)
	val fieldsWithNeighbor = foldl (fn ((pos, field), rest) => neighbors pos board @ rest) [] positionsTakenByPlayer
	val freeFieldsWithNeighbor = List.filter (fn (pos, field) => field = NONE) fieldsWithNeighbor
    in
	List.map (fn (pos, field) => pos) freeFieldsWithNeighbor
    end

fun isLegalMove pos (player, board) = not (null (positionsToTurn pos (player, board)))

fun findAllLegalPositions (player, board) =
    let
	val availablePositions =
	    sortWithDuplicatesRemoved Int.compare (findAllFreePositionsWithNeighbor (nextPlayer player, board))
    in
	List.filter (fn pos => isLegalMove pos (player, board)) availablePositions
    end

(* =============================== *)
(* Functions for evaluating boards *)
(* =============================== *)

fun lineTakenByPlayer row col (player, board) updatePosition =
    let
	fun lineLength (newRow, newColumn) positionsTaken =
	    let
		val (pos, field) = lookupByRowAndColumn newRow newColumn board
	    in
		if field = (SOME player)
		then lineLength (updatePosition (newRow, newColumn)) (pos :: positionsTaken)
		else positionsTaken
	    end
    in
	lineLength (row, col) []
    end

fun numSidesTakenByPlayer (player, board) =
    let
	val topRow = searchOneDirection 1 1 (player, board) (fn (row, col) => (row, col + 1))
	val bottomRow = searchOneDirection 8 1 (player, board) (fn (row, col) => (row, col + 1))
	val leftColumn = searchOneDirection 1 1 (player, board) (fn (row, col) => (row + 1, col))
	val rightColumn = searchOneDirection 1 8 (player, board) (fn (row, col) => (row + 1, col))
    in
	(if length topRow = 8 then 1 else 0) +
	(if length bottomRow = 8 then 1 else 0) +
	(if length leftColumn = 8 then 1 else 0) +
	(if length rightColumn = 8 then 1 else 0)
    end

fun totalSideLengthFromCorners (player, board) =
    length (lineTakenByPlayer 1 1 (player, board) (fn (row, col) => (row + 1, col))) +
    length (lineTakenByPlayer 1 1 (player, board) (fn (row, col) => (row, col + 1))) +
    length (lineTakenByPlayer 1 8 (player, board) (fn (row, col) => (row + 1, col))) +
    length (lineTakenByPlayer 1 8 (player, board) (fn (row, col) => (row, col - 1))) +
    length (lineTakenByPlayer 8 1 (player, board) (fn (row, col) => (row - 1, col))) +
    length (lineTakenByPlayer 8 1 (player, board) (fn (row, col) => (row, col + 1))) +
    length (lineTakenByPlayer 8 8 (player, board) (fn (row, col) => (row - 1, col))) +
    length (lineTakenByPlayer 8 8 (player, board) (fn (row, col) => (row, col - 1)))

fun evaluateBoard (player, board) =
    let
	val numPositionsTakenByPlayer = length (findAllPositionsTakenByPlayer (player, board))
	val numPositionsTakenByOpponent = length (findAllPositionsTakenByPlayer (nextPlayer player, board))
	val totalSideLengthFromCorners = totalSideLengthFromCorners (player, board)
	val score = numPositionsTakenByPlayer - numPositionsTakenByOpponent + totalSideLengthFromCorners * 50
    in
	if numPositionsTakenByOpponent = 0 then score + 2000
	else if numPositionsTakenByPlayer + numPositionsTakenByOpponent = 64 then
	    if numPositionsTakenByPlayer > numPositionsTakenByOpponent then score + 1000
	    else if numPositionsTakenByPlayer < numPositionsTakenByOpponent then score - 1000
	    else 0
	else
	    score
    end

(* ===================== *)
(* The Negamax algorithm *)
(* ===================== *)

val minScore = ~10000000

fun negamax depth color (player, board) =
    let
	fun handleChildren positions =
	    let
		val moves = map (fn pos => Move pos) positions
		val boards = map (fn move => #2 (makeMove move (player, board))) moves
		val scores = map (fn board => ~(negamax (depth - 1) (~color) (nextPlayer player, board))) boards
	    in
		foldl Int.max minScore scores
	    end
	val legalPositions = findAllLegalPositions (player, board)
    in
	if depth = 0 orelse null legalPositions
	then color * evaluateBoard (player, board)
	else handleChildren legalPositions
    end

fun evaluatePositions positions (player, board) =
    let
	val moves = map (fn pos => Move pos) positions
	val boards = map (fn move => #2 (makeMove move (player, board))) moves
	val scores = map (fn board => negamax 4 1 (player, board)) boards
    in
	ListPair.zip (positions, scores)
    end

fun findBestPosition positions (player, board) =
    let
	val bestPositionAndScore =
	    foldl (fn ((pos1, score1), (pos2, score2)) => if score1 > score2 then (pos1, score1) else (pos2, score2))
		  (~1, minScore)
		  (evaluatePositions positions (player, board));
    in
	#1 bestPositionAndScore
    end

(* ================ *)
(* The brain itself *)
(* ================ *)

fun think ((player, board), previousMove, timeLeft) =
    let
	val (_, newBoard) = makeMove previousMove (nextPlayer player, board)
	val legalPositions = findAllLegalPositions (player, newBoard)
	val move =
	    if null legalPositions then Pass
	    else (Move (findBestPosition legalPositions (player, newBoard)))
    in
	(move, makeMove move (player, newBoard))
    end

(* ========================= *)
(* Functions to print boards *)
(* ========================= *)

fun playerOptionToString NONE = "."
  | playerOptionToString (SOME Black) = "x"
  | playerOptionToString (SOME White) = "o"

fun rowToString row =
    Vector.foldr (fn ((pos, field), restOfRow) => (if pos < 0 then "" else playerOptionToString field) ^ restOfRow)
    ""
    row

fun rowsToString rows =
    Vector.foldr (fn (row, rows) => if rowToString row = "" then rows else (rowToString row ^ "\n" ^ rows)) "" rows

fun boardToString (player, board) =
    rowsToString board ^ playerOptionToString (SOME player) ^ " to move"


end;
