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

exception IllegalMove of player * move

fun otherPlayer Black = White
  | otherPlayer White = Black

(* ================================== *)
(* Functions to create an empty board *)
(* ================================== *)

val dummyNode = (~1, NONE);

fun createRow n =
    let
	fun createRow' n =
	    map (fn i =>
		    if i = 28 orelse i = 35 then (i, SOME Black)
		    else if (i = 27 orelse i = 36) then (i, SOME White)
		    else (i, NONE))
		(between n (n + 7))
    in
	(*[dummyNode] @ [createRow' n] @ [dummyNode]*)
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
	val pos = row * 10 + col
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

(* ====================================== *)
(* Search functions for turning positions *)
(* ====================================== *)

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
	    Vector.foldl (fn ((i, field), rest) => if isPositionTakenByPlayer player field then (i, field) :: rest else rest) [] row
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

fun findAllAvailableMoves (player, board) =
    let
	val availablePositions =
	    sortWithDuplicatesRemoved Int.compare (findAllFreePositionsWithNeighbor (otherPlayer player, board))
    in
	List.filter (fn pos => isLegalMove pos (player, board)) availablePositions
    end

fun think ((player, board), previousMove, timeLeft) =
    let
	val (_, newBoard) = makeMove previousMove (otherPlayer player, board)
	val availablePositions = findAllAvailableMoves (player, newBoard)
	val move = if null availablePositions then Pass else (Move (List.nth (availablePositions, length availablePositions div 2)))
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
    Vector.foldr (fn ((i, optionalPlayer), restOfRow) => (if i < 0 then "" else playerOptionToString optionalPlayer) ^ restOfRow)
    ""
    row

fun rowsToString rows =
    Vector.foldr (fn (row, rows) => if rowToString row = "" then rows else (rowToString row ^ "\n" ^ rows)) "" rows

fun boardToString (player, board) =
    rowsToString board ^ playerOptionToString (SOME player) ^ " to move"


end;
