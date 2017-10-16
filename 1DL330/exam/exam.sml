(* Functional Programming 1DL330
 * Take-Home Exam: An Articial Intelligence for Reversi
 *
 * Henrik Arro
 *)

(*
 * The board is represented as a vector of row vectors where each cell contains the position of the
 * cell on the board (0-63) and the contents (i.e., black, white or nothing). There is a border of
 * dummy cells around the board, so the dimension of the board is 10x10. The dummy cells contain a
 * position of -1, making it easy to filter them out when looking for neighbors of a cell.
 *
 * The search algorithm used is Negamax with alpha-beta pruning (https://en.wikipedia.org/wiki/Negamax).
 * Originally, the version without alpha-beta pruning was used. This made the code much cleaner since
 * we can then use map and fold in the implementation. However, the version with alpha-beta pruning
 * allows a search depth of 5 (12 at later stages) instead of 4 (6 at later stages), which makes s
 * small but noticeable difference in performance versus a random player.
 *
 * The time constraint of five minutes is ignored since the search depth is set so low that the total
 * think time on the machines svedberg and linne is usually less than a minute. If we wanted to take
 * the remaining time into account, we could for example decrease the search depth when the time starts
 * running out, perhaps going into complete random mode if we get into a real hurry.
 *
 * The evaluation function is based on the importance of holding the corner positions and lines along
 * the edges starting from a corner. A win is heavily rewarded, especially if it means wiping out the
 * opponent. Least importantly, the number of positions held by the players is also compared.
 *
 * I have occasionally added type annotations to values. This is just to make the compiler use
 * the type aliases (e.g., field, cell or board) instead of their basic types. It is not necessary,
 * but makes the signature that PolyML presents look a lot cleaner. I have only used the type T in
 * functions that are specified in the assignment description, in all other cases I use the type
 * player * board instead since I think this makes the code easier to understand.
 *)

structure Reversi_AI =
struct

(* The contents of a field on the board, i.e., empty, black or white *)
type field = player option

(* A cell contains the position (0 .. 63 or -1) of a field, and the field itself *)
type cell = int * field

(* A board is a vector of row vectors containing cells *)
type board = cell vector vector

(* The state of the machine: the player whose turn it is, and the board *)
type T = player * board

(* Exception signalling that player was not allowed to make move. *)
exception IllegalMove of player * move

val author = "Henrik Arro"

val nickname = "Hendrix"


(* ============================== *)
(* Miscellaneous helper functions *)
(* ============================== *)

(* between m n
 * TYPE: int -> int -> int list
 * PRE: true
 * POST: list of the integers m .. n, if m <= n, otherwise the empty list
 * SIDE EFFECTS:
 * EXAMPLES: between 2 3 = [2, 3]; between 2 2 = [2]; between 3 2 = []
 *)
(* VARIANT: size of the set of integers between m and n *)
fun between m n = if n < m then [] else m :: (between (m + 1) n);

(* repeat x n
 * TYPE: 'a -> int -> 'a list
 * PRE: n >= 0
 * POST: a list with x repeated n times
 * SIDE EFFECTS:
 * EXAMPLES: repeat "a" 0 = []; repeat 42 3 = [42, 42, 42];
 *)
(* VARIANT: n *)
fun repeat x 0 = []
  | repeat x n = x :: repeat x (n - 1)

(* insertUnique comp x xs
 * TYPE: ('a * 'a -> order) -> 'a -> 'a list -> 'a list
 * PRE: xs is sorted in ascending order according to comp
 * POST: the result of inserting x into xs, with any duplicates of x removed
 * SIDE EFFECTS:
 * EXAMPLES: insertUnique Int.compare 2 [1, 2, 2, 3, 3] = [1, 2, 3, 3];
 *)
(* VARIANT: length of xs *)
fun insertUnique comp x [] = [x]
  | insertUnique comp x (y::ys) =
    case comp (x, y) of
	LESS => x :: (y::ys)
      | EQUAL => insertUnique comp x ys
      | GREATER => y :: (insertUnique comp x ys);

(* sortWithDuplicatesRemoved comp xs
 * TYPE: ('a * 'a -> order) -> 'a list -> 'a list
 * PRE: true
 * POST: xs sorted in ascending order according to comp
 * SIDE EFFECTS:
 * EXAMPLES: sortWithDuplicatesRemoved Int.compare [3, 1, 1, 2, 3, 1, 1, 2, 2, 2] = [1, 2, 3];
 *)
(* VARIANT: length of xs *)
fun sortWithDuplicatesRemoved comp [] = []
  | sortWithDuplicatesRemoved comp (x::xs) = insertUnique comp x (sortWithDuplicatesRemoved comp xs);

(* Used to generate seeds used by nextRandom. *)
val randomSeedTimer = Timer.totalRealTimer ()

(* nextRandom n
 * TYPE: int -> int
 * PRE: n > 0
 * POST: a pseudorandom number in the range 0 .. n-1
 * SIDE EFFECTS: exception Div if n = 0
 * EXAMPLES: nextRandom 1 = 0; nextRandom 2 = 0; nextRandom 2 = 1;
 *)
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

(* opponent player
 * TYPE: player -> player
 * PRE: true
 * POST: the opponent of player
 * SIDE EFFECTS:
 * EXAMPLES: opponent Black = White; opponent White = Black;
 *)
fun opponent Black = White
  | opponent White = Black


(* ================================== *)
(* Functions to create an empty board *)
(* ================================== *)

(* The value of the border positions of the board *)
val dummyCell : cell = (~1, NONE);

(* createRow n
 * TYPE: int -> cell list
 * PRE: true
 * POST: a list of 8 empty cells with positions n .. n+7, surrounded by dummy cells, in total 10 cells
 *       (positions 28 and 35 are black, and positions 27 and 36 are white)
 * SIDE EFFECTS:
 * EXAMPLES: createRow 8 [(~1, NONE), (8, NONE), (9, NONE), ..., (15, NONE), (~1, NONE)];
 *)
fun createRow n =
    let
	fun createRow' n =
	    map (fn pos =>
		    if pos = 28 orelse pos = 35 then (pos, SOME Black)
		    else if (pos = 27 orelse pos = 36) then (pos, SOME White)
		    else (pos, NONE))
		(between n (n + 7))
    in
	(dummyCell :: createRow' n) @ [dummyCell]
    end

(* A board with the starting position for Reversi, with 10x10 cells, dummy cells around the edges, and with
 * empty cells with positions 0 .. 63 internally, except for positions 28 and 35 which are black and positions
 * 27 and 36 which are white. *)
val emptyBoard : board =
    Vector.fromList
	[Vector.fromList (repeat dummyCell 10),
	 Vector.fromList (createRow 0),
	 Vector.fromList (createRow 8),
	 Vector.fromList (createRow 16),
	 Vector.fromList (createRow 24),
	 Vector.fromList (createRow 32),
	 Vector.fromList (createRow 40),
	 Vector.fromList (createRow 48),
	 Vector.fromList (createRow 56),
	 Vector.fromList (repeat dummyCell 10)]

(* init player
 * TYPE: player -> T
 * PRE: true
 * POST: a starting state for a game where the computer plays as player
 *)
fun init p : T = (p, emptyBoard)


(* ====================================== *)
(* Lookup and update functions for boards *)
(* ====================================== *)

(* lookup row col board
 * TYPE: int -> int -> board -> cell
 * PRE: 0 <= row < 10 and 0 <= col < 10
 * POST: the cell at the given row and column of board
 * SIDE EFFECTS: Exception Subscript if row or column are out of range
 * EXAMPLES: lookup 2 3 emptyBoard = (10, NONE);
 * NOTE: the coordinates of the "normal" board, i.e., without the border of dummy cells,
 *       are 1-based, with row and col in the range 1 .. 8.
 *)
fun lookup row col (board : board) =
    let
	val row = Vector.sub (board, row)
    in
	Vector.sub (row, col)
    end

(* rowNum pos
 * TYPE: int -> int
 * PRE: 0 <= pos < 64
 * POST: the row number (1 .. 8) corresponding to position pos
 * SIDE EFFECTS: Exception Subscript if pos is out of range
 * EXAMPLES: rowNum 0 = 1; rowNum 15 = 2; rowNum 63 = 8;
 *)
fun rowNum pos = if pos < 0 orelse pos > 63 then raise Subscript else pos div 8 + 1

(* colNum pos
 * TYPE: int -> int
 * PRE: 0 <= pos < 64
 * POST: the column number (1 .. 8) corresponding to position pos
 * SIDE EFFECTS: Exception Subscript if pos is out of range
 * EXAMPLES: colNum 0 = 1; colNum 15 = 8; colNum 63 = 8;
 *)
fun colNum pos = if pos < 0 orelse pos > 63 then raise Subscript else pos mod 8 + 1;

(* updateBoard pos (player, board)
 * TYPE: int -> player * board -> board
 * PRE: 0 <= pos < 64
 * POST: a board where the cell with position pos is owned by player
 * SIDE EFFECTS: Exception Subscript if pos is out of range
 * EXAMPLES: lookup 42 (updateBoard 42 (Black, emptyBoard)) = (42, SOME Black);
 *)
fun updateBoard pos (player, board : board) : board =
    let
	val row = Vector.sub (board, rowNum pos)
	val newRow = Vector.update (row, colNum pos, (pos, (SOME player)))
    in
	Vector.update (board, rowNum pos, newRow)
    end

(* updateBoardMultiplePositions positions (player, board)
 * TYPE: int list -> player * board -> board
 * PRE: foreach pos in positions, 0 <= pos < 64
 * POST: a board where the cells with positions in the list of positions is owned by player
 * SIDE EFFECTS: Exception Subscript if any position in positions is out of range
 * EXAMPLES: updateBoardMultiplePositions [1, 2, 3] (Black, emptyBoard);
 *)
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

(* positionsToTurn pos (player, board)
 * TYPE: int -> player * board -> int list
 * PRE: 0 <= pos < 64
 * POST: a list of positions of pieces that would be turned if player places a piece at position pos of board
 * SIDE EFFECTS: Exception Subscript if pos is out of range
 * EXAMPLES: positionsToTurn 26 (Black, emptyBoard) = [27];
 *)
fun positionsToTurn pos (player, board) =
    let
	(* searchOneDirection row col (player, board) f
	 * TYPE: int -> int -> player * board -> (int * int -> int * int) -> int list
	 * PRE: 0 <= row < 10 and 0 <= col < 10 and
	 *      if f (row, col) = (r, c) then 0 <= r < 10 and 0 <= c < 10
	 * POST: a list of positions of pieces that player could turn if a piece is placed at row, col of board,
	 *       looking in the direction specified by f, which is assumed to increment or decrement
	 *       its arguments by 1
         * SIDE EFFECTS: Exception Subscript if row, col or f (row, col) are out of range
	 * EXAMPLES: searchOneDirection 4 3 (Black, emptyBoard) (fn (r,c)=>(r,c+1)) = [27];
	 *)
	(* VARIANT: is complex, but normal termination is guaranteed if the f function
	 *          updates the position so that it eventually reaches a) an empty cell,
	 *          b) a dummy (border) cell, or c) a cell owned by player. In other
	 *          cases, a Subscript exception is raised. The only way this function
	 *          will fail to terminate is if f is the identity function or otherwise
	 *          keeps the row and column to a small number of positions. *)
	fun searchOneDirection row col (player, board) updatePosition =
	    let
		fun search (newRow, newColumn) positionsToTurn =
		    let
			val (pos, field) = lookup newRow newColumn board
		    in
			if field = NONE orelse pos < 0 then []
			else if field = (SOME player) then positionsToTurn
			else search (updatePosition (newRow, newColumn)) (pos :: positionsToTurn)
		    end
	    in
		search (updatePosition (row, col)) []
	    end

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

(* makeMove move (player, board)
 * TYPE: move -> player * board -> board
 * PRE: if move = (Move pos) then the move must be legal (at least one piece is turned),
 *      and in range (0 <= pos < 64)
 * POST: a board with the resulting of making move: a pass leaves the board unchanged,
 *       a normal move will turn a number of pieces
 * SIDE EFFECTS: Exception IllegalMove (player, move) if move is not a pass and no pieces would be turned
 *               Exception Subscript if move contains a position that is out of range
 * EXAMPLES: makeMove (Move 26) (Black, emptyBoard); (* places a black piece at 26 and turns 27 *)
 *)
fun makeMove Pass (player, board) = board
  | makeMove (Move pos) (player, board) =
    let
	val positionsToTurn = positionsToTurn pos (player, board)
    in
	if null positionsToTurn then raise IllegalMove (player, (Move pos))
	else updateBoardMultiplePositions (pos :: positionsToTurn) (player, board)
    end

(* neighbors pos board
 * TYPE: int -> board -> cell list
 * PRE: 0 <= pos < 64
 * POST: a list of (up to) 8 cells that are neighbors to the cell at position pos
 * SIDE EFFECTS: Exception Subscript if pos is out of range
 * EXAMPLES: neighbors 1 emptyBoard = [(0, NONE), (2, NONE), (8, NONE), (9, NONE), (10, NONE)];
 *)
fun neighbors pos (board : board) : cell list =
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

(* isFieldTakenByPlayer player field
 * TYPE: player -> field -> bool
 * PRE: true
 * POST: true iff field is owned by player
 * SIDE EFFECTS:
 * EXAMPLES: isFieldTakenByPlayer Black NONE = false; isFieldTakenByPlayer Black (SOME White) = false;
 *           isFieldTakenByPlayer Black (SOME Black) = true;
 *)
fun isFieldTakenByPlayer (player : player) (NONE : field) = false
  | isFieldTakenByPlayer player (SOME p) = player = p

(* findAllCellsTakenByPlayer (player, board)
 * TYPE: player * board -> cell list
 * PRE: true
 * POST: a list of all cells owned by player in board
 * SIDE EFFECTS:
 * EXAMPLES: findAllCellsTakenByPlayer (Black, emptyBoard) = [(35, SOME Black), (28, SOME Black)];
 *)
fun findAllCellsTakenByPlayer (player, board : board) : cell list =
    let
	fun findAllCellsTakenByPlayerInRow row =
	    Vector.foldl
		(fn ((pos, field), rest) => if isFieldTakenByPlayer player field then (pos, field) :: rest else rest) [] row
    in
	Vector.foldl (fn (row, rest) => findAllCellsTakenByPlayerInRow row @ rest) [] board
    end

(* numTakenFields board
 * TYPE: board -> int
 * PRE: true
 * POST: the number of fields in board that are not free, i.e., taken by either black or white
 * SIDE EFFECTS:
 * EXAMPLES: numTakenFields emptyBoard = 4;
 *)
fun numTakenFields board =
    length (findAllCellsTakenByPlayer (Black, board)) +
    length (findAllCellsTakenByPlayer (White, board));

(* isLegalMove pos (player, board)
 * TYPE: int -> player * board -> bool
 * PRE: 0 <= pos < 64
 * POST: true iff it is legal for player to put a piece at position pos of board
 * SIDE EFFECTS: Exception Subscript if pos is out of range
 * EXAMPLES: isLegalMove 19 (Black, emptyBoard) = true; isLegalMove 19 (White, emptyBoard) = false;
 *)
fun isLegalMove pos (player, board) = not (null (positionsToTurn pos (player, board)))

(* findAllLegalPositions (player, board)
 * TYPE: player * board -> int list
 * PRE: true
 * POST: a list of all positions where it is legal for player to put a piece in board
 * SIDE EFFECTS:
 * EXAMPLES: findAllLegalPositions (Black, emptyBoard) = [19, 26, 37, 44];
 *)
fun findAllLegalPositions (player, board) =
    let
	(* findAllFreePositionsWithNeighbor (player, board)
	 * TYPE: player * board -> int list
	 * PRE: true
	 * POST: a list of all positions of free cells neighboring cells owned by player in board
	 * SIDE EFFECTS:
	 * EXAMPLES: findAllFreePositionsWithNeighbor (Black, emptyBoard) = [19, 20, 21, 29, 37, 26, 34, 42, 43, 44];
	 *)
	fun findAllFreePositionsWithNeighbor (player, board) =
	    let
		val positionsTakenByPlayer = findAllCellsTakenByPlayer (player, board)
		val fieldsWithNeighbor = foldl (fn ((pos, field), rest) => neighbors pos board @ rest) [] positionsTakenByPlayer
		val freeFieldsWithNeighbor = List.filter (fn (pos, field) => field = NONE) fieldsWithNeighbor
	    in
		List.map (fn (pos, field) => pos) freeFieldsWithNeighbor
	    end

	val availablePositions =
	    sortWithDuplicatesRemoved Int.compare (findAllFreePositionsWithNeighbor (opponent player, board))
    in
	List.filter (fn pos => isLegalMove pos (player, board)) availablePositions
    end

(* isFinalPosition board
 * TYPE: board -> bool
 * PRE: true
 * POST: true iff the board is in a final position where neither player can make a move
 * SIDE EFFECTS:
 * EXAMPLES: isFinalPosition emptyBoard = false;
 *)
fun isFinalPosition board =
    null (findAllLegalPositions (Black, board)) andalso null (findAllLegalPositions (White, board));


(* =============================== *)
(* Functions for evaluating boards *)
(* =============================== *)

(* totalSideLengthFromCorners (player, board)
 * TYPE: player * board -> int
 * PRE: true
 * POST: the total number of pieces owned by player in the board, starting in any corner and going
 *       along a side.
 * SIDE EFFECTS:
 * EXAMPLES: totalSideLengthFromCorners (Black, emptyBoard) = 0;
 * NOTE: If player owns a complete side, it will be counted twice, since it will be counted from
 *       both corners of the side. If the player owns the top row, for example, it will be counted
 *       as 16, 8 starting from the top left corner, and 8 starting from the top right corner. This
 *       is not a problem, or could even be a benefit, since the result is only used to give a rough
 *       estimate of the strength of the position.
 *)
fun totalSideLengthFromCorners (player, board) =
    let
	(* lineTakenByPlayer row col (player, board) f
	 * TYPE: int -> int -> player * board -> (int * int -> int * int) -> int list
	 * PRE: 0 <= row < 10 and 0 <= col < 10
	 * POST: A list of positions of pieces owned by player starting at row, col of board, looking the
	 *       direction specified by f, which is assumed to increment or decrement its arguments by 1
	 * SIDE EFFECTS: Exception Subscript if row or col are out of range
	 * EXAMPLES: lineTakenByPlayer 4 5 (Black, emptyBoard) (fn (r,c)=>(r,c+1)) = [28];
	 *)
	(* VARIANT: is complex, but normal completion is guaranteed if the f function updates
	 *          the position so that it eventually reaches a cell not owned by player. In
	 *          oher cases, a Subscrip exception is raised. The only way this function will
	 *          fail to terminate is if f is the identity function or otherwise keeps the
	 *          row and column to a small number of positions. *)
	fun lineTakenByPlayer row col (player, board) updatePosition =
	    let
		fun lineLength (newRow, newColumn) positionsTaken =
		    let
			val (pos, field) = lookup newRow newColumn board
		    in
			if field = (SOME player)
			then lineLength (updatePosition (newRow, newColumn)) (pos :: positionsTaken)
			else positionsTaken
		    end
	    in
		lineLength (row, col) []
	    end
    in
	length (lineTakenByPlayer 1 1 (player, board) (fn (row, col) => (row + 1, col))) +
	length (lineTakenByPlayer 1 1 (player, board) (fn (row, col) => (row, col + 1))) +
	length (lineTakenByPlayer 1 8 (player, board) (fn (row, col) => (row + 1, col))) +
	length (lineTakenByPlayer 1 8 (player, board) (fn (row, col) => (row, col - 1))) +
	length (lineTakenByPlayer 8 1 (player, board) (fn (row, col) => (row - 1, col))) +
	length (lineTakenByPlayer 8 1 (player, board) (fn (row, col) => (row, col + 1))) +
	length (lineTakenByPlayer 8 8 (player, board) (fn (row, col) => (row - 1, col))) +
	length (lineTakenByPlayer 8 8 (player, board) (fn (row, col) => (row, col - 1)))
    end

(* evaluateBoard (player, board)
 * TYPE: player * board -> int
 * PRE: true
 * POST: an estimate of the strength of the position for player on the board, the larger the number
 *       the better for player. A negative number means the opponent has a stronger positions, zero
 *       means equal strength.
 * SIDE EFFECTS:
 * EXAMPLES: evaluateBoard (Black, emptyBoard) = 0; evaluateBoard (White, emptyBoard) = 0;
 *)
fun evaluateBoard (player, board) =
    let
	val numCellsTakenByPlayer = length (findAllCellsTakenByPlayer (player, board))
	val numCellsTakenByOpponent = length (findAllCellsTakenByPlayer (opponent player, board))
	val sideLengthFromCorners = totalSideLengthFromCorners (player, board)
	val sideLengthFromCornersForOpponent = totalSideLengthFromCorners (opponent player, board)
	val scoreFromCells = (numCellsTakenByPlayer - numCellsTakenByOpponent) * 10
	val scoreFromLines = (sideLengthFromCorners - sideLengthFromCornersForOpponent) * 50
    in
	if numCellsTakenByOpponent = 0 then 10000
	else if numCellsTakenByPlayer = 0 then ~10000
	else if isFinalPosition board then
	    if numCellsTakenByPlayer > numCellsTakenByOpponent then scoreFromCells + 2000
	    else if numCellsTakenByPlayer < numCellsTakenByOpponent then scoreFromCells - 2000
	    else 0
	else
	    scoreFromCells + scoreFromLines
    end


(* ===================== *)
(* The Negamax algorithm *)
(* ===================== *)

(* Used as negative infinity for scores *)
val minScore = ~10000000

(* Used as positive infintiy for scores *)
val maxScore = 10000000

(* negamax depth (player, board) alpha beta
 * TYPE: int -> player * board -> int -> int -> int
 * PRE: depth >= 0
 * POST: the score for the best position for player that the negamax algorithm can find
 *       using the given search depth starting from board using alpha and beta as lower
 *       and upper bounds, respectively, for the score
 * SIDE EFFECTS:
 * NOTE: We do not have to use the color parameter described in the pseudocode at
 *       https://en.wikipedia.org/wiki/Negamax since evaluateBoard does the evaluation
 *       from the point of view of the current player.
 *)
(* VARIANT: depth *)
fun negamax depth (player, board) alpha beta =
    let
	fun handleChildren [] bestScore alpha beta = bestScore
	  | handleChildren (board::boards) bestScore alpha beta =
	    let
		val score = ~(negamax (depth - 1) (opponent player, board) (~beta) (~alpha))
		val newBestScore = Int.max (score, bestScore)
		val newAlpha = Int.max (alpha, score)
	    in
		if newAlpha >= beta then newBestScore
		else handleChildren boards newBestScore newAlpha beta
	    end
    in
	if depth = 0 orelse isFinalPosition board
	then evaluateBoard (player, board)
	else
	    let
		val positions = findAllLegalPositions (player, board)
		val moves = if (null positions) then [Pass] else map (fn pos => Move pos) positions
		val boards = map (fn move => makeMove move (player, board)) moves
	    in
		handleChildren boards minScore alpha beta
	    end
    end

(* This is the version without alpha-beta pruning. The code is a lot cleaner, but limits
 * the search depth to 4/6 instead of 5/12. *)
(*
fun negamax depth (player, board) =
    let
	fun handleChildren positions =
	    let
		val moves = if (null positions) then [Pass] else map (fn pos => Move pos) positions
		val boards = map (fn move => makeMove move (player, board)) moves
		val scores = map (fn board => ~(negamax (depth - 1) (opponent player, board))) boards
	    in
		foldl Int.max minScore scores
	    end
    in
	if depth = 0 orelse isFinalPosition board
	then evaluateBoard (player, board)
	else handleChildren (findAllLegalPositions (player, board))
    end
*)

(* searchDepth board
 * TYPE: board -> int
 * PRE: true
 * POST: the negamax search depth to use for the given board
 * SIDE EFFECTCS:
 *)
fun searchDepth board = if numTakenFields board > 51 then 12 else 5

(* evaluatePositions positions (player, board)
 * TYPE: int list -> player * board -> (int * int) list
 * PRE: for each pos in positions, 0 <= pos < 64 and pos represents a legal move for player
 * POST: a list with (position, score) pairs for each position in positions, showing
 *       the score that the negamax algorithm gives to the result of placing a piece
 *       for player at position on the board
 * SIDE EFFECTS: Exception IllegalMove (player, Move pos) if pos in positions represents an illegal move
 *               Exception Subscript if any position in positions is out of range
 *)
fun evaluatePositions positions (player, board) =
    let
	val moves = map (fn pos => Move pos) positions
	val boards = map (fn move => makeMove move (player, board)) moves
	val scores = map (fn board => ~(negamax (searchDepth board) (opponent player, board) minScore maxScore)) boards
    in
	ListPair.zip (positions, scores)
    end

(* findBestPosition positions (player, board)
 * TYPE: int list -> player * board -> int
 * PRE: for each pos in positions, 0 <= pos < 64 and pos represents a legal move for player
 * POST: the position in positions that represents what seems to be the best move for player on the board
 * SIDE EFFECTS: Exception IllegalMove (player, Move pos) if pos in positions represents an illegal move
 *               Exception Subscript if any position in positions is out of range
 * NOTE: This code randomizes the choice between moves with equal score. This is not necessary, but
 *       makes the result when playing against itself non-deterministic. It may also make it a bit more
 *       fun for a human player.
 *)
fun findBestPosition positions (player, board) =
    let
	val bestPositionAndScore =
	    foldl (fn ((pos1, score1), (pos2, score2)) =>
		      if score1 > score2 then (pos1, score1)
		      else if score1 = score2 then if (nextRandom 2) = 0 then (pos1, score1) else (pos2, score2)
		      else (pos2, score2))
		  (~1, minScore)
		  (evaluatePositions positions (player, board));
    in
	 #1 bestPositionAndScore
    end


(* ================ *)
(* The brain itself *)
(* ================ *)

(* think ((player, board), previousMove, timeLeft)
 * TYPE: T * move * Time.time -> move * T
 * PRE: previousMove is legal for the opponent given board
 * POST: (move, (player, board')) where move is the chosen move for player on the board
 *       that results after making previousMove for the opponent on board
 * SIDE EFFECTS: IllegalMove (opponent player, previousMove) if previousMove is illegal
 *               for the opponent on the given board
 *)
fun think ((player, board) : T, previousMove, timeLeft : Time.time) =
    let
	val newBoard = makeMove previousMove (opponent player, board)
	val legalPositions = findAllLegalPositions (player, newBoard)
	val move =
	    if null legalPositions then Pass
	    else (Move (findBestPosition legalPositions (player, newBoard)))
    in
	(move, (player, makeMove move (player, newBoard)) : T)
    end

end;
