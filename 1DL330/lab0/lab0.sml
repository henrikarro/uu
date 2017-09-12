fun fac 0 = 1
  | fac n = n * fac (n - 1);

fun cube n = n * n * n;

fun smallest a b c =
  Int.min(a, Int.min(b, c));

fun IF e e1 e2 =
  if e then e1 else e2;

fun between a b =
  let val min = Int.min(a, b)
      val max = Int.max(a, b)
  in
      if max - min < 2 then 42 else (max + min) div 2
  end;

fun pow x n =
  let fun pow' _ 0 = 1
	| pow' x n = x * pow' x (n - 1)
  in
      if n < 0 then 1.0 / real(pow' x (~n)) else real(pow' x n)
  end;

fun third l = List.nth (l, 2);

fun length' [] = 0
  | length' (x::xs) = 1 + length' xs;

fun largest [] = 0
  | largest (x::xs) = Int.max(x, largest xs);

fun largest' [] _ = 0
  | largest' (x::xs) cmp =
    let
	val maxOfXs = largest' xs cmp
    in
	case cmp(x, maxOfXs) of
	    LESS => maxOfXs
	  | EQUAL => x
	  | GREATER => x
    end;

fun largest'' [] _ maxSoFar = maxSoFar
  | largest'' (x::xs) cmp maxSoFar =
    case cmp (x, maxSoFar) of
	LESS => largest'' xs cmp maxSoFar
      | EQUAL => largest'' xs cmp x
      | GREATER => largest'' xs cmp x;

fun reverseComparison cmp (x, y) =
  case cmp (x, y) of
      LESS => GREATER
    | EQUAL => EQUAL
    | GREATER => LESS;
