fun iota 0 = []
  | iota n = iota (n - 1) @ [n - 1];

fun member _ [] = false
  | member x (y::ys) = x = y orelse member x ys;

fun inter [] _ = []
  | inter (x::xs) ys =
    if member x ys then
	x :: (inter xs ys)
    else
	inter xs ys;

fun inter' [] _ = []
  | inter' _ [] = []
  | inter' (x::xs) (y::ys) =
    if x = y then x :: (inter' xs ys)
    else if x < y then (inter' xs (y::ys))
    else (* x > y *) (inter' (x::xs) ys);

datatype fruit = Apple of real | Banana of real | Lemon of int;

fun sumPrice fruits applePrice bananaPrice lemonPrice =
    let fun price (Apple(weight)) = applePrice * weight
	  | price (Banana(weight)) = bananaPrice * weight
	  | price (Lemon(units)) = lemonPrice * real(units)
	fun sumPrice' [] = 0.0
	  | sumPrice' (fruit::fruits) = (price fruit) + (sumPrice' fruits)
    in
	sumPrice' fruits
    end;

datatype 'a ltree = Node of 'a * 'a ltree list;

fun sumList [] sumSoFar = sumSoFar
  | sumList (x::xs) sumSoFar = sumList xs (sumSoFar + x);

fun sumList' l minValue = foldl op+ minValue l;

fun count (Node(_, children)) = 1 + (sumList (List.map count children) 0);

fun labels (Node(x, children)) = x :: (foldr op@ [] (List.map labels children));

fun is_present' x (Node(y, children)) = x = y orelse List.exists (is_present' x) children;
fun is_present tree x = is_present' x tree;

fun maxList [] maxSoFar = maxSoFar
  | maxList (x::xs) maxSoFar = if x > maxSoFar then max xs x else  maxList xs maxSoFar;

fun maxList' l minValue = foldl Int.max minValue l;

fun height (Node(_, [])) = 1
  | height (Node(_, children)) = 1 + max (List.map height children) 0;

