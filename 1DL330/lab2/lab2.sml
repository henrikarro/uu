(* Functional Programming 1DL330
   Assignment 2

   Henrik Arro
   Imad Collin
 *)

(* Excercise 1: Iota *)

(* iota n
   TYPE: int -> int list
   PRE: n >= 0
   POST: list of the integers 0 .. n-1
   SIDE EFFECTS:
   EXAMPLES: iota 0 = [], iota 1 = [0], iota 5 = [0, 1, 2, 3, 4];
 *)
fun iota n =
  (* iota' n listSoFar
     TYPE: int -> int list -> int list
     PRE: n >= 0
     POST: list of the integers 0 .. n-1 concatenated to listSoFar *)
  (* VARIANT: n *)
  let fun iota' 0 listSoFar = listSoFar
	| iota' n listSoFar = iota' (n - 1) ((n - 1) :: listSoFar)
  in
      iota' n []
  end;

(* The following obvious implementation of iota is too slow, O(n^2),
   that's why we use the tail-recursive accumulator version above:

   fun iota 0 = []
     | iota n = iota (n - 1) @ [n - 1];
 *)

(* Imad found the List.tabulate function, so we can simplify iota like this
   (this is the best solution, in my opinion, but I guess you don't mind seeing
   a recursive solution):

   fun iota n = List.tabulate (n, fn x => x);
 *)

(* Excercise 2: Intersection *)

(* member x list
   TYPE: ''a -> ''a list -> bool
   PRE: true
   POST: true iff x is a member of list
   SIDE EFFECTS:
   EXAMPLES: member 42 [] = false; member "bar" ["foo", "bar", "baz"] = true;
 *)
(* VARIANT: length of list *)
fun member _ [] = false
  | member x (y::ys) = x = y orelse member x ys;

(* inter s1 s2
   TYPE: ''a list -> ''a list -> ''a list
   PRE: true
   POST: a list with the values that are members of both s1 and s2
   SIDE EFFECTS:
   EXAMPLES: inter [] [1, 2] = []; inter [1, 2] [] = []; inter [1, 3, 2] [4, 3, 2] = [3, 2];
 *)
(* VARIANT: length of s1 *)
fun inter [] _ = []
  | inter (x::xs) ys =
    if member x ys then
	x :: (inter xs ys)
    else
	inter xs ys;

(* inter' s1 s2
   TYPE: int list -> int list -> int list
   PRE: s1 and s2 are sorted in ascending order
   POST: a list with the values that are members of both s2 and s2
   SIDE EFFECTS:
   EXAMPLES: inter' [] [1, 2] = []; inter' [1, 2] [] = []; inter' [1, 2, 3] [2, 3, 4] = [2, 3];
 *)
(* VARIANT: length s1 + length s2 *)
fun inter' [] _ = []
  | inter' _ [] = []
  | inter' (x::xs) (y::ys) =
    if x = y then
	x :: (inter' xs ys)
    else if x < y then
	inter' xs (y::ys)
    else (* x > y *)
	inter' (x::xs) ys;

(* Speed comparison between inter and inter':

   length s1=10000, length s2=100000 -> inter time 0.241, inter' time 0.000
   length s1=20000, length s2=200000 -> inter time 0.992, inter' time 0.000
   length s1=40000, length s2=400000 -> inter time 3.816, inter' time 0.000
   length s1=80000, length s2=800000 -> inter time 15.317, inter' time 0.001

   It seems that inter is O(length s1 * length s2), while inter' is O(length s1 + length s2).

   Well, the time complexity of inter' is difficult to see from the numbers, but it seems
   reasonable that it needs to consider somewhere between the length of one list and the
   combined lengths of the two lists.

   This means that it is more efficient to first sort the lists (O(n log n)) and then use
   inter' than in it to use inter.
 *)

(* Exercise 3: Fruit *)

datatype fruit =
	 Apple of real (* Weight units of apples *)
	 | Banana of real (* Weight units of bananas *)
	 | Lemon of int; (* Units of lemons *)

(* sumPrice fruits applePrice bananPrice lemonPrice
   TYPE: fruit list -> real -> real -> real -> real
   PRE: true
   POST: the total price of fruits in the list fruits, given that the price per weight unit of apples is applePrice,
   	 the price per weight unit of bananas is bananaPrice, and the price per unit of lemons is lemonPrice
   SIDE EFFECTS:
   EXAMPLES: sumPrice [Banana 3.0, Apple 2.0, Lemon 3] 2.0 3.0 4.0 =~ 25.0;
 *)
fun sumPrice fruits applePrice bananaPrice lemonPrice =
  let
      (* price fruit
	 TYPE: fruit -> real
         PRE: true
         POST: the price of fruit, given applePrice, bananaPrice and lemonPrice *)
      fun price (Apple(weight)) = applePrice * weight
	| price (Banana(weight)) = bananaPrice * weight
	| price (Lemon(units)) = lemonPrice * real(units)
      (* sumPrice' fruits
	 TYPE: fruit list -> real
	 PRE: true
	 POST: the total price of fruits, given applePrice, bananaprice and lemonPrice *)
      (* VARIANT: length of fruits *)
      fun sumPrice' [] = 0.0
	| sumPrice' (fruit::fruits) = (price fruit) + (sumPrice' fruits)
  in
      sumPrice' fruits
  end;

(* Exercise 4: Trees *)

(* An ltree consists of nodes with a label of type 'a, and a list of children. A leaf is
   a node with no children, i.e., an empty list of children. *)
datatype 'a ltree = Node of 'a * 'a ltree list;

(* sumList l sumSoFar
   TYPE: int list -> int -> int
   PRE: true
   POST: the sum of the integers in l, plus sumSoFar
   SIDE EFFECTS:
   EXAMPLES: sumList [] 42 = 42; sumList [1, 2, 3] 0 = 6;
 *)
(* VARIANT: length of l *)
fun sumList [] sumSoFar = sumSoFar
  | sumList (x::xs) sumSoFar = sumList xs (sumSoFar + x);

(* Using foldl it is easy to write a similar summation function:

   fun sumList' l = foldl op+ 0 l;

   In this case, the meaning of the fold is pretty clear, so this is probably a better solution.
 *)

(* count t
   TYPE: 'a ltree -> int
   PRE: true
   POST: the number of nodes in t
   SIDE EFFECTS:
   EXAMPLES: count(Node(1, [Node(2, []), Node(3, [])])) = 3;
 *)
(* VARIANT: size of t (i.e., the number of nodes in t, i.e., count t :-) *)
fun count (Node(_, children)) = 1 + (sumList (List.map count children) 0);

(* labels t
   TYPE: 'a ltree -> 'a list
   PRE: true
   POST: a preorder list with the labels of t
   SIDE EFFECTS:
   EXAMPLES: labels(Node(1, [Node(2, []), Node(3, [])])) = [1, 2, 3];
 *)
(* VARIANT: size of t (= count t) *)
fun labels (Node(x, children)) = x :: (List.concat (List.map labels children));

(* is_present' x t
   TYPE: ''a -> ''a ltree -> bool
   PRE: true
   POST: true iff x is a label in t
   SIDE EFFECTS:
   EXAMPLES: is_present' 1 (Node(2, [])) = false; is_present' 2 (Node(1, [Node(2, []), Node(3, [])])) = true;
 *)
(* VARIANT: size of t (= count t) *)
(* Note: the reason we introduce this function with the order of the arguments reversed is that
   it is easier to use map and similar functions when the data structure is the last argument. *)
fun is_present' x (Node(y, children)) = x = y orelse List.exists (is_present' x) children;

(* is_present t x
   TYPE: ''a ltree -> ''a -> bool
   PRE: true
   POST: true iff x is a label in t
   SIDE EFFECTS:
   EXAMPLES: is_present (Node(2, [])) 1 = false; is_present (Node(1, [Node(2, []), Node(3, [])])) 2 = true;
 *)
fun is_present tree x = is_present' x tree;

(* maxList l maxSoFar
   TYPE: int list -> int -> int
   PRE: true
   POST: the maximmum of the values in l and maxSoFar
   SIDE EFFECTS:
   EXAMPLES: maxList [1, 2, 3] 0 = 3; maxList [~1, ~2, ~3] 0 = 0;
 *)
(* VARIANT: length of l *)
fun maxList [] maxSoFar = maxSoFar
  | maxList (x::xs) maxSoFar = if x > maxSoFar then maxList xs x else maxList xs maxSoFar;

(* We can rewrite maxList using foldl:

   fun maxList l minValue = foldl Int.max minValue l;

   Again, in this case the fold is pretty clear, so this is probably the better solution.
 *)

(* height t
   TYPE: 'a ltree -> int
   PRE: true
   POST: the height of t, i.e., the maximum length of a path from the root to a leaf
   SIDE EFFECTS:
   EXAMPLES: height (Node(1, [])) = 1; height (Node(1, [Node(2, []), Node(3, [])])) = 2;
 *)
(* VARIANT: size of t (= count t) *)
fun height (Node(_, [])) = 1
  | height (Node(_, children)) = 1 + maxList (List.map height children) 0;
