(* Functional Programming 1DL330
   Assignment 3

   Henrik Arro
   Imad Collin
 *)

(* Excercises 1 and 2: Tail Recursion with Specification *)

(* average list
   TYPE: real list -> real
   PRE: true
   POST: average of the elements of list, i.e., the sum of the elements divided
   by the number of elements. Average of the empty list is defined to be 0.0,
   even though I personally would prefer NAN since 0.0 is a valid result for
   non-empty lists.
   SIDE EFFECTS:
   EXAMPLES: average [] =~ 0.0; average [1.0, 4.0, 10.0] =~ 5.0;
 *)
fun average [] = 0.0
  | average l = 
    let
	(* average' list sum len
	   TYPE: real list -> real -> int -> real
	   PRE: (length list + len) > 0
	   POST: (sum of the elements of list + sum) / (length of list + len)
	   SIDE EFFECTS:
	   EXAMPLES: average' [1.0, 4.0, 10.0] 0.0 0 =~ 5.0; average' [4.0, 10.0] 1.0 1 =~ 5.0; average' [10.0] 5.0 2 =~ 5.0;
	 *)
	(* VARIANT: length list *)
	fun average' [] sum len = sum / real len
	  | average' (x::xs) sum len = average' xs (x + sum) (len + 1)
    in
	average' l 0.0 0
    end;

(* Exercise 3: Use of Higher-Order Functions *)

fun append list1 list2 = foldr op:: list2 list1;

fun member elem list = foldl (fn (x, y) => y orelse x = elem) false list;

fun last (elem::rest) = foldl (fn (x, _) => x) elem rest;

fun reverse list = foldl op:: [] list;

fun filter pred list = foldr (fn (x, y) => if pred x then x::y else y) [] list;

(* Exercise 4: Binary Search Trees *)

(* REPRESENTATION CONVENTION: a binary search tree is either empty or consists of a key and two sub-trees.
   REPRESENTATION INVARIANT:  In a binary search tree (Node(t1, x, t2)), all keys in t1 are smaller than x,
   and all keys in t2 are greater than x.
 *)
datatype tree = Void | Node of tree * int * tree;

(* sub_tree m n t
   TYPE: int -> int -> tree -> tree
   PRE: true
   POST: a tree with all keys k in t so that m <= k < n
   SIDE EFFECTS:
   EXAMPLES: sub_tree 0 1 Void = Void;
   sub_tree 0 1 (Node(Node(Void, 0, Void), 1, Node(Void, 2, Void))) = Node(Void, 0, Void);
   sub_tree 0 2 (Node(Node(Void, 0, Void), 1, Node(Void, 2, Void))) = Node(Node(Void, 0, Void), 1, Void);
 *)
fun sub_tree m n t =
  let
      (* sub_tree' t
	 TYPE: tree -> tree
	 A helper function that has the same specification as sub_tree, but without
	 the m and n arguments, which never change. This simplifies the recursive calls,
	 making the code a little bit clearer. *)
      fun sub_tree' Void = Void
	| sub_tree' (Node(t1, x, t2)) =
	  if x < m then sub_tree' t2
	  else if x >= n then sub_tree' t1
	  else (* m <= x < n *) Node(sub_tree' t1, x, sub_tree' t2)
  in
      sub_tree' t
  end;

(* Exercise 5: Complexity

   The sub_tree function needs to find all the nodes that should be in the sub tree,
   and also the two nodes that are "just outside" the sub tree to the left and to the
   right, if they exist. Since we assume that the lower and upper limits are fixed,
   the size of the sub tree is fixed. This means that the time to run sub_tree is a
   constant factor of the time to search for a node.

   The time complexity to search in a binary search tree is linear in the depth of
   the tree, i.e., O(depth t). If the tree is balanced, this means the average time
   to search for a node is O(log n), where n is the size of the tree. In turn, this
   means the time build a sub tree is a constant factor of this, i.e., also O(log n).
   
   However, if the tree is degenerate, it will be a kind of linked list:

   val degenerate = Node(Void, 0, Node(Void, 1, Node(Void, 2, Node(Void, 3, Node(Void, 4, Node(Void, 5, Void))))));

   In the case of a degenerate tree, the depth of the tree is linear in the number
   of nodes, so the worst case time complexity is O(n), where n is the size of the tree.

   The actual worst case would occur if a and b are chosen so that sub_tree needs
   to chase down the entire "list" while creating a copy, so for example "sub_tree 0 6 degenerate"
   is an example of a worst case scenario.
 *)

(* The following is not part of the assignment, but a continuation of my question if it is
   possible to replace all recursion with fold.

   I tried to create the everySecond function, which gives the first, third, fifth, and so on,
   elements of a list. That function uses map, filter, zip, tabulate and length. I then tried
   to implement these functions using only fold. It turns out that foldr on lists and list pairs
   gets you a long way, but you also need a way to construct new lists. Since SML does not have
   list comprehensions, it seems to be necessary to introduce a recursive function like
   "between m n" that gives a list of numbers between m and n.

   Anyway, this is what I came up with, replacements for the builtin list and list pair functions
   using only foldr, and the between function. *)

fun map' f l = foldr (fn (x, y) => f x::y) [] l;
fun filter' pred l = foldr (fn (x, y) => if pred x then x::y else y) [] l;
fun length' l = foldr (fn (x, y) => 1 + y) 0 l;
fun zip' (l1, l2) = ListPair.foldr (fn (x, y, z) => (x, y)::z) [] (l1, l2);
fun between m n = if n < m then [] else m :: between (m + 1) n;
fun tabulate' (n, f) = map' f (between 0 (n - 1));
	
fun everySecond l = map' #1 (filter' (fn (x, y) => y = 0) (zip' (l, tabulate' (length' l, fn x => x mod 2))));
