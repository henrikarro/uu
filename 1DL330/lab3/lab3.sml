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
	   PRE: length list > 0
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

fun member elem list = foldl (fn (x, y) => x = elem orelse y) false list;

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
	 the m and n arguments, which never change. This simplifies the recurive calls,
	 making the code a little bit clearer. *)
      fun sub_tree' Void = Void
	| sub_tree' (Node(t1, x, t2)) =
	  if x < m then sub_tree' t2
	  else if x >= n then sub_tree' t1
	  else (* m <= x < n *) Node(sub_tree' t1, x, sub_tree' t2)
  in
      sub_tree' t
  end;

fun depth Void = 0
  | depth (Node(t1, _, t2)) = 1 + Int.max (depth t1, depth t2);

(* Exercise 5: Complexity

   The time complexity of the function sub_tree is linear in the depth of the tree,
   i.e., O(depth t). If the tree is balanced, this means the average time to build
   a sub tree is O(log n), where n is the size of the tree.

   However, if the tree is degenerate, it will be a kind of linked list:

   val degenerate = Node(Void, 0, Node(Void, 1, Node(Void, 2, Node(Void, 3, Node(Void, 4, Node(Void, 5, Void))))));

   In the case of a degenerate tree, the depth of the tree is linear in the number
   of nodes, so the worst case time complexity is O(n), where n is the size of the tree.

   The actual worst case would occur if a and b are chosen so that sub_tree needs
   to chase down the entire "list" while creating a copy, so for example "sub_tree 0 6 degenerate"
   is an example of a worst case scenario.
 *)
