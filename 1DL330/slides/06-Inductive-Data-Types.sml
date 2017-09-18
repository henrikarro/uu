(* New Names for Old Types *)

type float = real;

type mylist = (char * int) list;

[(#"A", 1)] : mylist;

([(#"A", 1)] : mylist) = ([(#"A", 1)] : (char * int) list);

type ('a, 'b) assoc_list = ('a * 'b) list;

(*
type predicate = 'a -> bool;
*)

type 'a predicate = 'a -> bool;

op< : (int * int) predicate;


(* New Types *)

datatype direction = North | South | East | West;

North;

(*
datatype order = LESS | EQUAL | GREATER;
*)

fun compare (a, b) =
    if a < b then
      LESS
    else if a = b then
      EQUAL
    else (* a > b *)
      GREATER;

(* opposite d
     TYPE: direction -> direction
     PRE: true
     POST: the direction opposite d
     EXAMPLES: opposite North = South
 *)
fun opposite North = South
  | opposite South = North
  | opposite East = West
  | opposite West = East;

(* sign n
   TYPE: int -> int
   PRE: true
   POST: ~1 if n<0, 0 if n=0, 1 if n>0
   EXAMPLES: sign 42 = 1
 *)
fun sign n =
  case Int.compare (n, 0) of
    LESS => ~1
  | EQUAL => 0
  | GREATER => 1;

datatype rational = Rat of int * int;

Rat (2,3);

(* qadd (x, y)
   TYPE: rational * rational -> rational
   PRE: x and y are rational numbers
        (with denominator <> 0)
   POST: the sum of x and y
   EXAMPLES: qadd (Rat (1,2), Rat (1,3)) = Rat (5,6)
 *)
fun qadd (Rat (a,b), Rat (c,d)) =
  Rat (a*d + b*c, b*d);

datatype shape = Circle of real
                 | Square of real
                 | Triangle of real * real * real;

Circle 1.0;

Square (1.0 + 1.0);

Triangle (3.0, 4.0, 5.0);

(* area s
   TYPE: shape -> real
   PRE: true
   POST: the area of s
   EXAMPLES: area (Circle 1.0) = 3.141592654
 *)
fun area (Circle r) = Math.pi * r * r
  | area (Square l) = l * l
  | area (Triangle (a, b, c)) =
      let
        val s = (a + b + c) / 2.0
      in
        (* Heron's formula *)
        Math.sqrt (s * (s-a) * (s-b) * (s-c))
      end;

North;
Circle;
Square;
Triangle;

datatype ('a) option = NONE | SOME of 'a;

NONE;
SOME;
SOME 42;
SOME "foo";
SOME [];

Int.fromString "42";
Int.fromString "foo";

(*
datatype option = NONE | SOME of 'a;
*)

datatype 'a option = NONE | SOME of 'a;

NONE : int option;

datatype aexp = Int of int
                | Plus of aexp * aexp
                | Times of aexp * aexp;

(* eval e
   TYPE: aexp -> int
   PRE: true
   POST: the value of e
   EXAMPLES: eval (Plus (Int 1, Int 2)) = 3
 *)
(* VARIANT: size of e *)
fun eval (Int i) = i
  | eval (Plus (x, y)) = eval x + eval y
  | eval (Times (x, y)) = eval x * eval y;

Int 1 = Int 1;
Int 1 = Int 2;
Plus (Int 1, Int 2) = Int 3;
Plus (Int 1, Int 2) = Plus (Int 1, Int 2);

(*
Circle 1.0 = Square 1.0;
NONE = SOME 1.0;
*)
NONE = SOME 1;

datatype 'a fbtree = Leaf of 'a
                   | Node of 'a fbtree * 'a * 'a fbtree;

Leaf "Grandfather";
Node (Leaf "Grandfather", "Father", Leaf "Grandmother");

(* root_value t
   TYPE: 'a fbtree -> 'a
   PRE: true
   POST: the value at t's root node
   EXAMPLES: root_value (Leaf "foo") = "foo"
 *)
fun root_value (Leaf x) = x
  | root_value (Node (_, x, _)) = x;

(* height t
   TYPE: 'a fbtree -> int
   PRE: true
   POST: the height of t
   EXAMPLES: height (Leaf "foo") = 0
 *)
(* VARIANT: size of t *)
fun height (Leaf _) = 0
  | height (Node (l, _, r)) =
      1 + Int.max (height l, height r);

(* mirror t
   TYPE: 'a fbtree -> 'a fbtree
   PRE: true
   POST: the mirror image of t
   EXAMPLES: mirror (Node (Leaf 1, 2, Leaf 3)) =
               Node (Leaf 3, 2, Leaf 1)
 *)
(* VARIANT: size of t *)
fun mirror (Leaf x) = Leaf x
  | mirror (Node (l, x, r)) = Node (mirror r, x, mirror l);
