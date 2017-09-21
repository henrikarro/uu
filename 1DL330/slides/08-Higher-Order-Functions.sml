(* Definition, Introductory Examples *)

fun time f =
  let
    val timer = Timer.startCPUTimer ()
    val _ = f () (* do the actual work *)
  in
    Timer.checkCPUTimes timer
  end;

op o;
fun add1 x = x + 1;
(add1 o add1) 42;

fun twice f = f o f;
twice add1 42;
twice (twice add1) 42;

fun ntimes 0 f x = x
  | ntimes n f x = ntimes (n-1) f (f x);

ntimes 3 add1 42;
ntimes 3 twice add1 42;

fun insert order x [] = [x]
  | insert order x (y::ys) =
    if order(x,y) then
        x :: y :: ys
    else
        y :: (insert order x ys);

fun isort order [] = []
  | isort order (x::xs) =
    insert order x (isort order xs);

isort (op <) [6,3,0,1,7,8,5,9,2,4];
isort (op >) [6,3,0,1,7,8,5,9,2,4];
isort String.< ["one","two","three","four","five","six","seven"];
isort (op <);
isort String.< ;
isort (fn ((_,s1),(_,s2)) => String.< (s1,s2)) [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5,"five"), (6,"six"), (7,"seven")];

fun pair x y f = f x y;

fun frst p = p (fn x => fn y => x);

fun snd p = p (fn x => fn y => y);

val pair' = (fn x => fn y => fn f => f x y)
            : 'a -> 'a -> ('a -> 'a -> 'a) -> 'a;



(* Higher-Order Functions on Lists *)

fun foldr f b [] = b
 |  foldr f b (x::l) = f (x, foldr f b l);

fun sum xs = foldr (op +) 0 xs;
val sum = foldr (op +) 0;

foldr (op * ) 1 [1,2,3,4];

foldr Int.max 0 [1,2,3,44,5,6];

foldr (fn (x,ys) => x::ys) [] [1,2,3,4];

foldr (fn (x,ys) => x::ys) [5,6,7] [1,2,3,4];

foldr (fn (x,ys) => x::x::ys) [] [1,2,3,4];

foldr (op @) [] [[1,2], [34], [5,6,7,89]];

map;
foldr;
foldl;
List.filter;

(*
function map f L
TYPE: 'a -> 'b -> 'a list -> 'b list
PRE: (none)
POST: [f(a_1),f(a_2), ...,f(a_n)], if L = [a_1,a_2, ...,a_n]

Variant: length of L
*)



fun map f [] = []
  | map f (x::xs) = f x :: map f xs;

fun square x = x*x;
map square [1,2,3,4];
map (fn x => if x < 3 then 0 else x) [1,2,3,4];
map square;
map (map square);
map (map square) [[1,2,34],[5]];
map String.<;
map String.< [("a","ab"), ("hello", "bye")];

fun foldl f b [] = b
 |  foldl f b (x::xs) = foldl f (f (x,b)) xs;

fun foldr f b [] = b
 |  foldr f b (x::xs) = f (x, foldr f b xs);

foldl (op +) 0 [0,2,21,4,6];
foldr (op +) 0 [0,2,21,4,6];

foldl (fn (x,y) => y andalso x mod 2 = 0) true [0,2,21,4,6];
foldr (fn (x,y) => y andalso x mod 2 = 0) true [0,2,21,4,6];

fun all_even [] = true
  | all_even (x::xs) = x mod 2 = 0 andalso all_even xs;

all_even [0,2,21,4,6];

foldl (op ::) [];
foldr (op ::) [];


(*
function foldr f x l
TYPE: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
PRE: (none)
POST: [f(a_1, f(a_2, ...,f(a_n, x)...))], if l = [a_1,a_2, ...,a_n]

Variant: length of l

function foldr f x l
TYPE: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
PRE: (none)
POST: [f(a_n, f(a_{n-1}, ...,f(a_1, x)...))], if l = [a_1,a_2, ...,a_n]

Variant: length of l
*)                       

foldr (fn (x, y) => "["^x^","^y^"]") "0" ["a", "b", "c"];
foldl (fn (x, y) => "["^x^","^y^"]") "0" ["a", "b", "c"];


fun even_or_none (x, NONE) =
    if x mod 2 = 0 then SOME x else NONE
  | even_or_none (_, some_x) =
    some_x;

foldl even_or_none NONE;
foldr even_or_none NONE;

foldl (fn (x,n) => n+1) 0;
foldr (fn (x,n) => n+1) 0;

foldl (fn (x,n) => x) 0;
foldr (fn (x,n) => x) 0;

fun mystery f =
  foldr (fn (x,n) => f x :: n) [];

fun filter p [] = []
  | filter p (x::xs) =
    if p x then
        x :: filter p xs
    else
        filter p xs;

(*
function filter p L
TYPE: ('a -> bool) -> 'a list -> 'a list
PRE: (none)
POST: the list of elements of L for which p is true

Variant: length of L
*)


                                
filter (fn x => x<6) [6,3,0,1,8,5,9,3];
filter (fn x => x<6);


(* Polymorphic Ordered Binary Tree *)

datatype 'a obtree = Void | Node of 'a obtree * 'a * 'a obtree;

(* search compare x t
   TYPE: ('a * 'a -> order) -> 'a -> 'a obtree -> bool
   PRE: t is ordered (wrt. compare)
   POST: true iff t contains x
 *)
fun search compare _ Void =
      false
  | search compare x (Node (l,v,r)) =
      case compare (x,v) of
        EQUAL => true
      | LESS => search compare x l
      | GREATER => search compare x r;

val obtree =
  Node (
    Node (
      Node (Void, 2, Void),
      3,
      Node (Void, 5, Void)
    ),
    7,
    Node (
      Void,
      10,
      Node (Void, 12, Void)
    )
  );

search Int.compare 5 obtree;

datatype 'a obtree_with_order =
  TreeWithOrder of ('a * 'a -> order) * 'a obtree;

(* search x t
   TYPE: 'a -> 'a obtree_with_order -> bool
   PRE: true
   POST: true iff t contains x

Varinant: height of t.

 *)
fun search x (TreeWithOrder (compare, obtree)) =
  let
    fun search' Void = false
      | search' (Node (l,v,r)) =
          case compare (x,v) of
            EQUAL => true
          | LESS => search' l
          | GREATER => search' r
  in
    search' obtree
  end;

val obtree_with_order = TreeWithOrder (Int.compare, obtree);

search 5 obtree_with_order;

(* Higher-Order Functions on Trees *)

datatype 'a btree = Void | Node of 'a * 'a btree * 'a btree;

fun map_btree f Void =
      Void
  | map_btree f (Node (x,l,r)) =
      Node (f x, map_btree f l, map_btree f r);

map_btree;
map_btree Int.toString;
map_btree Int.toString (Node (1, Void, Node (2, Void, Void)));

fun fold_btree n v Void =
      v
  | fold_btree n v (Node (x,l,r)) =
      n (x, fold_btree n v l, fold_btree n v r);

fold_btree (fn (x,l,r) => 1 + Int.max (l,r)) 0;
fold_btree (fn (x,l,r) => Node (x,r,l)) Void;
fold_btree (fn (x,l,r) => l @ [x] @ r) [];
