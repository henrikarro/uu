(* Polymorphism *)

fun swap (x, y) = (y, x);

swap (1, 2);
swap ("a", "b");
swap (1, "b");

fun id x = x;

fun fst (x, _) = x;

fun snd (_, y) = y;

id;
id 1;
id "foo";
(id 1, id "foo");

val id = fn x => x;
val mono1 = let in fn x => x end;
val mono2 = id swap;

op =;

(op =): int * int -> bool;
(op =): real * real -> bool; (* Error *)

fun silly (a, b, c, d, e) = a=b orelse d<>e;


(* Lists *)

[18, 12, ~5, 12, 10];
[2.0, 5.3 - 1.2, 3.7, ~1E5];
["Bread", "Butter", "Milk"];
[(1,"A"), (2,"B")];
[Math.sin, fn x => x+1.0];
[[1], [2, 3]];
[];

[1, "not homogeneous"]; (* Error *)

[18, 3+9, 5-7, size "abc", 10];

[];
1 :: it;
2 :: it;
3 :: it;

[1, 2] = 1 :: 2 :: [];

[];
op::;

val [x, y, z] = [1, 2, 3];
val [_, y, 3] = [1, 2, 3];
val [x, y, z] = [1, 2]; (* Exception *)

val x :: xs = [1, 2, 3];
val x :: y :: _ = [1, 2, 3]
val x :: xs = []; (* Exception *)

fun null [] = true
  | null _ = false;

fun hd (x::_) = x
  | hd _ = raise Empty;

fun tl (_::xs) = xs
  | tl _ = raise Empty;

(* length xs
   TYPE: 'a list -> int
   PRE: true
   POST: the length of xs
 *)
fun length [] = 0
  | length (_::xs) = 1 + length xs;

(* last xs
   TYPE: 'a list -> 'a
   PRE: xs is non-empty
   POST: the last element of xs
 *)
fun last [x] = x
  | last (_::xs) = last xs
  | last [] = raise Empty;

(* append xs ys
   TYPE: 'a list * 'a list -> 'a list
   PRE: true
   POST: a list consisting of all elements of xs followed by all elements of ys
 *)
fun append ([], ys) = ys
  | append (x::xs, ys) = x :: append (xs, ys);

(* rev xs
   TYPE: 'a list -> 'a list
   PRE: true
   POST: xs in reverse order
 *)
fun rev [] = []
  | rev (x::xs) = append (rev xs, [x]);

(* member (x, ys)
   TYPE: ''a * ''a list -> bool
   PRE: true
   POST: true iff x is a member of (i.e., contained in) ys
 *)
fun member (x, []) = false
  | member (x, y::ys) = x=y orelse member (x, ys);

null;
hd;
tl;
length;
List.last;
rev;
op @;

explode "hello";
implode [#"h", #"e", #"l", #"l", #"o"];
concat ["he", "ll", "o"];

[1] = [1];
[1,2] = [2,1];
[1] = [1,1];
[1.0] = [1.0]; (* Error *)

(* Merge Sort *)

(* split xs
   TYPE: 'a list -> 'a list * 'a list
   PRE: true
   POST: a pair of lists (ys,zs) such that the length of ys and zs
     differs by at most 1, and ys @ zs is some permutation of xs
*)
fun split [] =
      ([], [])
  | split [x] =
      ([x], [])
  | split (x1::x2::xs) =
      let
        val (xs1,xs2) = split xs
      in
        (x1::xs1, x2::xs2)
      end;

split [1,2,3,4,5];

(* merge (xs,ys)
   TYPE: int list * int list -> int list
   PRE: xs and ys are sorted
   POST: a sorted permutation of xs @ ys
 *)
fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x::xs, y::ys) =
      if x <= y then
        x :: merge (xs, y::ys)
      else
        y :: merge (x::xs, ys);

merge ([1,3,5], [2,4]);

(* mergesort xs
   TYPE: int list -> int list
   PRE: true
   POST: a sorted permutation of xs
 *)
fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs =
      let
        val (ys,zs) = split xs
      in
        merge (mergesort ys, mergesort zs)
      end;

mergesort [1,3,5,2,4];
