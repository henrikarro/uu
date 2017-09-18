fun second xs = hd (tl xs);

fun last [x] = x
  | last (x::xs) = last xs;

fun reverse [] = []
  | reverse (x::xs) = (reverse xs) @ [x];

fun rev' acc [] = acc
  | rev' acc (x::xs) = rev' (x::acc) xs;

fun rev xs = rev' [] xs;

fun time f =
  let
    val timer = Timer.startCPUTimer ()
    val _ = f () (* do the actual work *)
  in
    Timer.checkCPUTimes timer
  end;

fun make 0 = []
  | make n = n :: make (n-1);

val xs = make 10000;
time (fn () => reverse xs);

val xs = make 20000;
time (fn () => reverse xs);

val xs = make 10000;
time (fn () => rev xs);

val xs = make 20000;
time (fn () => rev xs);

val xs = make 10000000;
time (fn () => rev xs);

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2);

(* ffib n
   TYPE: int -> int * int
   PRE: n >= 0
   POST: (fib(n), fib(n+1))
 *)
fun ffib 0 = (0, 1)
  | ffib n = let val (a, b) = ffib (n-1) in (b, a+b) end;

fun length [] = 0
  | length (x::xs) = 1 + length xs;

length [1,2,3];

fun length' acc [] = acc
  | length' acc (x::xs) = length' (acc+1) xs;

fun length'' xs = length' 0 xs;

length'' [1,2,3];

fun member v [] = false
  | member v (x::xs) = (v=x) orelse member v xs;

val xs = make 100000000;
time (fn () => length xs);
time (fn () => length'' xs);

(* tfib' (m, p, q, n)
   PRE: 1 <= m <= n, fib(m-1)=p, fib(m)=q
   POST: fib(n)
 *)
fun tfib' (m, p, q, n) =
  if m = n then q else tfib' (m+1, q, p+q, n);

fun tfib 0 = 0
  | tfib n = tfib' (1, 0, 1, n);

fun fac_end r i n = r;

fun fac_loop r i n =
  if i>n then
    fac_end r i n
  else
    fac_loop (r*i) (i+1) n;

fun fac n = fac_loop 1 1 n;

datatype 'a bTree =
  Leaf
| Node of 'a bTree * 'a * 'a bTree;

Node (Node (Leaf, 5, Leaf),
      8,
      Node (Node (Leaf, 2, Leaf),
            4,
            Leaf));

fun inorder Leaf =
    []
  | inorder (Node (l, x, r)) =
    (inorder l) @ (x :: inorder r);

fun inorder' acc Leaf =
    acc
  | inorder' acc (Node (l, x, r)) =
    inorder' (x :: inorder' acc r) l;

fun inorder t = inorder' [] t;
