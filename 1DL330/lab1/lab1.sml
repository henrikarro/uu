(* Excercise 1 *)

(* 1.1

   product 3 ->
   (fn n => if n = 1 then 1 else n * product (n - 1)) 3 ->
   (if 3 = 1 then 1 else 3 * product (3 - 1)) ->
   (if false then 1 else 3 * product (3 - 1)) ->
   (3 * product (3 - 1)) ->
   (3 * product 2) ->
   (3 * (fn n => if n = 1 then 1 else n * product (n - 1)) 2) ->
   (3 * (if 2 = 1 then 1 else 2 * product (2 - 1))) ->
   (3 * (if false then 1 else 2 * product (2 - 1))) ->
   (3 * (2 * product (2 - 1))) ->
   (3 * (2 * product 1)) ->
   (3 * (2 * (fn n => if n = 1 then 1 else n * product (n - 1)) 1)) ->
   (3 * (2 * (if 1 = 1 then 1 else 1 * product (1 - 1)))) ->
   (3 * (2 * (if true then 1 else 1 * product (1 - 1)))) ->
   (3 * (2 * (1))) ->
   (3 * (2)) ->
   6
 *)

(* 1.2

   The function computes the product of 1 .. n, i.e., n! (the factorial of n), for n > 0.
 *)

(* 1.3

   product n
   TYPE: int -> int
   PRE:  n > 0
   POST: n!
   SIDE EFFECTS:
   EXAMPLES: product 1 = 1; product 2 = 2; product 3 = 6; product 6 = 720
 *)

(* 1.4

   VARIANT: n
*)

(* Exercise 2 *)

val minus = fn x => fn y => x - y;

val foo = minus 5 4; (* foo = 1 *)

val bar = minus 5; (* bar = fn y => 5 - y, i.e., bar is a function that calculates 5 minus its argument. *)

(* minus 5 4 ->
   (fn x => fn y => x - y) 5 4 ->
   (fn y => 5 - y) 4 ->
   (5 - 4) ->
   1
 *)

(* Exercise 3 *)

fun fun1 n = n + 1;

fun fun2 m n = m + n;

fun fun3 n = (n, n + 1);

fun fun4 (m, n) = m + n;

fun fun5 n x s = s ^ ": n=" ^ Int.toString n ^ ", x=" ^ Real.toString x;

fun fun6 (m, (s1, s2, n)) = (m + n, s1 ^ s2);

(* Exercise 4 *)

(* m divides n
   TYPE: (int * int) -> bool
   PRE: m <> 0
   POST: true iff m divides n with no remainder
   SIDE EFFECTS: Exception Div if m = 0
   EXAMPLES: 2 divides 3 = false; 2 divides 4 = true
 *)
infix divides;
fun m divides n = n mod m = 0;

(* between m n
   TYPE: int -> int -> int list
   PRE: true
   POST: list of the integers m .. n, if m <= n, otherwise the empty list
   SIDE EFFECTS:
   EXAMPLES: between 2 3 = [2, 3]; between 2 2 = [2]; between 3 2 = []
 *)
(* VARIANT: size of the set of integers between m and n *)
fun between m n =
  if n < m then [] else m :: between (m + 1) n;

(* smallestSoThat pred n
   TYPE: (int -> bool) -> int -> int
   PRE: There is m >= n so that (pred m) is true
   POST: The smallest m >= n so that (pred m) is true
   SIDE EFFECTS:
   EXAMPLES: smallestSoThat (fn x => x > 10) 1 = 11
 *)
(* VARIANT: m - n *)
fun smallestSoThat pred n =
  if pred n then n else smallestSoThat pred (n + 1);

(* lcm n
   TYPE: int -> int
   PRE: n > 0
   POST: the smallest m so that m is divisible by 1 .. n
   SIDE EFFECTS: Exception Domain if n <= 0
   EXAMPLES: lcm 1 = 1; lcm 3 = 6; lcm 10 = 2520
 *)
fun lcm n =
  let
      (* candidateOk m
	 TYPE: int -> bool
	 PRE: n >= 0
	 POST: true if m is divisible by 1 .. n *)
      fun candidateOk candidate =
	List.all (fn i => i divides candidate) (between 2 n)
  in
      if n < 1 then
	  raise Domain
      else
	  smallestSoThat candidateOk 1
  end;

(* Not part of the assignment, it just occurred to me that the functions smallestSoThat and divides
   can be used to succinctly test for primality. *)

(* isPrime n
   TYPE: int -> bool
   PRE: n > 1
   POST: true iff n is prime
   SIDE EFFECTS:
   EXAMPLES: isPrime 2 = true; isPrime 3 = true; isPrime 4 = false
 *)
fun isPrime n =
  smallestSoThat (fn x => x divides n) 2 = n;

