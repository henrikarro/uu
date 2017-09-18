32+15;
3.12+4.1;
10 - 100;
not true;
"fun" ^ "ctional";
size("hello");
size "hello";
if 2+2=5 then "hello" else "goodbye";

val a = 1;
val pi = 3.14159;
val two_pi = 2.0 * pi;
val &@!+< = 42;

two_pi;
a + a;
&@!+< div 7;

val sum = 24;
val sum = 3.51;

fun sqr x = x*x;
fun abs x = if x >= 0 then x else ~x;
fun abs x = if x >= 0.0 then x else ~x;
fun test x = if x<0 then "negative" else "non-negative";

(1, 2, 3);
(1.1, 1.2);
("foo", "bar");
(3, "three", 3.0);

fun average (a,b) = (a+b)/2.0;
fun max (a,b) = if a > b then a else b

fun average a b = (a+b)/2.0;
fun max a b = if a > b then a else b;

fun triangle n =
  if n=0 then 0 else n+triangle (n-1);

fun silly x = silly x;

[];
3 :: [];
2 :: 3 :: [];
1 :: 2 :: 3 :: [];

[];
[3];
[2,3];
[1,2,3];

fun sum [] = 0
  | sum (x::xs) = x + sum xs;

sum [];
sum [1,2,3];
