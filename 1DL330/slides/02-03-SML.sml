(* Types *)

([abs, ~], ("cool", 3.5));

fun double x = 2 * x;
double 3.0; (*Error*)
2 * 3.0; (* Error *)


(* Literals *)

0;
~1;
42;
~0x2A;

0.0;
~15.5E3;
15E~2;

2 + 3;
2.0 + 3.0;

#"a";
#" ";
#"4";

"Hello!\nGoodbye";


(* Lazy evaluation *)

34 < 649 orelse Math.ln 12.4 * 3.4 > 12.0;
34 < 649 orelse 1 div 0 > 99;
1 div 0 > 99; (*Exception*)

if 1 + 2 < 4 then size ("hel" ^ "lo!") else 4 div 2;


(* Value declarations *)

val a = 1;
val pi = 3.14159;
val two_pi = 2.0 * pi;
val &@!+< = 42;

two_pi;
a + a;
&@!+< div 7;

val sum = 24;
val sum = 3.51;

val a = 1;
val b = 2;
val a = a+b val b = a+b;

val a = 1 val b = 2;
val a = a+b and b = a+b;

val x = 10;
fun addX y = x+y;
addX 5;
x = 100;
val x = 100;
addX 5;


(* Tuples *)

(22>5, "abc", 123);

(2.3, 5);

val bigTuple = ((2.3, 5), "two", (8, true)) ;
#3 bigTuple ;
#2 (#1 bigTuple) + #1 (#3 bigTuple);


(* Functions *)

fun double x = 2 * x;
fun even x = x mod 2 = 0;
val even = fn x => x mod 2 = 0; (* anonymous function *)
fun odd x = not (even x); (* using another function *)

odd 17 orelse even 17;

fun even x = x mod 2 = 0;
val plop = even;
plop 3;
(fn x => x mod 2 = 1) 3;
even 3 + 4; (*Error*)

fun max (a,b) = if a > b then a else b;
fun max a b = if a > b then a else b;

fun max1 (a,b) = if a > b then a else b;
val max1 = fn (a,b) => if a > b then a else b;

fun max2 a b = if a > b then a else b;
val max2 = fn a => fn b => if a > b then a else b;

val posOrZero = max2 0;
posOrZero 3;
posOrZero ~3;

fun greet word name = word ^ ", " ^ name ^ "!";
val greetEng = greet "Hello";
val greetSwe = greet "Hej";
greetEng "Tjark";
greetSwe "Kjell";

fun id x = x;
id 5;
id 3.5;

fun sqr x = x * x;
fun sqr x = (x:real) * x;
fun sqr (x:real) = x * x;
fun sqr x:real = x * x; (* (sqr x):real *)
fun sqr x = x:real * x; (*Error*)

fun id x = x;
val iidd = id id;
iidd 1; (*Error*)

(* triangle n
   TYPE: int -> int
   PRE: n>=0
   POST: sum_{0<=i<=n}(i)
   SIDE-EFFECTS: none
   EXAMPLES: triangle 0 = 0, triangle 3 = 6
*)
fun triangle n =
  if n = 0 then 0 else n + triangle (n-1);


(* Pattern matching *)

val x = (18, true);
val (n, b) = x;
val (n, _) = x;
val (18, b) = x;
val (17, b) = x; (*Exception*)

val t = ((3.5, true), 4);
val (d as (a, b), c) = t;
val (d, c) = t;
val ((a, b), c) = t;
val s as (d, c) = t;
val s as u as v = t;
val (t, d) = t; (* t is bound to a different value after this *)

(* sign x
   TYPE: int -> int
   PRE: true
   POST: ~1 if x<0, 0 if x=0, 1 if x>0
   SIDE-EFFECTS: none
   EXAMPLES: sign ~42 = ~1, sign 0 = 0, sign 42 = 1
 *)
fun sign x =
  if x = 0 then 0 else if x < 0 then ~1 else 1;

fun sign 0 = 0
  | sign x = if x < 0 then ~1 else 1;

(*clause order is important*)
fun sign x = if x < 0 then ~1 else 1
  | sign 0 = 0;

(* is_zero x
   TYPE: int -> bool
   PRE: true
   POST: true if x=0, false otherwise
   SIDE-EFFECTS: none
   EXAMPLES: is_zero 0 = true, is_zero 1 = false
 *)
fun is_zero 0 = true
  | is_zero x = false;

fun is_zero 0 = true
  | is_zero _ = false;

fun bool_string true = "true"
  | bool_string false = "false";

bool_string true;
bool_string false;

fun bool_string' truw = "true"
  | bool_string' false = "false";

bool_string' true;
bool_string' false;

fun is_zero 0 = true;
is_zero 0;
is_zero 1; (*Exception*)

fun is_zero 0 = true
  | is_zero _ = false;

fun equal (x, y, z) = x = y andalso y = z;

(*Error*)
fun equal (x, x, x) = true
  | equal _         = false;

case 17 mod 2 of
  0 => "even"
| 1 => "odd";

case 17 mod 2 of
  0 => "even"
| _ => "odd";

fun multiply (x, y) =
  case x*y of
    1 => "one"
  | 2 => "two"
  | x => Int.toString x;

multiply (3, 4);


(* Local declarations *)

let
  val x = 1
in
  x + 10
end;
x; (*Error*)

val x = 0;
val y =
  let
    val x = 1
  in
    x + 10
  end;
(x, y);

fun discount unit_price quantity =
  let
    val price = unit_price * real quantity
  in
    if price < 100.0 then price else 0.95 * price
  end;

fun is_leap_year year =
  let
    fun is_divisible b = year mod b = 0
  in
    is_divisible 4 andalso
      (not (is_divisible 100) orelse is_divisible 400)
  end;


(* Infix operators *)

fun x (a,b) = a*b;
infix 5 x;
2 x 4;

nonfix x;
x (2,4);

2 + 4;
op +;
(op +) (2,4);

(* Side effects *)

fun welcome name = print ("Hello, " ^ name ^ "!\n");
welcome "world";

(1; 2.0; "three");


(* Exceptions *)

1 div 0;
1 div 0 handle Div => 42;
exception errorDiv;
fun safeDiv a b =
   if b = 0 then raise errorDiv
   else a div b;


(* Modules *)

Int.toString;
Int.+;
Int.abs;
Real.Math.sqrt;
val sqrt = Math.sqrt;
