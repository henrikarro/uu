(* linear and tail-recursive *)
fun member v [] = false
  | member v (x::xs) = (v=x) orelse member v xs;

(* linear but not tail-recursive *)
fun slow_length [] = 0
  | slow_length (x::xs) = 1 + slow_length xs;

(* quadratic and tail-recursive *)
fun distinct [] = true
  | distinct (x::xs) = not (member x xs) andalso distinct xs;

(* quadratic but not tail-recursive *)
fun slow_distinct [] = true
  | slow_distinct (x::xs) = distinct xs andalso not (member x xs);

(* a structure for counters *)
structure Counter =
struct
  type T = int
  fun make_counter () = 0
  fun inc c = c+1
  fun dec c = if c=0 then 0 else c-1
  fun is_zero c = c=0
end;

(* dot notation *)
42 : Counter.T;
Counter.make_counter ();
Counter.inc (Counter.make_counter ());
Counter.is_zero 42;

(* open *)
open Counter;
42 : T;
make_counter ();
inc (make_counter ());
is_zero 42;

(* local open *)
let
  open Counter
in
  make_counter ()
end;

local
  open Counter
in
  val zero_counter = make_counter ()
end;

(* counters implemented by unit lists *)
structure Counter =
struct
  type T = unit list
  fun make_counter () = []
  fun inc c = () :: c
  fun dec c = case c of [] => [] | _::cs => cs
  fun is_zero c = null c
end;

(* counters implemented by a datatype *)
structure Counter =
struct
  datatype T = EmptyCounter
             | UnitCounter of T
  fun make_counter () = EmptyCounter
  fun inc c = UnitCounter c
  fun dec (EmptyCounter) = EmptyCounter
    | dec (UnitCounter c) = c
  fun is_zero (EmptyCounter) = true
    | is_zero (UnitCounter _) = false
end;

(* counters implemented by odd integers *)
structure Counter =
struct
  type T = int
  fun make_counter () = 1
  fun inc c = c+2
  fun dec c = if c=1 then 1 else c-2
  fun is_zero c = c=1
end;

(* a signature for counters *)
signature COUNTER =
sig
  type T
  val make_counter: unit -> T
  val inc: T -> T
  val dec: T -> T
  val is_zero: T -> bool
end;

(* opaque ascription *)
structure Counter :> COUNTER =
struct
  type T = int
  fun make_counter () = 0
  fun inc c = c+1
  fun dec c = if c=0 then 0 else c-1
  fun is_zero c = c=0
end;

(* an ADT for stacks *)
signature STACK =
sig
  type 'a T
  val empty: 'a T
  val push: 'a -> 'a T -> 'a T
  val pop: 'a T -> 'a * 'a T
  exception Empty
end;

structure Stack :> STACK =
struct
  type 'a T = 'a list
  val empty = []
  fun push x s = x::s
  fun pop [] = raise Empty
    | pop (x::s) = (x,s)
  exception Empty
end;

Stack.empty;
Stack.push 42 Stack.empty;
Stack.pop (Stack.push 42 Stack.empty);

(* ill-formed
Stack.empty = [];
Stack.push 42 [];
Stack.pop [42];
*)

(* an ADT for dictionaries *)
signature DICTIONARY =
sig
  type (''a,'b) T
  val empty: (''a,'b) T
  val insert: ''a -> 'b -> (''a, 'b) T -> (''a, 'b) T
  val lookup: ''a -> (''a, 'b) T -> 'b option
end;

structure Dictionary :> DICTIONARY =
struct
  type (''a,'b) T = (''a * 'b) list
  val empty = []
  fun insert k v d = (k,v) :: d
  fun lookup k [] = NONE
    | lookup k ((k',v) :: d) = if k=k' then SOME v else lookup k d
end;

(* a functor example *)
signature INT = sig val x: int end;

functor Double(I: INT) =
struct
  val x = 2 * I.x
end;

structure Two = struct val x = 2 end;
structure Four = Double(Two);

Four.x;

(* a functor for stack-based postfix evaluation *)
datatype atom = Int of int | Plus | Times;

functor POSTFIX(S: STACK) =
struct
  fun eval xs =
  let
    fun eval' (Int i, s) = S.push i s
      | eval' (Plus, s) =
          let
            val (b, s) = S.pop s
            val (a, s) = S.pop s
          in
            S.push (a+b) s
          end
      | eval' (Times, s) =
          let
            val (b, s) = S.pop s
            val (a, s) = S.pop s
          in
            S.push (a*b) s
          end
    val (v, _) = S.pop (foldl eval' S.empty xs)
  in
    v
  end
end;

structure Postfix = POSTFIX(Stack);

Postfix.eval [Int 3, Int 4, Plus, Int 2, Times];
