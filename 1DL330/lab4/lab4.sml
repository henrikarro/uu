(* Functional Programming 1DL330
   Assignment 4

   Henrik Arro
 *)

(* Exercise 1: A Signature for Valuations *)

(* The following dictionary code using binary trees is taken from
 * http://www.cl.cam.ac.uk/~lp15/MLbook/programs/sample4.sml.
 *
 * The only addition is the function keys to list all keys in a dictionary.
 *
 * The comments here are from the original file, I have not added any extra
 * comments since I consider this to be "library code".
 *)

(*** Binary trees ***)

datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

(*** Dictionaries as Binary search trees ***)

signature DICTIONARY = 
sig
    type key				(*type of keys*)
    type 'a t				(*type of tables*)
    exception E of key			(*errors in lookup, insert*)
    val empty: 'a t			(*the empty dictionary*)
    val lookup: 'a t * key -> 'a
    val insert: 'a t * key * 'a -> 'a t
    val update: 'a t * key * 'a -> 'a t
    val keys: 'a t -> key list
end;


(*Structure Order can vary; Tree avoids referring to a free structure. *)
structure Dict : DICTIONARY = 
struct

type key = string;
type 'a t = (key * 'a) tree;

exception E of key;

val empty = Lf;

fun lookup (Lf, b) = raise E b
  | lookup (Br ((a,x),t1,t2), b) =
    (case String.compare(a,b) of
	 GREATER => lookup(t1, b)
       | EQUAL   => x
       | LESS    => lookup(t2, b));

fun insert (Lf, b, y) = Br((b,y), Lf, Lf)
  | insert (Br((a,x),t1,t2), b, y) =
    (case String.compare(a,b) of
	 GREATER => Br ((a,x),  insert(t1,b,y),  t2)
       | EQUAL   => raise E b
       | LESS    => Br ((a,x),  t1,  insert(t2,b,y)));

fun update (Lf, b, y) = Br((b,y), Lf, Lf)
  | update (Br((a,x),t1,t2), b, y) =
    (case String.compare(a,b) of
	 GREATER => Br ((a,x),  update(t1,b,y),  t2)
       | EQUAL   => Br ((a,y),  t1,  t2)
       | LESS    => Br ((a,x),  t1,  update(t2,b,y)));

fun keys Lf = []
  | keys (Br((a, x), t1, t2)) = a :: (keys t1 @ keys t2);

end;

(***** End stolen code. The following is written by me. *****)

signature VALUATION = sig
    type t;
    val empty : t
    val set : t -> string -> bool -> t
    val value_of : t -> string -> bool
    val variables : t -> string list
    val print : t -> unit
end;

(* Exercise 2: A Structure for Valuations *)

functor ValuationFromDictionary (D : DICTIONARY where type key = string) :> VALUATION = struct

type t = bool D.t;

val empty = D.empty;
fun set valuation var value = D.update (valuation, var, value);
fun value_of valuation var = D.lookup (valuation, var);
fun variables valuation = D.keys valuation;
fun to_string valuation =
  foldr (fn (var, rest) => var ^ "=" ^ Bool.toString (value_of valuation var) ^ "; " ^ rest)
	""
	(variables valuation);
fun print valuation = TextIO.print (to_string valuation);
end;

structure Valuation = ValuationFromDictionary (Dict);

(* Exercise 3: A Functor for Propositional Logic *)

(* REPRESENTATION CONVENTION: A propositional formula is either an atom (true, false
   or a variable) or complex, created using the connectives not, and, or.
   REPRESENTATION INVARIANT: 
 *)
datatype formula = True | False | Var of string | Not of formula
		   | And of formula * formula | Or of formula * formula;

signature SEMANTICS = sig

    type valuation
    val truth_value : valuation -> formula -> bool
    val is_taut : formula -> bool

end;

functor Semantics (V : VALUATION) :> SEMANTICS where type valuation = V.t = struct

type valuation = V.t

fun truth_value _ True = true
  | truth_value _ False = false
  | truth_value valuation (Var var) = V.value_of valuation var
  | truth_value valuation (Not f) = not (truth_value valuation f)
  | truth_value valuation (And (f1, f2)) = (truth_value valuation f1) andalso (truth_value valuation f2)
  | truth_value valuation (Or (f1, f2)) = (truth_value valuation f1) orelse (truth_value valuation f2);

fun insertUnique comp x [] = [x]
  | insertUnique comp x (y::ys) =
    case comp (x, y) of
	LESS => x :: (y::ys)
      | EQUAL => insertUnique comp x ys
      | GREATER => y :: (insertUnique comp x ys);
			    
fun sortWithDuplicatesRemoved comp [] = []
  | sortWithDuplicatesRemoved comp (x::xs) = insertUnique comp x (sortWithDuplicatesRemoved comp xs);

fun variables formula =
  let
      fun variables' True = []
	| variables' False = []
	| variables' (Var x) = [x]
	| variables' (Not f) = variables' f
	| variables' (And (f1, f2)) = variables' f1 @ variables' f2
	| variables' (Or (f1, f2)) = variables' f1 @ variables' f2
  in
      sortWithDuplicatesRemoved String.compare (variables' formula)
  end;

fun valuationsForVariables vars =
  let
      fun valuationsForVariable var valuation = (V.set valuation var true) :: [V.set valuation var false];

      fun distributeVariableOverValuations var [] = []
	| distributeVariableOverValuations var (valuation::valuations) =
	  (valuationsForVariable var valuation) @ (distributeVariableOverValuations var valuations);

      fun valuationsForVariables' [] = [V.empty]
	| valuationsForVariables' (var::vars) =
	  let
	      val valuations = valuationsForVariables' vars
	  in
	      distributeVariableOverValuations var valuations
	  end;
  in
      valuationsForVariables' vars
  end;

fun is_taut formula =
  let
      val vars = variables formula
      val valuations = valuationsForVariables vars
  in
      List.all (fn v => truth_value v formula) valuations
  end
end;

(* Exercise 4: Simplification of Propositional Formulas *)

fun simp f =
  let
      fun simp' (Or (True, _)) = True
	| simp' (Or (False, f)) = simp' f
	| simp' (Or (_, True)) = True
	| simp' (Or (f, False)) = simp' f
	| simp' (And (False, _)) = False
	| simp' (And (True, f)) = simp' f
	| simp' (And (_, False)) = False
	| simp' (And (f, True)) = simp' f
	| simp' (Not (Not (f))) = simp' f
	| simp' (Not True) = False
	| simp' (Not False) = True
	| simp' True = True
	| simp' False = False
	| simp' (Var x) = Var x
	| simp' (Or (f1, f2)) = Or (simp' f1, simp' f2)
	| simp' (And (f1, f2)) = And (simp' f1, simp' f2)
	| simp' (Not f) = Not (simp' f);

      val f' = simp' f
  in
      if f = f' then f else simp f'
  end;
