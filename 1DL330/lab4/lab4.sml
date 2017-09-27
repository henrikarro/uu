(* The following dictionary code using binary trees is taken from
 * https://www.cs.cmu.edu/~rwh/introsml/modules/sigstruct.htm.
 *
 * The only addition is the function keys.
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

functor ValuationFromDictionary (D : DICTIONARY where type key = string) :> VALUATION = struct
type t = bool D.t;
val empty = D.empty;
fun set valuation var value = D.update (valuation, var, value);
fun value_of valuation var = D.lookup (valuation, var);
fun variables valuation = D.keys valuation;
fun print valuation = ();
end;

structure Valuation = ValuationFromDictionary (Dict);

datatype formula = True | False | Var of string | Not of formula
		   | And of formula * formula | Or of formula * formula;

functor Semantics (V : VALUATION) = struct
fun truth_value valuation formula = false;
fun is_taut formula = false;
end;

fun simp formula = formula;
