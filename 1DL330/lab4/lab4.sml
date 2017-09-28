(* Functional Programming 1DL330
   Assignment 4

   Henrik Arro
 *)

(* Exercise 1: A Signature for Valuations *)

(* The following dictionary code using binary trees is taken from
 * http://www.cl.cam.ac.uk/~lp15/MLbook/programs/sample4.sml.
 *
 * The only addition is the function keys to list all keys in a dictionary.
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
fun print valuation =
  TextIO.print (foldr (fn (var, rest) => var ^ "=" ^ Bool.toString (value_of valuation var) ^ "; " ^ rest)
		      ""
		      (variables valuation));
end;

structure Valuation = ValuationFromDictionary (Dict);

(* Exercise 3: A Functor for Propositional Logic *)

(* REPRESENTATION CONVENTION: A propositional formula is either an atom (true, false
   or a variable) or complex, created using the connectives not, and, or.
   REPRESENTATION INVARIANT: 
 *)
datatype formula = True | False | Var of string | Not of formula
		   | And of formula * formula | Or of formula * formula;

fun nnf True = True
  | nnf False = False
  | nnf (Var x) = Var x
  | nnf (Not True) = False
  | nnf (Not False) = True
  | nnf (Not (Var x)) = Not (Var x)
  | nnf (Not (Not p)) = nnf p
  | nnf (Not (And (p, q))) = nnf (Or (Not p, Not q))
  | nnf (Not (Or (p,q))) = nnf (And (Not p, Not q))
  | nnf (And (p, q)) = And (nnf p, nnf q)
  | nnf (Or (p, q)) = Or (nnf p, nnf q);

infix mem;
fun x mem []  =  false
  | x mem (y::l)  =  (x=y) orelse (x mem l);

fun inter([],ys) = []
  | inter(x::xs, ys) = 
    if x mem ys then x::inter(xs, ys)
    else inter(xs, ys);

fun distrib (p, And (q, r)) = And (distrib (p, q), distrib (p, r))
  | distrib (And (q, r), p) = And (distrib (q, p), distrib (r, p))
  | distrib (p, q) = And (p, q)   (*no conjunctions*) ;

fun cnf (And (p, q)) = And (cnf p, cnf q)
  | cnf (Or (p, q)) = distrib (cnf p, cnf q)
  | cnf p = p    (*a literal*) ;

exception NonCNF;

fun positives True = ["true"]
  | positives False = []
  | positives (Var x) = [x]
  | positives (Not (Var _)) = []
  | positives (Or (p, q)) = positives p @ positives q
  | positives _ = raise NonCNF;

fun negatives True = []
  | negatives False = ["false"]
  | negatives (Var _) = []
  | negatives (Not (Var x)) = [x]
  | negatives (Or (p, q)) = negatives p @ negatives q
  | negatives _ = raise NonCNF;

fun taut (And (p, q)) = taut p andalso taut q
  | taut p = not (null (inter (positives p, negatives p)));

functor Semantics (V : VALUATION) = struct
fun truth_value _ True = true
  | truth_value _ False = false
  | truth_value valuation (Var var) = V.value_of valuation var
  | truth_value valuation (Not f) = not (truth_value valuation f)
  | truth_value valuation (And (f1, f2)) = (truth_value valuation f1) andalso (truth_value valuation f2)
  | truth_value valuation (Or (f1, f2)) = (truth_value valuation f1) orelse (truth_value valuation f2);

fun is_taut formula = taut (cnf (nnf formula));
end;

(* Exercise 4: Simplification of Propositional Formulas *)

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

fun simp f =
  let
      val f' = simp' f
  in
      if f = f' then f else simp f'
end;
