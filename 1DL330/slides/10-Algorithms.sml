(* Huffman Coding: Introduction  *)

datatype hufftree = Leaf of int * char
                  | Node of int * hufftree * hufftree;

fun frequency (Leaf (n,_)) = n
  | frequency (Node (n,_,_)) = n;


(* Counting Character Frequencies *)

signature DICTIONARY =
sig
  type (''a,'b) T
  val empty: (''a,'b) T
  val insert: ''a -> 'b -> (''a, 'b) T -> (''a, 'b) T
  val lookup: ''a -> (''a, 'b) T -> 'b option
  val list_of: (''a,'b) T -> (''a * 'b) list
end;

structure Dictionary :> DICTIONARY =
struct
  type (''a,'b) T = (''a * 'b) list
  val empty = []
  (* insert avoids duplicate keys *)
  fun insert k v [] = [(k,v)]
    | insert k v ((k',v') :: d) = if k=k' then (k,v) :: d else (k',v') :: insert k v d
  fun lookup k [] = NONE
    | lookup k ((k',v) :: d) = if k=k' then SOME v else lookup k d
  fun list_of d = d
end;

(* frequencies cs
   TYPE: char list -> (char, int) Dictionary.T
   PRE: true
   POST: a dictionary that maps each char in cs to the number of its
     occurrences in cs
 *)
fun frequencies cs =
  foldl (fn (char, dict) =>
    case Dictionary.lookup char dict of
      NONE => Dictionary.insert char 1 dict
    | SOME n => Dictionary.insert char (n+1) dict) Dictionary.empty cs;

frequencies (String.explode "this is an example of a huffman tree");


(* Building a Huffman Tree *)

signature PRIORITY_QUEUE =
  sig
    type 'a T
    val empty: ('a * 'a -> order) -> 'a T
    val insert: 'a -> 'a T -> 'a T
    val least: 'a T -> 'a * 'a T
    val is_empty: 'a T -> bool
    exception Empty
  end;

structure Priority_Queue :> PRIORITY_QUEUE =
  struct
    datatype 'a T = PQ of ('a * 'a -> order) * 'a list
    fun empty order = PQ (order, [])
    fun insert x (PQ (order, qs)) =
      let
        fun insert' [] = [x]
          | insert' (q::qs) = case order (x,q) of LESS => x::q::qs | _ => q :: insert' qs
      in
        PQ (order, insert' qs)
      end
    fun least (PQ (order, [])) = raise Empty
      | least (PQ (order, q::qs)) = (q, PQ (order, qs))
    fun is_empty (PQ (_, qs)) = null qs
    exception Empty
  end;

(* hufftree dict
   TYPE: (char,int) Dictionary.T -> hufftree
   PRE: dict is non-empty
   POST: a Huffman tree based on the frequency data in dict
 *)
fun hufftree dict =
  let
    val trees = map (fn (c,n) => Leaf (n,c)) (Dictionary.list_of dict)
    fun hufftree_compare (t1, t2) =
      Int.compare (frequency t1, frequency t2)
    val queue = foldl (fn (t,q) => Priority_Queue.insert t q)
      (Priority_Queue.empty hufftree_compare) trees
    fun merge_hufftrees t1 t2 =
      Node (frequency t1 + frequency t2, t1, t2)
    fun merge_queue q =
      let
        val (t1,q) = Priority_Queue.least q
      in
        if Priority_Queue.is_empty q then
          t1
        else
          let
            val (t2,q) = Priority_Queue.least q
          in
            merge_queue (Priority_Queue.insert (merge_hufftrees t1 t2) q)
          end
      end
  in
    merge_queue queue
  end;

hufftree
  (frequencies (String.explode "this is an example of a huffman tree"));


(* Encoding a Text *)

(* code_dict t
   TYPE: hufftree -> (char,int list) Dictionary.T
   PRE: t is not a Leaf
   POST: a dictionary mapping characters in t to their Huffman
     encoding
 *)
fun code_dict t =
  let
    fun code_dict' dict path (Leaf (_,c)) =
          Dictionary.insert c (rev path) dict
      | code_dict' dict path (Node (_,l,r)) =
          code_dict' (code_dict' dict (0::path) l) (1::path) r
  in
    code_dict' Dictionary.empty [] t
  end;

code_dict (hufftree
  (frequencies (String.explode "this is an example of a huffman tree")));

(* encode_with_tree t cs
   TYPE: hufftree -> char list -> int list
   PRE: t is not a Leaf and contains all characters in cs
   POST: the Huffman coding of cs under the given tree t
 *)
fun encode_with_tree t cs =
  let
    val dict = code_dict t
  in
    List.concat
      (map (fn c => valOf (Dictionary.lookup c dict)) cs)
  end;

let
  val cs = String.explode "this is an example of a huffman tree"
  val t = hufftree (frequencies cs)
in
  encode_with_tree t cs
end;

(* encode cs
   TYPE: char list -> hufftree * int list
   PRE: cs contains at least two distinct characters
   POST: (a Huffman tree based on the character frequencies in cs,
     the Huffman coding of cs under this tree)
 *)
fun encode cs =
  let
    val t = hufftree (frequencies cs)
  in
    (t, encode_with_tree t cs)
  end;

encode (String.explode "this is an example of a huffman tree");

(* Decoding a Sequence of Bits *)

(* decode_one t xs
   TYPE: hufftree -> int list -> char * int list
   PRE: xs = w @ xs', where w is a valid Huffman code word for t
   POST: (the decoding of w under t, xs')
 *)
fun decode_one (Leaf (_,c)) xs = (c, xs)
  | decode_one (Node (_,l,_)) (0::xs) = decode_one l xs
  | decode_one (Node (_,_,r)) (1::xs) = decode_one r xs
  | decode_one (Node _) _ = raise Domain;

val (t,xs) = encode (String.explode "this is an example of a huffman tree");
decode_one t xs;

(* decode t xs
   TYPE: hufftree -> int list -> char list
   PRE: xs is a concatenation of valid Huffman code words for t
   POST: the decoding of xs under t
 *)
fun decode t [] =
      []
  | decode t xs =
      let
        val (c,xs) = decode_one t xs
      in
        c :: decode t xs
      end;

val (t,xs) = encode (String.explode "this is an example of a huffman tree");
decode t xs;
String.implode (decode t xs);


(* Exercise *)

(* encode []; *)

val (t,xs) = encode [#"x"];
decode t xs;

val (t,xs) = encode [#"x", #"x", #"x"];
decode t xs;


(* Genericity *)

(* one signature that contains all parameters *)
signature PARAMETERS =
sig
  structure D : DICTIONARY
  structure PQ : PRIORITY_QUEUE
end;

(* this signature is optional; it hides auxiliary functions (declared
   in the functor below) in the functor's result *)
signature HUFFMAN_SIG =
sig
  type hufftree
  val encode: char list -> hufftree * int list
  val decode: hufftree -> int list -> char list
end;

functor HUFFMAN_CODING(Parameters: PARAMETERS) : HUFFMAN_SIG =
struct

  (* aliases (just for convenience) *)
  structure Dictionary = Parameters.D
  structure Priority_Queue = Parameters.PQ

  (* the same code as above (but now it refers to the functor's parameters) *)
  datatype hufftree = Leaf of int * char
                    | Node of int * hufftree * hufftree

  fun frequency (Leaf (n,_)) = n
    | frequency (Node (n,_,_)) = n

  fun frequencies cs =
    foldl (fn (char, dict) =>
      case Dictionary.lookup char dict of
        NONE => Dictionary.insert char 1 dict
      | SOME n => Dictionary.insert char (n+1) dict) Dictionary.empty cs

  fun hufftree dict =
    let
      val trees = map (fn (c,n) => Leaf (n,c)) (Dictionary.list_of dict)
      fun hufftree_compare (t1, t2) =
        Int.compare (frequency t1, frequency t2)
      val queue = foldl (fn (t,q) => Priority_Queue.insert t q)
        (Priority_Queue.empty hufftree_compare) trees
      fun merge_hufftrees t1 t2 =
        Node (frequency t1 + frequency t2, t1, t2)
      fun merge_queue q =
        let
          val (t1,q) = Priority_Queue.least q
        in
          if Priority_Queue.is_empty q then
            t1
          else
            let
              val (t2,q) = Priority_Queue.least q
            in
              merge_queue (Priority_Queue.insert (merge_hufftrees t1 t2) q)
            end
        end
    in
      merge_queue queue
    end

  fun code_dict t =
    let
      fun code_dict' dict path (Leaf (_,c)) =
            Dictionary.insert c (rev path) dict
        | code_dict' dict path (Node (_,l,r)) =
            code_dict' (code_dict' dict (0::path) l) (1::path) r
    in
      code_dict' Dictionary.empty [] t
    end

  fun encode_with_tree t cs =
    let
      val dict = code_dict t
    in
      List.concat
        (map (fn c => valOf (Dictionary.lookup c dict)) cs)
    end

  fun encode cs =
    let
      val t = hufftree (frequencies cs)
    in
      (t, encode_with_tree t cs)
    end

  fun decode_one (Leaf (_,c)) xs = (c, xs)
    | decode_one (Node (_,l,_)) (0::xs) = decode_one l xs
    | decode_one (Node (_,_,r)) (1::xs) = decode_one r xs
    | decode_one (Node _) _ = raise Domain

  fun decode t [] =
        []
    | decode t xs =
        let
          val (c,xs) = decode_one t xs
        in
          c :: decode t xs
        end

end; (* functor *)

structure List_Parameters =
struct
  (* our list-based implementations of dictionaries and priority queues *)
  structure D = Dictionary
  structure PQ = Priority_Queue
end;

(* an implementation of Huffman coding that uses our list-based
   implementations of dictionaries and priority queues *)
structure List_Coding = HUFFMAN_CODING(List_Parameters);

val (t,xs) = List_Coding.encode
  (String.explode "this is an example of a huffman tree");
String.implode (List_Coding.decode t xs);
