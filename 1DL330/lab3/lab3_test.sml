(* test test_name test_function
   TYPE: string -> (unit -> bool) -> unit
   PRE: true
   POST: ()
   SIDE-EFFECTS: any side-effects of test_function () other than
     exceptions; prints whether the test test_name succeeded (i.e.,
     test_function () = true), failed, or an exception was raised
 *)
fun test test_name test_function =
    (
        if test_function () then
            print (" + SUCCESSFUL TEST, name: " ^ test_name ^ "\n")
        else
            print (" - FAILED TEST, name: " ^ test_name ^ "\n")
    )
    handle _ =>
        print (" - EXCEPTION RAISED IN TEST, name: " ^ test_name ^ "\n");


(* Do not modify the following line. Rename your file instead.
   The file that you submit must have this name. *)
use "lab3.sml";


(* TYPE: unit -> unit
   PRE: true
   POST: ()
   SIDE-EFFECTS: performs several tests and prints their results
 *)
(fn () =>

    (

        (* Test I *)

        test "I_1"
            (fn () => Real.compare (average [], 0.0) = EQUAL);

        test "I_2"
            (fn () => Real.compare (average [4.0, 4.0, 4.0, 4.0], 4.0) = EQUAL);

        test "I_3"
            (fn () => Real.compare (average [1.0, 4.0, 10.0], 5.0) = EQUAL);

        (* Test K *)

        test "K.1_1"
            (fn () => append [] [1,2] = [1,2]);

        test "K.1_2"
            (fn () => append ["3","2"] [] = ["3","2"]);

        test "K.1_3"
            (fn () => append [1,7,3,4,5] [3,4,10,1] = [1,7,3,4,5,3,4,10,1]);

        test "K.2_1"
            (fn () => not (member "A" ["B"]));

        test "K.2_2"
            (fn () => member 5 [1,2,3,4,200,100,5]);

        test "K.2_3"
            (fn () => member 1 [1,2,3,4,200,100,5]);

        test "K.2_4"
            (fn () => member 4 [1,2,3,4,200,100,5]);

        test "K.2_5"
            (fn () => not (member 6 [1,2,3,4,200,100,5]));

        test "K.3_1"
            (fn () => last [1,2,3,4,200,100,5] = 5);

        test "K.3_2"
            (fn () => last ["G"] = "G");

        test "K.4_1"
            (fn () => reverse ["G"] = ["G"]);

        test "K.4_2"
            (fn () => reverse [1,2,3,4,200,100,5] = [5,100,200,4,3,2,1]);

        test "K.5_1"
            (fn () => filter (fn n => n > 50) [1,2,3,4,200,100,5] = [200,100]);

        test "K.5_2"
            (fn () => filter (fn n => n < 0) [1,2,3,4,200,100,5] = []);

        (* Test L *)

        let
            val ex1 = Node(Node(Node(Void, 0, Node(Void, 2, Void)), 3, Node(Void, 5, Void)), 6, Node(Void, 7, Node(Void, 8, Node(Void, 9, Node(Node(Void, 10, Void), 12, Node(Void, 15, Node(Void, 19, Void)))))))
            fun all_elems Void = []
              | all_elems (Node(l,n,r)) = (all_elems l) @ [n] @ (all_elems r)
        in
            (
                test "L_1"
                    (fn () => all_elems (sub_tree 5 8 ex1) = [5,6,7]);

                test "L_2"
                    (fn () => all_elems (sub_tree 10 20 ex1) = [10,12,15,19]);

                test "L_3"
                    (fn () => all_elems (sub_tree 0 1 ex1) = [0]);

                test "L_4"
                    (fn () => all_elems (sub_tree 1 2 ex1) = [])
            )
        end
    )

) ();
