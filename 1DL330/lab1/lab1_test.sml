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
use "lab1.sml";


(* TYPE: unit -> unit
   PRE: true
   POST: ()
   SIDE-EFFECTS: performs several tests and prints their results
 *)
(fn () =>

    (
        (* Test 2 *)

        test "2.1_1"
            (fn () => minus 1 1 = 0);

        test "2.1_2"
            (fn () => minus 7 4 = 3);

        (* Test 3 *)

        test "3.1"
            (fn () => ( fun1 0 = 42; true ));

        test "3.2"
            (fn () => ( fun2 0 0 = 42; true ));

        test "3.3"
            (fn () => ( fun3 0 = (42, 42); true ));

        test "3.4"
            (fn () => ( fun4 (0, 0) = 42; true ));

        test "3.5"
            (fn () => ( fun5 0 0.0 "foo" = "bar"; true ));

        test "3.6"
            (fn () => ( fun6 (0, ("foo", "bar", 0)) = (42, "baz"); true ));

        (* Test 4 *)

        test "4_1"
            (fn () => lcm 1 = 1);

        test "4_2"
            (fn () => lcm 3 = 6);

        test "4_3"
            (fn () => lcm 5 = 60);

        test "4_4"
            (fn () => lcm 10 = 2520)
    )

) ();
