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
use "lab4.sml";


structure S = Semantics(Valuation);

(* TYPE: unit -> unit
   PRE: true
   POST: ()
   SIDE-EFFECTS: performs several tests and prints their results
 *)
(fn () =>

    (
        let
            val v = foldl (fn ((name,value), v) => Valuation.set v name value)
                Valuation.empty
                [("A",true), ("B",false), ("C",true), ("D",false), ("Y",true), ("Y",false)]
            val vars = Valuation.variables v
        in
            (* Test O *)

            test "O_1"
                (fn () => Valuation.value_of v "A");

            test "O_2"
                (fn () => not (Valuation.value_of v "B"));

            test "O_3"
                (fn () => Valuation.value_of v "C");

            test "O_4"
                (fn () => not (Valuation.value_of v "D"));

            test "O_5"
                (fn () => not (Valuation.value_of v "Y"));

            test "O_6"
                (fn () => length vars = 5);

            test "O_7"
                (fn () => List.all (fn x => List.exists (fn n => n = x) vars) ["A","B","C","D","Y"]);

            test "O_8"
                (fn () => (Valuation.print v; true));

            (* Test P *)

            test "P_1"
                (fn () => S.truth_value v (Not (Not (Not (Var "B")))));

            test "P_2"
                (fn () => not (S.truth_value v (Not (Not (Not (Var "A"))))));

            test "P_3"
                (fn () => not (S.is_taut (Not (Not (Not (Var "A"))))));

            test "P_4"
                (fn () => not (S.is_taut (Not (Not (Not (Var "B"))))));

            test "P_5"
                (fn () => S.is_taut (Or (Not (Var "A"), (Var "A"))));

            test "P_6"
                (fn () => not (S.is_taut (Or (Not (Var "A"), (Var "B")))));

            test "P_7"
                (fn () => S.is_taut (And (Or (Not (Var "B"), (Var "B")),
                                               Or (Not (Var "A"), (Var "A")))));

            test "P_8"
                (fn () => not (S.is_taut (And (Or (Not (Var "B"), (Var "A")),
                                                    Or (Not (Var "A"), (Var "B"))))));

            test "P_9"
                (fn () => S.is_taut True);

            test "P_10"
                (fn () => not (S.is_taut False))
        end;

        (* Test Q *)

        test "Q_1"
            (fn () => simp (Not (And (And (False, Var "x"), Var "y"))) = True);

        test "Q_2"
            (fn () => simp (And (And (Var "x", Var "y"), Var "z")) =
                And (And (Var "x", Var "y"), Var "z"));

        test "Q_3"
            (fn () => simp (And (Not (Or (True, Var "x")), True)) = False);

        test "Q_4"
            (fn () => simp (Var "x") = Var "x");

        test "Q_5"
            (fn () => simp (Not (Or (False, Var "x"))) = Not (Var "x"));

        test "Q_6"
            (fn () => simp (And (And (Var "x", Var "y"), And (Var "z", Not (Not (Not (Var "a")))))) =
                And (And (Var "x", Var "y"), And (Var "z", Not (Var "a"))));

        test "Q_7"
            (fn () => simp True = True);

        test "Q_8"
            (fn () => simp False = False)
    )

) ();
