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
use "lab2.sml";


(* TYPE: unit -> unit
   PRE: true
   POST: ()
   SIDE-EFFECTS: performs several tests and prints their results
 *)
(fn () =>

    (
        (* Test E *)

        test "E_1"
            (fn () => iota 3 = [0, 1, 2]);

        test "E_2"
            (fn () => iota 0 = []);

        test "E_3"
            (fn () => iota 1 = [0]);

        (* Test F *)

        test "F.1_1"
            (fn () => inter [2, 1] [1] = [1]);

        test "F.1_2"
            (fn () => inter [1] [1, 2] = [1]);

        test "F.1_3"
            (fn () => inter [2, 1] [] = []);

        test "F.1_4"
            (fn () => inter [] [1, 2] = []);

        test "F.1_5"
            (fn () => inter [2, 3, 4, 1] [7, 1, 0] = [1]);

        test "F.1_6"
            (fn () =>
                let
                    val r = inter [1, 2] [1, 2];
                in
                    r = [1, 2] orelse r = [2, 1]
                end);

        test "F.1_7"
            (fn () =>
                let
                    val r = inter [3, 4] [4, 3];
                in
                    r = [3, 4] orelse r = [4, 3]
                end);

        test "F.1_8"
            (fn () =>
                let
                    val r = inter [4, 3] [3, 4];
                in
                    r = [3, 4] orelse r = [4, 3]
                end);

        test "F.2_1"
            (fn () => inter' [1, 2] [1] = [1]);

        test "F.2_2"
            (fn () => inter' [1] [1, 2] = [1]);

        test "F.2_3"
            (fn () => inter' [1, 2] [] = []);

        test "F.2_4"
            (fn () => inter' [] [1, 2] = []);

        test "F.2_5"
            (fn () => inter' [1, 2, 3, 4] [0, 1, 7] = [1]);

        test "F.2_6"
            (fn () => inter' [1, 2, 3, 4] [2, 4, 5] = [2, 4]);

        test "F.2_7"
            (fn () => inter' [3, 4] [3, 4] = [3, 4]);

        test "F.3"
            (fn () =>
                let
                    (* real_time f
                       TYPE: (unit -> 'a) -> Time.time * 'a
                       PRE : true
                       POST: (the amount of (real) time spent evaluating f (), f ())
                       SIDE-EFFECTS: any side-effects caused by evaluating f ()
                     *)
                    fun real_time f =
                        let
                            val rt = Timer.startRealTimer()
                            val result = f ()
                            val time = Timer.checkRealTimer rt
                        in
                            (time, result)
                        end
                    val s1 = iota 10000
                    val s2 = iota 100000
                    val (slow_time, slow_res) = real_time (fn () => inter s1 s2)
                    val (fast_time, fast_res) = real_time (fn () => inter' s1 s2)
                    (* Merge sort from rosetta code
                       http://rosettacode.org/wiki/Sorting_algorithms/Merge_sort#Standard_ML *)
                    fun merge cmp ([], ys) = ys
                      | merge cmp (xs, []) = xs
                      | merge cmp (xs as x::xs', ys as y::ys') =
                        case cmp (x, y) of
                          GREATER => y :: merge cmp (xs, ys')
                        | _ => x :: merge cmp (xs', ys);
                    fun merge_sort cmp [] = []
                      | merge_sort cmp [x] = [x]
                      | merge_sort cmp xs =
                        let
                            val ys = List.take (xs, length xs div 2)
                            val zs = List.drop (xs, length xs div 2)
                        in
                            merge cmp (merge_sort cmp ys, merge_sort cmp zs)
                        end
                    val int_list_sort = merge_sort Int.compare
                    val _ = print ("   (inter time " ^ Time.toString slow_time ^ " inter' time " ^ Time.toString fast_time ^ ")\n")
                in
                    slow_time > fast_time andalso int_list_sort slow_res = int_list_sort fast_res
                end);

        (*Test G*)

        test "G.2_1"
            (fn () => Real.compare (sumPrice [] 3.0 2.0 5.0, 0.0) = EQUAL);

        test "G.2_2"
            (fn () => Real.compare (sumPrice [Banana 4.0, (* 8 *)
                                              Apple 3.0, (* 9 *)
                                              Lemon 7, (* 35 *)
                                              Banana 2.0, (* 4 *)
                                              Apple 1.0, (* 3 *)
                                              Lemon 1 (* 5 *)] 3.0 2.0 5.0, 64.0) = EQUAL);

        (*Test H*)

        let
            val test_tree1 = Node ("hej", []);
            val test_tree2 = Node (1, [Node (2, [])]);
            val test_tree3 = Node ("hej",
                                   [Node ("hello", [Node ("ni hao", [Node ("ahoj", [])])]),
                                    Node ("bonjour", [Node ("privet", [Node ("guten tag", [])])]),
                                    Node ("namaste", [Node ("ciao", [Node ("As-salam alaykom", [Node ("saluton", [Node ("hei", [Node ("halo", [])])]),
                                                                                                Node ("kon-nichiwa", [Node ("an-nyong ha-se-yo ", [Node ("ola", [])])]),
                                                                                                Node ("sa-wat-dee", [Node ("selam", [Node ("jambo", [])])])])])])]);
        in
            (
                test "H.1_1" (fn () => count test_tree1 = 1);

                test "H.1_2" (fn () => count test_tree2 = 2);

                test "H.1_3" (fn () => count test_tree3 = 19);

                test "H.2_1" (fn () => labels test_tree1 = ["hej"]);

                test "H.2_2" (fn () =>
                                 let
                                     val v = labels test_tree2
                                 in
                                     v = [1,2] orelse v = [2,1]
                                 end);

                test "H.2_3" (fn () => length (labels test_tree3) = 19);

                test "H.3_1" (fn () => is_present test_tree1 "no" = false);

                test "H.3_2" (fn () => is_present test_tree2 8 = false);

                test "H.3_3" (fn () => is_present test_tree3 "no" = false);

                test "H.3_4" (fn () => is_present test_tree1 "hej" = true);

                test "H.3_5" (fn () => is_present test_tree2 1 = true);

                test "H.3_6" (fn () => is_present test_tree3 "As-salam alaykom" = true);

                test "H.3_7" (fn () => is_present test_tree3 "ahoj" = true);

                test "H.3_8" (fn () => is_present test_tree3 "hej" = true);

                test "H.4_1" (fn () => height test_tree1 = 1);

                test "H.4_2" (fn () => height test_tree2 = 2);

                test "H.4_3" (fn () => height test_tree3 = 7)
            )
        end
    )

) ();
