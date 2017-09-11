use "lab0";

(*
 * An extremely simple unit testing framework inspired by SMLUnit (https://github.com/smlsharp/SMLUnit).
 * Specific to PolyML since PolyML.makestring is used to convert an arbitrary value to a string.
 *)
structure PolyMLUnit =
struct
exception Fail of string;

fun fail message = raise Fail message;

fun assertEqual expected actual =
  if expected = actual
  then ()
  else fail ("Expected: " ^ PolyML.makestring expected ^ ", actual: " ^ PolyML.makestring actual);

fun assertEqualReal expected actual =
  let
      val delta = 10E~8
  in
      if Real.abs (expected - actual ) < delta
      then ()
      else fail ("Expected: " ^ Real.toString expected ^ ", actual: " ^ Real.toString actual)
  end;

fun runTest test n =
  test ()
  handle Fail message => print ("Test " ^ Int.toString n ^ ": " ^ message ^ "\n")
       | e => print ("Test " ^ Int.toString n ^ ": " ^ PolyML.makestring e ^ "\n");

fun runTests tests =
  let fun runTests' [] _ numTests = print (Int.toString numTests ^ " tests run\n")
	| runTests' (test::tests) n numTests = (
	    runTest test n;
	    runTests' tests (n + 1) numTests
	)
  in
      runTests' tests 1 (length tests)
  end;
      
end;

(*
 * Unit tests for Lab 0.
 *)
structure Lab0Tests =
struct

fun testCubeZero () =
  PolyMLUnit.assertEqual 0 (cube 0);

fun testCubePositiveNumber () =
  PolyMLUnit.assertEqual 27 (cube 3);

fun testCubeNegativeNumber () =
  PolyMLUnit.assertEqual ~27 (cube ~3);

fun testSmallest1 () =
  PolyMLUnit.assertEqual 1 (smallest 1 2 3);

fun testSmallest2 () =
  PolyMLUnit.assertEqual 1 (smallest 2 1 3);

fun testSmallest3 () =
  PolyMLUnit.assertEqual 1 (smallest 2 3 1);

fun testIFTrue () =
  PolyMLUnit.assertEqual 1 (IF true 1 2);

fun testIFFalse () =
  PolyMLUnit.assertEqual 2 (IF false 1 2);

fun testIFNotLazyInSecondArgument () =
  (
    IF true 1 (1 div 0);
    PolyMLUnit.fail "Expected exception div"
  ) handle Div => ();

fun testIFNotLazyInFirstArgument () =
  (
    IF false (1 div 0) 2;
    PolyMLUnit.fail "Expected exception div"
  ) handle Div => ();

fun testBetween1 () =
  PolyMLUnit.assertEqual 2 (between 1 3);

fun testBetween2 () =
  PolyMLUnit.assertEqual 2 (between 3 1);

fun testBetweenNothing1 () =
  PolyMLUnit.assertEqual 42 (between 1 2);

fun testBetweenNothing2 () =
  PolyMLUnit.assertEqual 42 (between 2 1);

fun testPowPositive () =
  PolyMLUnit.assertEqualReal 8.0 (pow 2 3);

fun testPowNegative () =
  PolyMLUnit.assertEqualReal 0.125 (pow 2 ~3);

fun testPowZero1 () =
  PolyMLUnit.assertEqualReal 0.0 (pow 0 42);

fun testPowZero2 () =
  PolyMLUnit.assertEqualReal Real.posInf (pow 0 ~42);

fun testPowZeroPower1 () =
  PolyMLUnit.assertEqualReal 1.0 (pow 2 0);

fun testPowZeroPower2 () =
  PolyMLUnit.assertEqualReal 1.0 (pow ~2 0);

fun testThird () =
  PolyMLUnit.assertEqual 3 (third [1, 2, 3, 4]);

fun testThirdNoSuchElement () =
  (
    PolyMLUnit.assertEqual 3 (third [1, 2]);
    PolyMLUnit.fail "Expected exception Subscript"
  ) handle Subscript => ();

fun testLengthEmptyList () =
  PolyMLUnit.assertEqual 0 (length []);

fun testLengthNonEmptyList () =
  PolyMLUnit.assertEqual 3 (length [1, 2, 3]);

fun testLargest''1 () =
  PolyMLUnit.assertEqual 34 (largest'' [4,2,6,34,6,8,6,4,3,6,4,7,8,6,5,3] Int.compare 0);

fun testLargest''2 () =
  PolyMLUnit.assertEqual 42 (largest'' [] Int.compare 42);

fun testLargest''3 () =
  PolyMLUnit.assertEqual "bar" (largest'' ["foo", "bar", "baz", "frotz"] (reverseComparison String.compare) "z");

fun runTests () =
  let val allTests = [
	  testCubeZero,
	  testCubePositiveNumber,
	  testCubeNegativeNumber,
	  testSmallest1,
	  testSmallest2,
	  testSmallest3,
	  testIFTrue,
	  testIFFalse,
	  testIFNotLazyInSecondArgument,
	  testIFNotLazyInFirstArgument,
	  testBetween1,
	  testBetween2,
	  testBetweenNothing1,
	  testBetweenNothing2,
	  testPowPositive,
	  testPowNegative,
	  testPowZero1,
	  testPowZero2,
	  testPowZeroPower1,
	  testPowZeroPower2,
	  testThird,
	  testThirdNoSuchElement,
	  testLengthEmptyList,
	  testLengthNonEmptyList,
	  testLargest''1,
	  testLargest''2,
	  testLargest''3
      ]
  in
      PolyMLUnit.runTests allTests
  end;

end;

Lab0Tests.runTests ();
