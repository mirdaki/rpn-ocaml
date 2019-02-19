open OUnit

(* Test Fixture *)
let test_fixture = "rpn" >:::
[
  "correct" >:: ( fun () -> 
    assert_equal "1." (Rpn.evaluate_line "1");
    assert_equal "2." (Rpn.evaluate_line "1 1 +");
    assert_equal "0.5" (Rpn.evaluate_line "1.5 1 -");
    assert_equal "9." (Rpn.evaluate_line "3 3 *");
    assert_equal "1." (Rpn.evaluate_line "3 3 /");
    assert_equal "27." (Rpn.evaluate_line "3 3 ^");
    assert_equal "3.14285714286" (Rpn.evaluate_line "22 7 /");
    assert_equal "0." (Rpn.evaluate_line "1 2 + 3 -");
    assert_equal "18." (Rpn.evaluate_line "2 3 11 + 5 - *");
    assert_equal "1." (Rpn.evaluate_line "9 5 3 + 2 4 ^ - +");
    assert_equal "2." (Rpn.evaluate_line "162 2 1 + 4 ^ / ");
    assert_equal "5." (Rpn.evaluate_line "15 7 1 1 + - / 3 * 2 1 1 + + -");
  );

  "error" >:: ( fun () ->
    assert_equal "Invalid input" (Rpn.evaluate_line "");
    assert_equal "Not enough arguments" (Rpn.evaluate_line "-");
    assert_equal "Not a number" (Rpn.evaluate_line "1 q -");
    assert_equal "Divide by zero" (Rpn.evaluate_line "1 0 /");
    assert_equal "Divide by zero" (Rpn.evaluate_line "0 0 /");
    assert_equal "Not enough arguments" (Rpn.evaluate_line "1 +");
    assert_equal "Invalid input" (Rpn.evaluate_line "1 1 11 1 1 +");
    assert_equal "Not a number" (Rpn.evaluate_line "hello");
  );
]

(* Test Runner *)
let _ = run_test_tt test_fixture