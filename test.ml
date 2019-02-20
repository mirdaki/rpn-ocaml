open OUnit

(* Test Fixture *)
let test_fixture = "rpn" >:::
[
  "correct" >:: ( fun () -> 
    assert_equal "1." (Rpn.process_rpn "1");
    assert_equal "2." (Rpn.process_rpn "1 1 + ");
    assert_equal "0.5" (Rpn.process_rpn "1.5   1 -");
    assert_equal "9." (Rpn.process_rpn "3 3 *");
    assert_equal "1." (Rpn.process_rpn "3 3 /");
    assert_equal "27." (Rpn.process_rpn "3 3 ^");
    assert_equal "3.14285714286" (Rpn.process_rpn "22 7 /");
    assert_equal "0." (Rpn.process_rpn "1 2 + 3 -");
    assert_equal "18." (Rpn.process_rpn "2 3 11 + 5 - *");
    assert_equal "1." (Rpn.process_rpn "9 5 3 + 2 4 ^ - +");
    assert_equal "2." (Rpn.process_rpn "162 2 1 + 4 ^ /");
    assert_equal "5." (Rpn.process_rpn "15 7 1 1 + - / 3 * 2 1 1 + + -");
  );

  "error" >:: ( fun () ->
    assert_equal "Not enough operators" (Rpn.process_rpn "");
    assert_equal "Not enough arguments for '-'" (Rpn.process_rpn "-");
    assert_equal "Unrecognized token 'q'" (Rpn.process_rpn "1 q -");
    assert_equal "Divide by zero" (Rpn.process_rpn "1 0 /");
    assert_equal "Divide by zero" (Rpn.process_rpn "0 0 /");
    assert_equal "Not enough arguments for '+'" (Rpn.process_rpn "1 +");
    assert_equal "Not enough operators" (Rpn.process_rpn "1 1 11 1 1 +");
    assert_equal "Unrecognized token 'hello'" (Rpn.process_rpn "hello");
  );
]

(* Test Runner *)
let _ = run_test_tt test_fixture