open OUnit2

module C = Convert
module D = Data

let test_goal_exn test_ctxt =
    let err =
        "unable to convert 'a' to <int>, check games with date '2018-10-04'" in
    assert_raises
        (D.InputValue err)
        (fun () -> C.string_to_goal "a" "2018-10-04")

let suite =
    "suite">:::
    [ "test string-to-goal exception">:: test_goal_exn
    ]

let () = run_test_tt_main suite
