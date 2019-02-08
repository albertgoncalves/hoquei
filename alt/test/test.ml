open OUnit2

module D = Data
module R = Record
module S = Scrape

let test_scalpel test_ctxt =
    let test_case =
        "<tr ><th scope=\"row\" class=\"left \" data-stat=\"date_game\" csk=\"201810030SJS\" ><a href=\"/boxscores/201810030SJS.html\">2018-10-03</a></th><td class=\"left \" data-stat=\"visitor_team_name\" csk=\"ANA.201810030SJS\" ><a href=\"/teams/ANA/2019.html\">Anaheim Ducks</a></td><td class=\"right \" data-stat=\"visitor_goals\" >5</td><td class=\"left \" data-stat=\"home_team_name\" csk=\"SJS.201810030SJS\" ><a href=\"/teams/SJS/2019.html\">San Jose Sharks</a></td><td class=\"right \" data-stat=\"home_goals\" >2</td><td class=\"center iz\" data-stat=\"overtimes\" csk=\"0\" ></td><td class=\"right \" data-stat=\"attendance\" >17,562</td><td class=\"right \" data-stat=\"game_duration\" >2:25</td><td class=\"left iz\" data-stat=\"game_remarks\" ></td></tr>" in
    let result =
        [ "2:25"
        ; "17,562"
        ; "2"
        ; "San Jose Sharks"
        ; "5"
        ; "Anaheim Ducks"
        ; "2018-10-03"
        ] in
    assert_equal result (S.scalpel test_case)

let test_goal_exn test_ctxt =
    let err =
        "unable to convert 'a' to <int>, check games with date '2018-10-04'" in
    assert_raises
        (D.InputValue err)
        (fun () -> R.string_to_goal "a" "2018-10-04")

let test_future_label test_ctxt =
    let record : D.game =
        { home = "Carolina Hurricanes"
        ; away = "Calgary Flames"
        ; date = "2019-02-03"
        ; result = None
        } in
    let test_case =
        [ "Carolina Hurricanes"
        ; "Calgary Flames"
        ; "2019-02-03"
        ] in
    assert_equal (Some record) (R.label test_case)

let test_past_label test_ctxt =
    let result : D.result =
        { home = 9
        ; away = 3
        ; ot = None
        } in
    let record : D.game =
        { home = "Winnipeg Jets"
        ; away = "Anaheim Ducks"
        ; date = "2019-02-02"
        ; result = Some result
        } in
    let test_case =
        [ "2:30"
        ; "15,321"
        ; "9"
        ; "Winnipeg Jets"
        ; "3"
        ; "Anaheim Ducks"
        ; "2019-02-02"
        ] in
    assert_equal (Some record) (R.label test_case)

let test_past_ot_label test_ctxt =
    let result : D.result =
        { home = 2
        ; away = 3
        ; ot = Some "SO"
        } in
    let record : D.game =
        { home = "Anaheim Ducks"
        ; away = "Arizona Coyotes"
        ; date = "2018-10-10"
        ; result = Some result
        } in
    let test_case =
        [ "2:30"
        ; "17,015"
        ; "SO"
        ; "2"
        ; "Anaheim Ducks"
        ; "3"
        ; "Arizona Coyotes"
        ; "2018-10-10"
        ] in
    assert_equal (Some record) (R.label test_case)

let suite =
    "suite">:::
    [ "test scalpel">:: test_scalpel
    ; "test string-to-goal exception">:: test_goal_exn
    ; "test future label">:: test_future_label
    ; "test past label">:: test_past_label
    ; "test past ot label">:: test_past_ot_label
    ]

let () = run_test_tt_main suite
