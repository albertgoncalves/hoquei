module P = Printf

type past_game =
    { date : string
    ; away : string
    ; home : string
    ; away_goals : int
    ; home_goals : int
    }

type future_game =
    { date : string
    ; away : string
    ; home : string
    }

exception InputValue of string

let string_to_goal (goals : string) (date : string) : int =
    try int_of_string goals with _ ->
        let (template : ('a, unit, string) format) =
            "unable to convert '%s' to <int>, see rows for date '%s'" in
        let (err : string) = P.sprintf template goals date in
        raise (InputValue err)

let split : (string list list -> (past_game list * future_game list)) =
    let rec loop (past : past_game list) (future : future_game list)
        : (string list list -> (past_game list * future_game list)) = function
        | [h; a; d]::xs ->
            let (x : future_game) =
                {date = d; away = a; home = h} in
            loop past (x::future) xs
        | [_; _; hg; h; ag; a; d]::xs ->
            let (x : past_game) =
                { date = d
                ; away = a
                ; home = h
                ; away_goals = string_to_goal ag d
                ; home_goals = string_to_goal hg d
                } in
            loop (x::past) future xs
        | _::xs -> loop past future xs
        | [] -> (past, future) in
    loop [] []

let test_future =
    [ "Carolina Hurricanes"
    ; "Calgary Flames"
    ; "2019-02-03"
    ]

let test_past =
    [ "2:30"
    ; "15,321"
    ; "9"
    ; "Winnipeg Jets"
    ; "3"
    ; "Anaheim Ducks"
    ; "2019-02-02"
    ]

let test () : (past_game list * future_game list) =
    split [[]; test_past; test_future]
