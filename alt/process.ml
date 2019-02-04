module P = Printf

type past_game =
    { date : string
    ; away : string
    ; home : string
    ; away_goals : int
    ; home_goals : int
    }

type past_game_ot =
    { date : string
    ; away : string
    ; home : string
    ; away_goals : int
    ; home_goals : int
    ; ot : string
    }

type future_game =
    { date : string
    ; away : string
    ; home : string
    }

type split_games = past_game list * past_game_ot list * future_game list

exception InputValue of string

let string_to_goal (goals : string) (date : string) : int =
    try int_of_string goals with _ ->
        let (template : ('a, unit, string) format) =
            "unable to convert '%s' to <int>, see rows for date '%s'" in
        let (err : string) = P.sprintf template goals date in
        raise (InputValue err)

let split : (string list list -> split_games) =
    let rec loop
            (past : past_game list)
            (past_ot : past_game_ot list)
            (future : future_game list)
        : (string list list -> split_games) = function
        | [_; _; hg; h; ag; a; d]::xs ->
            let (x : past_game) =
                { date = d
                ; away = a
                ; home = h
                ; away_goals = string_to_goal ag d
                ; home_goals = string_to_goal hg d
                } in
            loop (x::past) past_ot future xs
        | [_; _; o; hg; h; ag; a; d]::xs ->
            let (x : past_game_ot) =
                { date = d
                ; away = a
                ; home = h
                ; away_goals = string_to_goal ag d
                ; home_goals = string_to_goal hg d
                ; ot = o
                } in
            loop past (x::past_ot) future xs
        | [h; a; d]::xs ->
            let (x : future_game) =
                { date = d
                ; away = a
                ; home = h
                } in
            loop past past_ot (x::future) xs
        | _::xs -> loop past past_ot future xs
        | [] -> (past, past_ot, future) in
    loop [] [] []

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

let test_past_ot =
    [ "2:30"
    ; "17,015"
    ; "SO"
    ; "2"
    ; "Anaheim Ducks"
    ; "3"
    ; "Arizona Coyotes"
    ; "2018-10-10"
    ]

let test () : split_games =
    split [[]; test_past; test_past_ot; test_future]
