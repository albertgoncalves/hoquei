open Data

module C = Convert
module P = Printf

let string_to_goal (goals : string) (date : string) : int =
    try int_of_string goals with _ ->
        let (template : ('a, unit, string) format) =
            "unable to convert '%s' to <int>, check games with date '%s'" in
        let (err : string) = P.sprintf template goals date in
        raise (InputValue err)

let split : (string list list -> split_games) =
    let rec loop past_games future_games
        : (string list list -> split_games) = function
        | [_; _; home_goals; home; away_goals; away; date]::xs ->
            let x =
                C.past_record
                    ~date ~away ~home ~away_goals ~home_goals ~ot:None in
            loop (x::past_games) future_games xs
        | [_; _; ot; home_goals; home; away_goals; away; date]::xs ->
            let x =
                C.past_record
                    ~date ~away ~home ~away_goals ~home_goals ~ot:(Some ot) in
            loop (x::past_games) future_games xs
        | [home; away; date]::xs ->
            let x = C.future_record ~date ~away ~home in
            loop past_games (x::future_games) xs
        | _::xs -> loop past_games future_games xs
        | [] -> C.games_record ~past_games ~future_games in
    loop [] []

let future_example =
    [ "Carolina Hurricanes"
    ; "Calgary Flames"
    ; "2019-02-03"
    ]

let past_example =
    [ "2:30"
    ; "15,321"
    ; "9"
    ; "Winnipeg Jets"
    ; "3"
    ; "Anaheim Ducks"
    ; "2019-02-02"
    ]

let past_ot_example =
    [ "2:30"
    ; "17,015"
    ; "SO"
    ; "2"
    ; "Anaheim Ducks"
    ; "3"
    ; "Arizona Coyotes"
    ; "2018-10-10"
    ]

let example () : split_games =
    split [[]; past_example; past_ot_example; future_example]