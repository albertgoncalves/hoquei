open Data

module P = Printf

let string_to_goal (goals : string) (date : string) : int =
    try int_of_string goals with _ ->
        let (template : ('a, unit, string) format) =
            "unable to convert '%s' to <int>, check games with date '%s'" in
        let (err : string) = P.sprintf template goals date in
        raise (InputValue err)

let past_record ~date ~away ~home ~away_goals ~home_goals ~ot : past_game =
    { date = date
    ; away = away
    ; home = home
    ; away_goals = string_to_goal away_goals date
    ; home_goals = string_to_goal home_goals date
    ; ot = ot
    }

let future_record ~date ~away ~home : future_game =
    { date = date
    ; away = away
    ; home = home
    }

let games_record ~past_games ~future_games : split_games =
    { past_games = past_games
    ; future_games = future_games
    }
