open Data

module P = Printf

let string_to_goal (goals : string) (date : string) : int =
    try int_of_string goals with _ ->
        let (template : ('a, unit, string) format) =
            "unable to convert '%s' to <int>, check games with date '%s'" in
        let (err : string) = P.sprintf template goals date in
        raise (InputValue err)

let game_record ~date ~away ~home ~result : game =
    { date = date
    ; away = away
    ; home = home
    ; result = result
    }

let past_record ~date ~away ~home ~away_goals ~home_goals ~ot : game =
    let result =
        { away = string_to_goal away_goals date
        ; home = string_to_goal home_goals date
        ; ot = ot
        } in
    game_record ~date ~away ~home ~result:(Some result)

let future_record ~date ~away ~home : game =
    game_record ~date ~away ~home ~result:None

let games_record ~past_games ~future_games : split_games =
    { past_games = past_games
    ; future_games = future_games
    }
