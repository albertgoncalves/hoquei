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

let result_record ~ot ~away ~home ~date : result option =
    Some
        { away = string_to_goal away date
        ; home = string_to_goal home date
        ; ot = ot
        }

let label : (string list -> game option) = function
    | [_; _; home_goals; home; away_goals; away; date] ->
        let ot = None in
        let result =
            result_record ~ot ~away:away_goals ~home:home_goals ~date in
        Some (game_record ~date ~away ~home ~result)
    | [_; _; ot; home_goals; home; away_goals; away; date] ->
        let ot = Some ot in
        let result =
            result_record ~ot ~away:away_goals ~home:home_goals ~date in
        Some (game_record ~date ~away ~home ~result)
    | [home; away; date] ->
        let result = None in
        Some (game_record ~date ~away ~home ~result)
    | _ -> None