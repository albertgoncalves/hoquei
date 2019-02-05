type past_game =
    { date : string
    ; away : string
    ; home : string
    ; away_goals : int
    ; home_goals : int
    ; ot : string option
    }

type future_game =
    { date : string
    ; away : string
    ; home : string
    }

type split_games =
    { past_games : past_game list
    ; future_games : future_game list
    }

exception InputValue of string
