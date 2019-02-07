type result =
    { away : int
    ; home : int
    ; ot : string option
    }

type game =
    { date : string
    ; away : string
    ; home : string
    ; result : result option
    }

type split_games =
    { past_games : game list
    ; future_games : game list
    }

exception InputValue of string
