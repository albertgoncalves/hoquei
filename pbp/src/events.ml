module C = Csv
module L = List
module J = Json
module P = Printf
module S = String
module T = Utils
module Y = Yojson.Basic
module U = Y.Util

type coordinates =
    { x : float
    ; y : float
    }

type player =
    { id : int
    ; name : string
    ; result : string
    }

type team =
    { id : int
    ; name : string
    }

type score =
    { away : int
    ; home : int
    }

type event =
    { result : string
    ; period : int
    ; second : int
    ; team : team option
    ; players : (player list) option
    ; score : score option
    ; coordinates : coordinates option
    }

let extract_team (json : Y.json) : team =
    let team_json = json |> U.member "team" in
    { id = team_json |> U.member "id" |> U.to_int
    ; name = team_json |> U.member "name" |> U.to_string
    }

let extract_player (json : Y.json) : player =
    let player_json = json |> U.member "player" in
    { id = player_json |> U.member "id" |> U.to_int
    ; name = player_json |> U.member "fullName" |> U.to_string
    ; result = json |> U.member "playerType" |> U.to_string
    }

let extract_coordinates (json : Y.json) : coordinates =
    let coordinates : Y.json = json |> U.member "coordinates" in
    { x = coordinates |> U.member "x" |> U.to_float
    ; y = coordinates |> U.member "y" |> U.to_float
    }

let extract_score (json : Y.json) : score =
    let score = json |> U.member "goals" in
    { away = score |> U.member "away" |> U.to_int
    ; home = score |> U.member "home" |> U.to_int
    }

let extract (json : Y.json) : event =
    let about = json |> U.member "about" in
    let result =
        json |> U.member "result" |> U.member "event" |> U.to_string in
    let period = about |> U.member "period" |> U.to_int in
    let second =
        about
        |> U.member "periodTime"
        |> U.to_string_option
        |> J.clock_to_seconds in
    let score = T.try_option (fun () -> extract_score about) in
    let team = T.try_option (fun () -> extract_team json) in
    let players =
        T.try_option
            begin
                fun () ->
                    json
                    |> U.member "players"
                    |> U.to_list
                    |> L.map extract_player

            end in
    let coordinates = T.try_option (fun () -> extract_coordinates json) in
    { result = result
    ; period = period
    ; second = second
    ; team = team
    ; players = players
    ; score = score
    ; coordinates = coordinates
    }

let players_to_string (players : player list) : string =
    let to_string (player : player) : string =
        P.sprintf
            "{%s}"
            begin
                S.concat
                    ","
                    [ P.sprintf "\"id\": %d" player.id
                    ; P.sprintf "\"name\": \"%s\"" player.name
                    ; P.sprintf "\"result\": \"%s\"" player.result
                    ]
            end in
    let players = L.map to_string players in
    P.sprintf "[%s]" (S.concat "," players)

let csv_header : string =
    C.concat
        [ "game_id"
        ; "period"
        ; "second"
        ; "result"
        ; "away_goals"
        ; "home_goals"
        ; "x"
        ; "y"
        ; "team_id"
        ; "team_name"
        ; "players"
        ]

let csv_row (game_id : int) (event : event) : string =
    C.concat
        [ game_id |> string_of_int
        ; event.period |> string_of_int
        ; event.second |> string_of_int
        ; event.result
        ; T.option_to_string (fun a -> a.away |> string_of_int) event.score
        ; T.option_to_string (fun a -> a.home |> string_of_int) event.score
        ; T.option_to_string
              (fun a -> a.x |> string_of_float)
              event.coordinates
        ; T.option_to_string
              (fun a -> a.y |> string_of_float)
              event.coordinates
        ; T.option_to_string (fun a -> a.id |> string_of_int) event.team
        ; T.option_to_string (fun a -> a.name) event.team
        ; T.option_to_string players_to_string event.players
        ]
