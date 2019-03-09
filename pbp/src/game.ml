module C = Csv
module L = List
module J = Json
module P = Printf
module S = String
module T = Utils
module Y = Yojson.Basic
module U = Y.Util

let (@.) = T.(@.)

type player =
    { team_id : int
    ; team_name : string
    ; player_id : int
    ; full_name : string
    ; handedness : string
    ; position : string
    }

type coordinates =
    { x : float
    ; y : float
    }

type event_player =
    { id : int
    ; name : string
    ; result : string
    }

type event_team =
    { id : int
    ; name : string
    }

type event_score =
    { away : int
    ; home : int
    }

type event =
    { result : string
    ; period : int
    ; second : int
    ; team : event_team option
    ; players : (event_player list) option
    ; score : event_score option
    ; coordinates : coordinates option
    }

let extract_event_team (json : Y.json) : event_team =
    let team_json = json |> U.member "team" in
    { id = team_json |> U.member "id" |> U.to_int
    ; name = team_json |> U.member "name" |> U.to_string
    }

let extract_event_player (json : Y.json) : event_player =
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

let extract_score (json : Y.json) : event_score =
    let score = json |> U.member "goals" in
    { away = score |> U.member "away" |> U.to_int
    ; home = score |> U.member "home" |> U.to_int
    }

let extract_event (json : Y.json) : event =
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
    let team = T.try_option (fun () -> extract_event_team json) in
    let players =
        T.try_option
            begin
                fun () ->
                    json
                    |> U.member "players"
                    |> U.to_list
                    |> L.map extract_event_player

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

let all_players : (Y.json -> Y.json) = U.member "teams" @. U.member "boxscore"

let extract_player (team_id : int) (team_name : string) (players : Y.json)
        (player_id : string) =
    let player_json = players |> U.member player_id in
    let person = player_json |> U.member "person" in
    let position =
        player_json
        |> U.member "position"
        |> U.member "abbreviation"
        |> U.to_string in
    { team_id = team_id
    ; team_name = team_name
    ; player_id = person |> U.member "id" |> U.to_int
    ; full_name = person |> U.member "fullName" |> U.to_string
    ; handedness = person |> U.member "shootsCatches" |> U.to_string
    ; position = position
    }

let unpack_players (json : Y.json) : player list =
    let team_players json team =
        let json = json |> U.member team in
        let team_json = json |> U.member "team" in
        let team_id = team_json |> U.member "id" |> U.to_int in
        let team_name = team_json |> U.member "name" |> U.to_string in
        let players = json |> U.member "players" in
        let player_ids = players |> U.keys in
        L.map (extract_player team_id team_name players) player_ids in
    L.map (team_players json) ["away"; "home"] |> L.flatten

let player_header : string =
    C.concat
        [ "game_id"
        ; "team_id"
        ; "team_name"
        ; "player_id"
        ; "full_name"
        ; "handedness"
        ; "position"
        ]

let player_row (game_id : int) (player : player) : string =
    C.concat
        [ game_id |> string_of_int
        ; player.team_id |> string_of_int
        ; player.team_name
        ; player.player_id |> string_of_int
        ; player.full_name
        ; player.handedness
        ; player.position
        ]

let event_players_to_string (players : event_player list) : string =
    let to_string (player : event_player) : string =
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

let option_to_string (to_string : 'a -> string) : ('a option -> string) =
    function
        | None -> ""
        | Some x -> to_string x

let event_header : string =
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

let event_row (game_id : int) (event : event) : string =
    C.concat
        [ game_id |> string_of_int
        ; event.period |> string_of_int
        ; event.second |> string_of_int
        ; event.result
        ; option_to_string (fun a -> a.away |> string_of_int) event.score
        ; option_to_string (fun a -> a.home |> string_of_int) event.score
        ; option_to_string (fun a -> a.x |> string_of_float) event.coordinates
        ; option_to_string (fun a -> a.y |> string_of_float) event.coordinates
        ; option_to_string (fun a -> a.id |> string_of_int) event.team
        ; option_to_string (fun a -> a.name) event.team
        ; option_to_string event_players_to_string event.players
        ]

let main () =
    let input_file = Sys.argv.(1) in
    let events_file = Sys.argv.(2) in
    let players_file = Sys.argv.(3) in
    let json = Y.from_file input_file in
    let game_id =
        json
        |> U.member "gameData"
        |> U.member "game"
        |> U.member "pk"
        |> U.to_int in
    let live_data = json |> U.member "liveData" in
    let event_rows =
        live_data
        |> U.member "plays"
        |> U.member "allPlays"
        |> U.to_list
        |> L.map ((fun event -> event_row game_id event) @. extract_event) in
    let player_rows =
        live_data
        |> all_players
        |> unpack_players
        |> L.map (fun player -> player_row game_id player) in
    let (^^) filename rows = T.write_to_file filename rows in
    L.iter
        T.apply
        [ (fun () -> events_file ^^ (event_header::event_rows))
        ; (fun () -> players_file ^^ (player_header::player_rows))
        ]

let () = main ()
