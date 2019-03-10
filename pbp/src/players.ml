module C = Csv
module L = List
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

let all_players : (Y.json -> Y.json) = U.member "teams" @. U.member "boxscore"

let extract (team_id : int) (team_name : string) (players : Y.json)
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

let extract_all (json : Y.json) : player list =
    let team_players json team =
        let json = json |> U.member team in
        let team_json = json |> U.member "team" in
        let team_id = team_json |> U.member "id" |> U.to_int in
        let team_name = team_json |> U.member "name" |> U.to_string in
        let players = json |> U.member "players" in
        let player_ids = players |> U.keys in
        L.map (extract team_id team_name players) player_ids in
    L.map (team_players json) ["away"; "home"] |> L.flatten

let csv_header : string =
    C.concat
        [ "game_id"
        ; "team_id"
        ; "team_name"
        ; "player_id"
        ; "full_name"
        ; "handedness"
        ; "position"
        ]

let csv_row (game_id : int) (player : player) : string =
    C.concat
        [ game_id |> string_of_int
        ; player.team_id |> string_of_int
        ; player.team_name
        ; player.player_id |> string_of_int
        ; player.full_name
        ; player.handedness
        ; player.position
        ]
