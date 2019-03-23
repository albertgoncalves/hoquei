module C = Csv
module L = List
module T = Utils
module Y = Yojson.Basic
module U = Y.Util
module P = Players
module E = Events

let (|.) = T.(|.)

type game =
    { season : int
    ; season_type : string
    ; team_id : int
    ; team_name : string
    ; venue : string
    }

let extract_team (season : int) (season_type : string) (venue : string)
        (json : Y.json) : game =
    let team : Y.json = json |> U.member venue in
    { season = season
    ; season_type = season_type
    ; team_id = team |> U.member "id" |> U.to_int
    ; team_name = team |> U.member "name" |> U.to_string
    ; venue = venue
    }

let extract (json : Y.json) : game list =
    let game : Y.json = json |> U.member "game" in
    let season : int =
        game |> U.member "season" |> U.to_string |> int_of_string in
    let season_type : string = game |> U.member "type" |> U.to_string in
    let teams : Y.json = json |> U.member "teams" in
    L.map
        (fun venue -> extract_team season season_type venue teams)
        ["away"; "home"]

let csv_header : string =
    C.concat
        [ "game_id"
        ; "season"
        ; "season_type"
        ; "team_id"
        ; "team_name"
        ; "venue"
        ]

let csv_row (game_id : int) (game : game) : string =
    C.concat
        [ game_id |> string_of_int
        ; game.season |> string_of_int
        ; game.season_type
        ; game.team_id |> string_of_int
        ; game.team_name
        ; game.venue
        ]

let main () =
    let input_file = Sys.argv.(1) in
    let game_file = Sys.argv.(2) in
    let events_file = Sys.argv.(3) in
    let players_file = Sys.argv.(4) in
    let json = Y.from_file input_file in
    let game_data = json |> U.member "gameData" in
    let game_id = game_data |> U.member "game" |> U.member "pk" |> U.to_int in
    let live_data = json |> U.member "liveData" in
    let game_rows =
        game_data
        |> extract
        |> L.map (fun game -> csv_row game_id game) in
    let event_rows =
        live_data
        |> U.member "plays"
        |> U.member "allPlays"
        |> U.to_list
        |> L.map begin
            E.extract
            |. E.csv_row game_id
        end in
    let player_rows =
        live_data
        |> P.all_players
        |> P.extract_all
        |> L.map (fun player -> P.csv_row game_id player) in
    let (^^) filename rows = T.write_to_file filename rows in
    L.iter
        T.apply
        [ (fun () -> game_file ^^ (csv_header::game_rows))
        ; (fun () -> events_file ^^ (E.csv_header::event_rows))
        ; (fun () -> players_file ^^ (P.csv_header::player_rows))
        ]

let () = main ()
