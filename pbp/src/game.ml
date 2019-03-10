module L = List
module T = Utils
module Y = Yojson.Basic
module U = Y.Util
module P = Players
module E = Events

let (@.) = T.(@.)

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
        |> L.map ((fun event -> E.csv_row game_id event) @. E.extract) in
    let player_rows =
        live_data
        |> P.all_players
        |> P.extract_all
        |> L.map (fun player -> P.csv_row game_id player) in
    let (^^) filename rows = T.write_to_file filename rows in
    L.iter
        T.apply
        [ (fun () -> events_file ^^ (E.csv_header::event_rows))
        ; (fun () -> players_file ^^ (P.csv_header::player_rows))
        ]

let () = main ()
