module C = Csv
module L = List
module J = Json
module S = String
module T = Utils
module Y = Yojson.Basic
module U = Y.Util

type id =
    { game_id : int
    ; team_name : string
    ; player_id : int
    ; first_name : string
    ; last_name : string
    }

type shift =
    { id : id
    ; period : int
    ; end_time : int
    ; duration : int
    }

type shift_slice =
    { id : id
    ; second : int
    }

let extract_shift (json : Y.json) : shift option =
    match json |> U.member "eventDescription" |> U.to_string_option with
        | Some event -> None
        | None ->
            let access (field : string) : Y.json = json |> U.member field in
            let to_seconds (time : Y.json) : int =
                time |> U.to_string_option |> J.clock_to_seconds in
            Some
                { id =
                      { game_id = access "gameId" |> U.to_int
                      ; team_name = access "teamName" |> U.to_string
                      ; player_id = access "playerId" |> U.to_int
                      ; first_name = access "firstName" |> U.to_string
                      ; last_name = access "lastName" |> U.to_string
                      }
                ; period = access "period" |> U.to_int
                ; end_time = access "endTime" |> to_seconds
                ; duration = access "duration" |> to_seconds
                }

let shift_to_slices (shift : shift option) : shift_slice list =
    match shift with
        | None -> []
        | Some shift ->
            if shift.period > 3 then
                []
            else
                let rec loop id end_time offset xs = function
                    | 0 -> xs
                    | s ->
                        let x =
                            { id = id
                            ; second = offset + end_time - s + 1
                            } in
                        loop id end_time offset (x::xs) (s - 1) in
                let offset = (shift.period - 1) * 20 * 60 in
                loop shift.id shift.end_time offset [] shift.duration

let csv_header : string =
    C.csv_concat
        [ "game_id"
        ; "team_name"
        ; "player_id"
        ; "first_name"
        ; "last_name"
        ; "second"
        ]

let csv_row (slice : shift_slice) : string =
    C.csv_concat
        [ slice.id.game_id |> string_of_int
        ; slice.id.team_name
        ; slice.id.player_id |> string_of_int
        ; slice.id.first_name
        ; slice.id.last_name
        ; slice.second |> string_of_int
        ]

let main () =
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in
    let json = Y.from_file input_file in
    let rows =
        json
        |> U.member "data"
        |> U.to_list
        |> L.map extract_shift
        |> L.map shift_to_slices
        |> L.flatten
        |> L.map csv_row in
    T.write_to_file output_file (csv_header::rows)

let () = main ()
