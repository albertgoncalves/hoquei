module L = List
module P = Printf
module S = String
module Y = Yojson.Basic
module U = Y.Util

type shift =
    { first_name : string
    ; last_name : string
    ; game_id : int
    ; team_name : string
    ; end_time : int
    ; duration : int
    }

exception Value of string

let clock_to_seconds : (string option -> int) =
    let convert (minutes : string) (seconds : string) : int =
        let minutes = int_of_string minutes in
        let seconds = int_of_string seconds in
        (minutes * 60) + seconds in
    function
        | None -> 0
        | Some time ->
            match S.split_on_char ':' time with
                | [minutes; seconds] -> convert minutes seconds
                | _ ->
                    let error = P.sprintf "unable to parse '%s'" time in
                    raise (Value error)

let rec last : ('a list -> 'a) = function
    | [] -> raise (Value "empty list")
    | [x] -> x
    | (_::xs) -> last xs

let extract (json : Y.json) : shift option =
    match json |> U.member "eventDescription" |> U.to_string_option with
        | Some event -> None
        | None ->
            let access (field : string) : Y.json = json |> U.member field in
            let to_seconds (time : Y.json) : int =
                time |> U.to_string_option |> clock_to_seconds in
            Some
                { first_name = access "firstName" |> U.to_string
                ; last_name = access "lastName" |> U.to_string
                ; game_id = access "gameId" |> U.to_int
                ; team_name = access "teamName" |> U.to_string
                ; end_time = access "endTime" |> to_seconds
                ; duration = access "duration" |> to_seconds
                }

let csv_concat : (string list -> string) = S.concat ";"

let csv_header : string =
    csv_concat
        [ "first_name"
        ; "last_name"
        ; "game_id"
        ; "team_name"
        ; "end_time"
        ; "duration"
        ]

let csv_row (shift : shift) : string =
    csv_concat
        [ shift.first_name
        ; shift.last_name
        ; shift.game_id |> string_of_int
        ; shift.team_name
        ; shift.end_time |> string_of_int
        ; shift.duration |> string_of_int
        ]

let take (n : int) (xs : 'a list) : 'a list =
    let rec loop (accu : 'a list) : (('a list * int) -> 'a list) = function
        | ([], _) | (_, 0) -> accu
        | ((x::xs), n) -> loop (x::accu) (xs, n - 1) in
    loop [] (xs, n)

let main () =
    let filename = "data/shifts.json" in
    let json = Y.from_file filename in
    let blobs = json |> U.member "data" |> U.to_list |> take 5 in
    let print_blob () : unit =
        L.iter
            (fun blob -> blob |> Y.pretty_to_string |> print_endline)
            blobs in
    let print_header () : unit = print_endline csv_header in
    let print_row () : unit =
        let print_row_opt : (shift option -> unit) = function
            | None -> ()
            | Some shift -> shift |> csv_row |> print_endline in
        L.iter (fun blob -> blob |> extract |> print_row_opt) blobs in
    L.iter (fun f -> f ()) [print_blob; print_header; print_row]

let () = main ()
