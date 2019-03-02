module L = List
module P = Printf
module S = String
module Y = Yojson.Basic
module U = Y.Util

exception InputValue of string

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
                    let error = P.sprintf "unable to convert '%s'" time in
                    raise (InputValue error)

let rec last : ('a list -> 'a) = function
    | [] -> raise (InputValue "empty list")
    | [x] -> x
    | (_::xs) -> last xs

let demo (json : Y.json) (field : string) : int =
    json
    |> U.member field
    |> U.to_string_option
    |> clock_to_seconds

let main () =
    let filename = "shifts.json" in
    let json = Y.from_file filename in
    let data =
        json
        |> U.member "data"
        |> U.to_list
        |> last in

    data
    |> Y.pretty_to_string
    |> print_endline;

    L.iter
        begin
            fun field ->
                demo data field
                |> string_of_int
                |> print_endline
        end
        ["startTime"; "duration"; "endTime"]

let () = main ()
