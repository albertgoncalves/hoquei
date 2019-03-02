module L = List
module P = Printf
module S = String
module Y = Yojson.Basic
module U = Y.Util

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

let main () =
    let filename = "shifts.json" in
    let json = Y.from_file filename in
    let data = json |> U.member "data" |> U.to_list |> last in
    let print_blob () : unit = data |> Y.pretty_to_string |> print_endline in
    let print_clocks () : unit =
        L.iter
            begin
                fun field ->
                    data
                    |> U.member field
                    |> U.to_string_option
                    |> clock_to_seconds
                    |> string_of_int
                    |> print_endline
            end
            ["startTime"; "duration"; "endTime"] in
    L.iter (fun f -> f ()) [print_blob; print_clocks]

let () = main ()
