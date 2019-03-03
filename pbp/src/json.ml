module S = String
module P = Printf
module T = Utils

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
                    raise (T.Value error)
