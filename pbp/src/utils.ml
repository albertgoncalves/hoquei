module L = List
module P = Printf

exception Value of string

let rec last : ('a list -> 'a) = function
    | [] -> raise (Value "empty list")
    | [x] -> x
    | (_::xs) -> last xs

let take (n : int) (xs : 'a list) : 'a list =
    let rec loop (accu : 'a list) : (('a list * int) -> 'a list) = function
        | ([], _) | (_, 0) -> accu
        | ((x::xs), n) -> loop (x::accu) (xs, n - 1) in
    loop [] (xs, n)

let finally (f : unit -> 'a) (resolve : unit -> 'b) : 'a =
    let f_exception =
        try f () with error ->
            resolve ();
            raise error in
    resolve ();
    f_exception

let write_to_file (filename : string) (strings : string list) : unit =
    let out_channel = open_out filename in
    finally
        (fun () -> L.iter (P.fprintf out_channel "%s\n") strings)
        (fun () -> close_out out_channel)
