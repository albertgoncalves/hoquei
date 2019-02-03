module C = Char
module L = List
module S = String

let (@.) (f : ('b -> 'c)) (g : ('a -> 'b)) : ('a -> 'c) = fun x -> f @@ g x

let finally (f: unit -> 'a) (resolve: unit -> 'b) : 'a =
    let f_exception =
        try f () with
              err ->
                resolve ();
                raise err in
    resolve ();
    f_exception

let with_file (path : string) (f: in_channel -> 'a) : 'a =
    let channel = open_in path in
    finally
        (fun () -> f channel)
        (fun () -> close_in channel)

let explode (s : string) : char list =
    let n = S.length s in
    L.init n @@ S.get s

let rev_implode : (char list -> string) =
    let rec loop (accu : string list) : (char list -> string list) = function
        | [] -> accu
        | x::xs ->
            let y = C.escaped x in
            loop (y::accu) xs in
    S.concat "" @. loop []
