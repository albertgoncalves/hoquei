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
