module E = ExtString.String
module L = List
module S = String
module R = Str
module Y = Sys
module X = Std

let (@.) (f : ('b -> 'c)) (g : ('a -> 'b)) : ('a -> 'c) = fun x -> f @@ g x

let scalpel (l : string) : string list =
    let rec loop (target : bool) (accu : string list)
        : ((char list * char list) -> string list) = function
        | ys, [] -> accu
        | ys, '>'::xs -> loop true accu (ys, xs)
        | [], '<'::xs -> loop false accu ([], xs)
        | ys, '<'::xs ->
            let y = ys |> L.rev |> E.implode in
            loop false (y::accu) ([], xs)
        | ys, x::xs ->
            if target then
                loop target accu (x::ys, xs)
            else
                loop target accu (ys, xs) in
    loop false [] ([], E.explode l)

let pattern : R.regexp = R.regexp "<tr .*"

let rows (l : string) : string option =
    if R.string_match pattern l 0 then
        Some l
    else
        None

let html : string list =
    let channel = open_in Y.argv.(1) in
    let l = X.input_list channel in
    close_in channel;
    l

let sift (l : 'a option list) : 'a list =
    let rec loop accu = function
        | [] -> accu
        | (Some x)::xs -> loop (x::accu) xs
        | None::xs -> loop accu xs in
    loop [] l

let main () =
    L.map rows html
    |> sift
    |> L.map @@ (S.concat " ") @. scalpel
    |> L.iter print_endline

let () = main ()
