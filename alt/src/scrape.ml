module L = List
module S = String
module U = Utils

(* NOTE: This seems to work -- but, is there a better way to do this? *)
let scalpel (l : string) : string list =
    let rec loop (target : bool) (accu : string list)
        : ((char list * char list) -> string list) = function
        | ys, [] -> accu
        | ys, '>'::xs -> loop true accu (ys, xs)
        | [], '<'::xs -> loop false accu ([], xs)
        | ys, '<'::xs ->
            let y = U.rev_implode ys in
            loop false (y::accu) ([], xs)
        | ys, x::xs ->
            let ys =
                if target then
                    x::ys
                else
                    ys in
            loop target accu (ys, xs) in
    loop false [] ([], U.explode l)

let pattern = Str.regexp "^<tr[ ]*><th scope=\"row\""

let row (pattern : Str.regexp) : (string -> bool) = function
    | "" -> false
    | x -> Str.string_match pattern x 0
