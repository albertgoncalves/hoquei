module B = BatString
module E = BatEnum
module F = BatFile
module L = List
module S = String
module R = Str

let (@.) (f : ('b -> 'c)) (g : ('a -> 'b)) : ('a -> 'c) = fun x -> f @@ g x

let scalpel (l : string) : string list =
    let rec loop (target : bool) (accu : string list)
        : ((char list * char list) -> string list) = function
        | ys, [] -> accu
        | ys, '>'::xs -> loop true accu (ys, xs)
        | [], '<'::xs -> loop false accu ([], xs)
        | ys, '<'::xs ->
            let y = ys |> L.rev |> B.of_list in
            loop false (y::accu) ([], xs)
        | ys, x::xs ->
            if target then
                loop target accu (x::ys, xs)
            else
                loop target accu (ys, xs) in
    loop false [] ([], B.to_list l)

let pattern : R.regexp = R.regexp "<tr .*"

let rows (l : string) : string option =
    if R.string_match pattern l 0 then
        Some l
    else
        None

let html : string E.t = F.lines_of "tmp.html"

let main () =
    E.filter_map rows html
    |> E.map @@ (S.concat " ") @. scalpel
    |> E.iter print_endline

let () = main ()
