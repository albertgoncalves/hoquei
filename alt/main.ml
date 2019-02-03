module B = BatString
module E = BatEnum
module F = BatFile
module L = List
module S = String
module R = Str

let (@.) (f : ('b -> 'c)) (g : ('a -> 'b)) : ('a -> 'c) =
    fun x -> f @@ g x

let scalpel (l : string) : string list =
    let rec loop (switch : bool) (accu : string list)
        : ((char list * char list) -> string list) = function
        | ys, [] -> accu
        | ys, ('>'::xs) -> loop true accu (ys, xs)
        | [], ('<'::xs) -> loop false accu ([], xs)
        | ys, ('<'::xs) ->
            let y = B.of_list @@ L.rev ys in
            loop false (y::accu) ([], xs)
        | ys, (x::xs) ->
            if switch then
                loop switch accu ((x::ys), xs)
            else
                loop switch accu (ys, xs) in
    loop false [] ([], (B.to_list l))

let html : string E.t = F.lines_of "tmp.html"

let pattern : R.regexp = R.regexp "<tr .*"

let rows (l : string) : string option =
    if R.string_match pattern l 0 then Some l else None

let main () =
    E.iter print_endline
    @@ E.map ((S.concat " ") @. scalpel)
    @@ E.filter_map rows html

let () = main ()
