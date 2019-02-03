module E = ExtString.String
module L = List
module S = String
module U = Utils

let (@.) = U.(@.)

(* NOTE: This seems to work -- but, is there a better way to do this? *)
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
            let ys =
                if target then
                    x::ys
                else
                    ys in
            loop target accu (ys, xs) in
    loop false [] ([], E.explode l)

let rows (l : string) : string option =
    let pattern = Str.regexp "<tr .*" in
    if Str.string_match pattern l 0 then
        Some l
    else
        None

let sift (l : 'a option list) : 'a list =
    let rec loop (accu : 'a list) : ('a option list -> 'a list) = function
        | [] -> accu
        | (Some x)::xs -> loop (x::accu) xs
        | None::xs -> loop accu xs in
    loop [] l

let html : string list = U.with_file Sys.argv.(1) Std.input_list

let main () : unit =
    L.map rows html
    |> sift
    |> L.map scalpel
    |> L.iter (print_endline @. S.concat " ")

let () = main ()
