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

let rows : (string list -> string list list) =
    let pattern = Str.regexp "<tr .*" in
    let rec loop (accu : string list list)
        : (string list -> string list list) = function
        | [] -> accu
        | x::xs ->
            if Str.string_match pattern x 0 then
                loop ((scalpel x)::accu) xs
            else
                loop accu xs in
    loop []

let html : string list = U.with_file Sys.argv.(1) Std.input_list

let main () : unit =
    html
    |> rows
    |> L.iter (print_endline @. S.concat " ")

let () = main ()
