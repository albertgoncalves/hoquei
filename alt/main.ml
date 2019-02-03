module E = ExtString.String
module L = List
module S = String
module R = Str
module Y = Sys
module X = Std

let (@.) (f : ('b -> 'c)) (g : ('a -> 'b)) : ('a -> 'c) = fun x -> f @@ g x

let finally (f: unit -> 'a) (resolve: unit -> 'b) =
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

let html : string list = with_file Y.argv.(1) X.input_list

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
