module L = List
module P = Printf

exception Value of string

(*
let (@.) (f : 'b -> 'c) (g : 'a -> 'b) : ('a -> 'c) = fun x -> f (g x)
*)
let (|.) (f : 'a -> 'b) (g : 'b -> 'c) : ('a -> 'c) = fun x -> g (f x)

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
    let f_exception : 'a =
        try f () with error ->
            resolve ();
            raise error in
    resolve ();
    f_exception

let apply (f : unit -> unit) : unit = f ()

let write_to_file (filename : string) (strings : string list) : unit =
    let out_channel : out_channel = open_out filename in
    finally
        (fun () -> L.iter (P.fprintf out_channel "%s\n") strings)
        (fun () -> close_out out_channel)

let print_option : (string option -> unit) = function
    | Some a -> print_endline a
    | None -> ()

let try_option (f : unit -> 'a) : 'a option =
    try
        Some (f ())
    with _ ->
        None

let option_to_string (to_string : 'a -> string) : ('a option -> string) =
    function
        | None -> ""
        | Some x -> to_string x
