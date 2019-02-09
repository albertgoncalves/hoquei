module L = List
module S = String
module U = Utils

let scalpel (l : string) : string list =
    let p = Str.regexp ">\\([a-zA-Z0-9 :,-]+?\\)<" in
    let forward p l n : (int * string) option =
        let lambda () =
            let n = Str.search_forward p l n in
            let s = Str.matched_group 1 l in
            Some (n, s) in
        try lambda () with Not_found -> None in
    let rec loop p l accu : ((int * string) option -> string list) = function
        | Some (n, s) ->
            let maybe_accu = function
                | "" -> accu
                | s -> (s::accu) in
            loop p l (maybe_accu s) (forward p l (n + 1))
        | None ->
            accu in
    loop p l [] (forward p l 0)

let row : (string -> bool) =
    let p = Str.regexp "^<tr[ ]*><th scope=\"row\"" in
    function
        | "" -> false
        | x -> Str.string_match p x 0
