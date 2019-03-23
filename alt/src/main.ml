module L = List
module S = Scrape
module U = Utils

let (|.) = U.(|.)

let html : string list = U.with_file Sys.argv.(1) Std.input_list

let main () : unit =
    html
    |> L.filter S.row
    |> L.iter begin
        S.scalpel
        |. String.concat " "
        |. print_endline
    end

let () = main ()
