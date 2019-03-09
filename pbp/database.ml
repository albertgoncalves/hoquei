module P = Printf
module S = Sys

type player =
    { team_id : int
    ; team_name : string
    ; player_id : int
    ; full_name : string
    ; handedness : string
    ; position : string
    }

exception ExitWith of int

let execute (values : string) : unit =
    let insert =
        format_of_string "sqlite3 database.db \"INSERT INTO %s;\"" in
    let error message code =
        print_endline message;
        raise (ExitWith code) in
    let expression = P.sprintf insert values in
    let code = S.command expression in
    if code = 0 then
        ()
    else
        error expression code

let insert_player (game_id : int) (player : player) : unit =
    let sql =
        format_of_string "
            players( game_id
                   , team_id
                   , team_name
                   , player_id
                   , full_name
                   , handedness
                   , position
                   )
            VALUES(%d, %d, '%s', %d, '%s', '%s', '%s')
        " in
    let values =
        P.sprintf
            sql
            game_id
            player.team_id
            player.team_name
            player.player_id
            player.full_name
            player.handedness
            player.position in
    execute values

let main () =
    let player =
        { team_id = 200
        ; team_name = "Montreal"
        ; player_id = 300
        ; full_name = "Jesperi Something"
        ; handedness = "Left?"
        ; position = "C"
        } in
    let game_id = 1000 in
    insert_player game_id player

let () = main ()
