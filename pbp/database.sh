#!/usr/bin/env bash

set -e

alias csvlook="csvlook --no-inference -d ';'"
alias sqlite3="sqlite3 -header -csv -separator ';'"

db="database.db"

create() {
    sqlite3 $db "CREATE TABLE $1;"
}

insert() {
    sqlite3 $db "INSERT INTO $1 $2;"
}

players_header="
    players( id INTEGER PRIMARY KEY
           , game_id INTEGER
           , team_id INTEGER
           , team_name TEXT
           , player_id INTEGER
           , full_name TEXT
           , handedness TEXT
           , position TEXT
           )
"

players_row="
    players( game_id
           , team_id
           , team_name
           , player_id
           , full_name
           , handedness
           , position
           )
"

shifts_header="
    shifts( id INTEGER PRIMARY KEY
          , game_id INTEGER
          , team_id INTEGER
          , team_name TEXT
          , player_id INTEGER
          , first_name TEXT
          , last_name TEXT
          , second INTEGER
          )
"

shifts_row="
    shifts( game_id
          , team_id
          , team_name
          , player_id
          , first_name
          , last_name
          , second
          )
"

if [ ! -f ./$db ]; then
    create "$players_header"
    insert \
        "$players_row" \
        "VALUES(1, 2, 'Montreal', 3, 'PK Subban', 'Right', 'D')"
    create "$shifts_header"
    insert \
        "$shifts_row" \
        "VALUES(1, 2, 'Montreal', 3, 'PK', 'Subban', 4)"
fi

query="
    SELECT p.full_name
        , s.first_name
        , s.last_name
    FROM players p
        INNER JOIN shifts s
            ON s.team_name = p.team_name;
"

sqlite3 $db "$query" | csvlook
