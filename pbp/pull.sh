#!/usr/bin/env bash

set -e

directory="data"
team_id="22"
game_id="2018020875"

curl "https://statsapi.web.nhl.com/api/v1/schedule?teamId=$team_id&startDate=2018-10-01&endDate=2019-10-01" \
    > $directory/schedule.json
sleep 1
curl "http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=$game_id" \
    > $directory/shifts.json
sleep 1
curl "https://statsapi.web.nhl.com/api/v1/game/$game_id/feed/live?site=en_nhl" \
    > $directory/game.json
