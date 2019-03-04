#!/usr/bin/env bash

cat ../data/game.json \
    | jq '.liveData | .boxscore | .teams | .away | .players' \
    | less
