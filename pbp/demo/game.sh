#!/usr/bin/env bash

cat ../data/game.json | jq '.liveData | .plays | .allPlays' | less
