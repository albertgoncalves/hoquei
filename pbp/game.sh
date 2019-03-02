#!/usr/bin/env bash

cat game.json | jq '.liveData | .plays | .allPlays' | less
