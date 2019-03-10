#!/usr/bin/env bash

cat ../data/game.json | jq '.gameData' | less
