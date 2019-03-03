#!/usr/bin/env bash

cat ../data/shifts.json | jq '.data' | less
