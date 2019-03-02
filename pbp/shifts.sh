#!/usr/bin/env bash

cat shifts.json | jq '.data' | less
