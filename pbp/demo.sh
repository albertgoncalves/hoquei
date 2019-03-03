#!/usr/bin/env bash

sh main.sh src/shifts | tail -n 6 | csvlook --no-inference -d ';'
